{***********************************************************************}
{                          омпонент KOL XPMenu                          }
{ ќписание:                                                             }
{   * ѕринимает на себ€ отрисовку меню в стиле OfficeXP.                }
{                                                                       }
{ ¬ерси€  : 1.09 от 22.10.2005                                          }
{ јвтор   : Ўир€ев јлександр                                            }
{ E-mail  : alex@diploms.com                                            }
{ ¬ыполнен на основе класса RyMenu јлексе€ –ум€нцева (skitl@mail.ru).   }
{***********************************************************************}
{$DEFINE USE_AUTOFREE4CONTROLS}

unit XPMenus;

interface

uses
  Windows, Types, Messages, KOL;

type
  PXPControl=^TXPControl;
  TXPControl=object(TControl)
  end;

  PXPMenu = ^TXPMenu;
  TXPMenu = object(TMenu)
  private
    FFont: PGraphicTool;
    FGutterColor: TColor;
    FBackColor: TColor;
    FSelectedColor: TColor;
    FSelLightColor: TColor;
    FCheckColor: TColor;
    FMinWidth: Integer;
    FMinHeight: Integer;
    FIsPopup  : boolean;
    FBmpCheck: PBitmap;

    procedure SetFont(Value: PGraphicTool);
    procedure SetSelectedColor(const Value: TColor);
    procedure InternalInitItems(Item: PMenu);
    function  TextExtent(const Text: string): TSize;
    procedure InitCheckBmp;
  protected
  public
//    destructor Destroy; {-}virtual;{+}{++}(*override;*){--}
    procedure DrawXPstyle;
    function  MeasureItem(Sender: PObj;  Idx: Integer): Integer;
    function  DrawItem(Sender: PObj; DC: HDC; const Rect: TRect;
     ItemIdx: Integer; DrawAction: TDrawAction; ItemState: TDrawState): Boolean;

  public
    property  GutterColor: TColor read FGutterColor write FGutterColor;
    property  BackColor: TColor read FBackColor write FBackColor;
    property  SelectedColor: TColor read FSelectedColor write SetSelectedColor;
    property  CheckColor: TColor read FCheckColor write FCheckColor;
    property  Font: PGraphicTool read FFont write SetFont; {можете помен€ть фонт у меню}
    property  ItemHeight: Integer read FMinHeight write FMinHeight;
    property  ItemWidth: Integer read FMinWidth write FMinWidth;
  end;
  TKOLXPMainMenu= PXPMenu;
  TKOLXPPopupMenu= PXPMenu;

{procedure XPDrawItem(Sender: PObj; DC: HDC; ARect: TRect;
          ItemState: TDrawState; TopLevel, IsLine: Boolean;
          Bitmap:HBitmap;BitmapSize: tagBitmap; AFont: PGraphicTool;
          const Caption: String; GutterWidth: Integer;
          SelectedColor, GutterColor, MenuColor, SelLightColor, CheckColor: TColor);
 }
function  NewXPMenu( AParent : PControl; MaxCmdReserve: DWORD; const Template : array of PKOLChar;
                      aOnMenuItem: TOnMenuItem; isPopup:boolean ): PXPMenu;

implementation

type
  TRGB = packed record
    R, G, B: Byte;
  end;

  AGRBQuad = array [0..0] of RGBQuad;
  PAGRBQuad = ^AGRBQuad;

  PPoints = ^TPoints;
  TPoints = array[0..0] of TPoint;

//–аскладывает колор на составные части
function GetRGB(const Color: TColor): TRGB;
var
  iColor: TColor;
begin
  iColor := Color2RGB(Color);
  Result.R := GetRValue(iColor);
  Result.G := GetGValue(iColor);
  Result.B := GetBValue(iColor);
end;

//получаем бледный цвет
function GetLightColor(Color: TColor; Light: Byte) : TColor;
var
  fFrom: TRGB;
begin
  FFrom := GetRGB(Color);

  Result := RGB(
    Round(FFrom.R + (255 - FFrom.R) * (Light / 100)),
    Round(FFrom.G + (255 - FFrom.G) * (Light / 100)),
    Round(FFrom.B + (255 - FFrom.B) * (Light / 100))
  );
end;

function  GetShadeColor(Color: TColor; Shade: Byte) : TColor;
var
  fFrom: TRGB;
begin
  FFrom := GetRGB(Color);

  Result := RGB(
    Max(0, FFrom.R - Shade),
    Max(0, FFrom.G - Shade),
    Max(0, FFrom.B - Shade)
  );
end;

function BtnHighlight : TColor;
begin
  Result := GetLightColor(clBtnFace, 50)
end;

function NewXPMenu( AParent : PControl; MaxCmdReserve: DWORD; const Template : array of PKOLChar;
                      aOnMenuItem: TOnMenuItem; isPopup:boolean): PXPMenu;
var M: PXPMenu;
    {$IFDEF INITIALFORMSIZE_FIXMENU}
    R: TRect;
    {$ENDIF}
                      
begin
  New( Result, Create );
  {+}{++}(*Result := PXPMenu.Create;*){--}
  Result.FVisible := TRUE;
  Result.FPopupFlags := TPM_LEFTALIGN or TPM_LEFTBUTTON;
  Result.FMenuItems := NewList;
  Result.FOnMenuItem := aOnMenuItem;
  if (High(Template)>=0) and (Template[0] <> nil) then
  begin
    if (AParent <> nil) and (PXPControl( AParent).fMenuObj = nil) and not PXPControl( AParent).IsControl then
      Result.FHandle := CreateMenu
    else
      Result.FHandle := CreatePopupMenu;
    Result.FillMenuItems( Result.FHandle, 0, Template );
  end;
  if Assigned( AParent ) then
  begin
    Result.FControl :=PControl( AParent);
    if Assigned(PXPControl( AParent).fMenuObj) then
    begin
      // add popup menu to the end of menu chain
      M := PXPMenu( PXPControl( AParent).fMenuObj );
      while Assigned(M.fNextMenu) do
        M := PXPMenu(M.fNextMenu);
      M.fNextMenu := Result;
    end
       else
    begin
      if not PXPControl( AParent).IsControl then
      begin
        {$IFDEF INITIALFORMSIZE_FIXMENU}
        R := AParent.ClientRect;
        {$ENDIF}
        AParent.Menu := Result.FHandle;
        {$IFDEF INITIALFORMSIZE_FIXMENU}
        AParent.SetClientSize( R.Right, R.Bottom );
        {$ENDIF}
      end;
      PXPControl( AParent).fMenuObj := Result;
      AParent.AttachProc(WndProcMenu );
      {$IFDEF USE_AUTOFREE4CONTROLS}     //dufa
      AParent.Add2AutoFree( Result );
      {$ENDIF}
    end;
  end;

  Result.FGutterColor := clBtnFace; //сера€ полоска
  Result.FBackColor :=  GetLightColor(clBtnFace, 85);
  Result.FSelectedColor := GetLightColor(clHighlight, 65); //выделенный пункт меню
  Result.FSelLightColor := GetLightColor(clHighlight, 75);
  Result.FCheckColor :=clBlack;
  Result.FMinWidth := 0;
  Result.FMinHeight:=0;
  Result.FIsPopup:=isPopup;
  Result.FFont := NewFont;
  Result.Add2AutoFree(Result.FFont);
end;

procedure TXPMenu.InitCheckBmp;
const   ChkBMP: array[0..11] of word=(0,0,0,8,24,568,880,992,448,128,0,0);

var
  i,j: Byte;
  row: PAGRBQuad;
  x:word;

begin
  FBmpCheck := NewDIBBitmap(12,12,pf32bit);
  Add2AutoFree(FBmpCheck);
  with FBmpCheck^ do
  begin
    if FCheckColor=clWhite then Canvas.Brush.Color := clBlack else Canvas.Brush.Color := clWhite;
    Canvas.FillRect(Rect(0, 0, Width, Height));
    for j:=0 to Height-1 do begin
      row:=ScanLine[j];  x:=ChkBMP[j];
      for i:=0 to Width-1 do begin
         if (x and 2048)=2048 then row[i]:=Color2RGBQuad(FCheckColor);
         x:=x shl 1;
      end;
    end;
  end
end;

function TXPMenu.TextExtent(const Text: string): TSize;
var
  DC: HDC;
begin
  DC := CreateCompatibleDC( 0 );
  SelectObject(DC,FFont.Handle);
  GetTextExtentPoint32( DC, PKOLChar(Text), Length(Text), Result);
  DeleteDC(DC);
end;

{destructor TXPMenu.Destroy;
begin
  FFont.Free;
  if Assigned(FBmpCheck) then
    Free_And_Nil(FBmpCheck);
  inherited;
end;}

procedure TXPMenu.InternalInitItems(Item : PMenu);
//бежит по всем пунктам, при случае загл€дыва€ в подпункты
var
  I: Integer;
begin
  with Item^ do begin
    OnMeasureItem := MeasureItem;
    OnDrawItem := DrawItem;
    OwnerDraw :=true;
    for I := 0 to Count - 1 do
       if Items[I].Count > 0 then InternalInitItems(Items[I]);
  end;
end;

procedure TXPMenu.DrawXPstyle;
var i:integer;
begin
   for i:=0 to Count -1 do
    InternalInitItems(Items[i]);
end;

//собственно отрисовка-c
function  TXPMenu.DrawItem(Sender: PObj; DC: HDC;
  const Rect: TRect; ItemIdx: Integer; DrawAction: TDrawAction;
  ItemState: TDrawState): Boolean;

var BitmapSize:tagBITMAP;
    aBrush, aPen, aFont: PGraphicTool;
    i:byte;
    BMP,maskBMP{, grayBMP}:PBitmap;
{    oldBrush:HBrush;
    oldPen:HPen;
    oldFont:HFont;}
    GutterWidth:Integer;
    TopLevel:boolean;
    ARect:TRect;

  function GetGutterWidth(IsLine: Boolean): Integer;
  begin

    with PMenu(Sender)^ do
    begin
      if Pointer(Bitmap)<>nil then
      begin
        Result := Max(BitmapSize.bmWidth + 4,
            Rect.Bottom - Rect.Top); //четыре точки до картинки + картинка + п€ть после
        if IsLine then
          Result := Max(Result, TextExtent('W').cy  + 4);
      end else
      if IsLine then
        Result := TextExtent('W').cy  + 4
      else
        Result := Rect.Bottom - Rect.Top; {ширина = высоте + 2 + 2 точки}
    end;
    Result := Max(Result, ItemHeight) + 1;
  end;

  procedure RGB2GrayScale(grayBMP:PBitmap);
  var i,j:word;
      fFrom: TRGB;
      c:byte;

  begin
   with grayBMP^ do
    for i:=0 to Width-1 do
       for j:=0 to Height-1 do begin
         FFrom := GetRGB(Pixels[i,j]);
         with FFrom do c:=round(0.30*R+0.59*G+0.11*B);
         Pixels[i,j]:=RGB(c,c,c) ;
       end;
  end;

procedure MyPolyline(DC: HDC;const Points: array of TPoint);
begin
  Polyline(DC, PPoints(@Points)^, High(Points) + 1);
end;


const
  //текстовые флаги
  _Flags: LongInt = DT_NOCLIP or DT_VCENTER or DT_END_ELLIPSIS or DT_SINGLELINE;
  _FlagsTopLevel: array[Boolean] of Longint = (DT_LEFT, DT_CENTER);
  _FlagsShortCut: Longint = (DT_RIGHT);

begin
  with PMenu(Sender)^ do begin
    if Pointer(Bitmap)<>nil then
      GetObject(Bitmap , sizeof(tagBITMAP), @BitmapSize);
    GutterWidth:=GetGutterWidth(IsSeparator);
    TopLevel:=(TopParent.IndexOf( Parent )=-1) and not FIsPopup;
    aBrush:=NewBrush;
    aPen:=NewPen;
    aFont:=NewFont;

    aPen.Color := GetShadeColor(clHighlight, 50);
//    oldPen:=SelectObject(DC,aPen.Handle);
//    oldBrush:=SelectObject(DC,aBrush.Handle);
    if (odsSelected in ItemState) then //если пункт меню выделен
    begin
      if TopLevel then //если это полоска основного меню
      begin
        aBrush.Color := BtnHighLight;
        SelectObject(DC,aBrush.Handle);
        FillRect(DC,Rect,aBrush.Handle);
        aPen.Color := GetShadeColor(clBtnShadow, 50);
        SelectObject(DC,aPen.Handle);
        MyPolyline(DC,[
          Point(Rect.Left, Rect.Bottom-1),
          Point(Rect.Left, Rect.Top),
          Point(Rect.Right-1, Rect.Top),
          Point(Rect.Right-1, Rect.Bottom)
        ]);
      end else
      if not (odsDisabled in ItemState) then
      begin
        aBrush.Color := FSelectedColor;
        SelectObject(DC,aBrush.Handle);
        Rectangle(DC,Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
      end
    end else
    if TopLevel then //если это полоска основного меню
    begin
      if (odsHotList in ItemState) then //если мышь над пунктом меню
      begin
        aPen.Color := GetShadeColor(clHighlight, 50);
        SelectObject(DC,aPen.Handle);
        aBrush.Color := FSelectedColor;
        SelectObject(DC,aBrush.Handle);
        Rectangle(DC,Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
      end else
      begin
        aBrush.Color := clBtnFace;
        FillRect(DC,Rect,aBrush.Handle);
      end
    end else
      begin //ничем не примечательный пункт меню
        aBrush.Color := FGutterColor; //полоска
        FillRect(DC,Types.Rect(Rect.Left, Rect.Top, Rect.Left + GutterWidth, Rect.Bottom),aBrush.Handle );
        aBrush.Color := FBackColor;
        FillRect(DC,Types.Rect(Rect.Left + GutterWidth, Rect.Top, Rect.Right, Rect.Bottom),aBrush.Handle);
      end;

    if odsChecked in ItemState then
    begin // подсвечиваем чекнутый пункт меню
      aPen.Color := GetShadeColor(clHighlight, 50);
      SelectObject(DC,aPen.Handle);
      if (odsSelected in ItemState) then aBrush.Color :=  GetShadeColor(FSelLightColor, 40)
       else aBrush.Color := FSelLightColor;
      SelectObject(DC,aBrush.Handle);
      Rectangle(DC,(Rect.Left + 1), (Rect.Top + 1),
        (Rect.Left - 1 + GutterWidth - 1), (Rect.Bottom - 1)   );
    end;

    if (Pointer(Bitmap)<>nil) and (not TopLevel) then begin
      BMP:=NewBitmap(BitmapSize.bmWidth ,BitmapSize.bmHeight);
      BMP.Handle:=CopyImage(Bitmap,IMAGE_BITMAP ,0,0,0);
      maskBMP:=NewBitmap(BitmapSize.bmWidth ,BitmapSize.bmHeight);
      maskBMP.Handle:=CopyImage(Bitmap,IMAGE_BITMAP ,0,0,0);
      maskBMP.Convert2Mask(BMP.Pixels[0,0]);
      if not (odsDisabled in ItemState) then begin //рисуем цветную картинку
          if (odsSelected in ItemState) then begin
            {grayBMP:=NewBitmap(BitmapSize.bmWidth ,BitmapSize.bmHeight);
            grayBMP.Handle:=CopyImage(Bitmap,IMAGE_BITMAP ,0,0,0);
            RGB2GrayScale(grayBMP);
            grayBMP.DrawMasked(DC ,(ARect.Left + GutterWidth - 1 - BitmapSize.bmWidth ) shr 1+2,
             (ARect.Top + ARect.Bottom - BitmapSize.bmHeight ) shr 1+2, maskBMP.Handle);
            grayBMP.Free; }
          end;
        end else begin //рисуем погасшую картинку
          RGB2GrayScale(BMP);
       end;
       BMP.DrawMasked(DC ,(Rect.Left + GutterWidth - 1 - BitmapSize.bmWidth ) shr 1,
          (Rect.Top + Rect.Bottom - BitmapSize.bmHeight ) shr 1, maskBMP.Handle);
       maskBMP.Free;   BMP.Free;
    end else
    if odsChecked in ItemState then begin
      if not Assigned(FBmpCheck) then InitCheckBmp;
      FBmpCheck.DrawTransparent(DC,(Rect.Left + GutterWidth - 1 - FBmpCheck.Width) shr 1,
          (Rect.Top + Rect.Bottom - FBmpCheck.Height) shr 1,FBmpCheck.Pixels[0,0]);
    end;
    ARect:=Rect;
    if not TopLevel then
      Inc(ARect.Left, GutterWidth + 5); //отступ дл€ текста

    aFont.Assign(Font);
    with aFont^ do
    begin
      if (odsDefault in ItemState) then FontStyle := [fsBold];
      if (odsDisabled in ItemState) then Color := clGray;
    end;
//    oldFont:=SelectObject(DC,aFont.Handle);

    if IsSeparator then //если разделитель
    begin
      aPen.Color := clBtnShadow;
      SelectObject(DC,aPen.Handle);
      MyPolyline(DC,[
        Point(Rect.Left, ARect.Top + (ARect.Bottom - ARect.Top) shr 1),
        Point(Rect.Right, ARect.Top + (ARect.Bottom - ARect.Top) shr 1)]);
    end else
    begin //текст меню
      i:=1; while (i<=Length(Caption)) and  (Caption[i]<>#9) do inc(i);
{      i:=Pos(#9, Caption);
      if i=0 then i:=Length(Caption)+1;
}      SetBkMode(DC, TRANSPARENT);
      SetTextColor(DC, aFont.Color );
      DrawText(DC, PKOLChar(copy(Caption,1,i-1)),i-1 ,ARect,
        _Flags or _FlagsTopLevel[TopLevel]);
      if i<Length(Caption) then //разпальцовка
      begin
        Dec(ARect.Right, 5);
        DrawText(DC, PKOLChar(copy(Caption,i+1,Length(Caption)-i)),Length(Caption)-i,
          ARect, _Flags or _FlagsShortCut);
      end
    end;
    //DeleteObject(SelectObject(DC, oldFont));
    //DeleteObject(SelectObject(DC, oldBrush));
    //DeleteObject(SelectObject(DC, oldPen));
    aFont.Free;
    aBrush.Free;
    aPen.Free;
  end;
  Result:=true;
end;

//размеры меню
function TXPMenu.MeasureItem(Sender: PObj;  Idx: Integer): Integer;
VAR Bound:integer;
    bb:packed record
      Height:word;
      Width:word;
    end absolute Bound;
    BitmapSize:tagBitmap;

begin
  with PMenu(Sender)^ do
   if (TopParent.IndexOf( Parent )=-1) and not FIsPopup then
    begin
      bb.Width := TextExtent(Caption).cX;
      bb.Height := TextExtent(Caption).cY;
    end else begin

     if Pointer(Bitmap)<>nil then
     begin
       GetObject(Bitmap , sizeof(tagBITMAP), @BitmapSize);
       if IsSeparator then
         if Max(ItemHeight, BitmapSize.bmHeight ) > 20 then //при большем 20 узка€ полоска некрасива
            bb.Height := 11 else bb.Height := 5
       else
         begin
           bb.Height := Max(ItemHeight,
             Max(TextExtent('W').cy , BitmapSize.bmHeight ) + 4);

           bb.Width := BitmapSize.bmWidth ;
           if bb.Width < bb.Height then bb.Width := bb.Height else bb.Width := bb.Width + 5;
           bb.Width := Max(ItemWidth,
             bb.Width + TextExtent(Caption).cx + 15);
         end
     end else
       begin
         bb.Height := Max(TextExtent('W').cY + 4, ItemHeight);
         bb.Width := Max(ItemWidth, bb.Height + TextExtent(Caption).cx  + 15);

         if IsSeparator then
           if bb.Height > 20 then //при большем 20 узка€ полоска некрасива
              bb.Height := 11 else bb.Height := 5;
       end

    end;
    Result:=Bound;
end;

procedure TXPMenu.SetFont(Value: PGraphicTool);
begin
  FFont.Assign(Value);
end;

procedure TXPMenu.SetSelectedColor(const Value: TColor);
begin
  FSelectedColor := Value;
  FSelLightColor := GetLightColor(Value, 75);
end;

end.
