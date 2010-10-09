{  ToGRush -- (C) by Vladimir Kladov, 2010
This version is compatible only with KOL + Grush Controls of version 3.00+

Purpose: provides easy way to convert KOL project to use Grush controls
inplace of standard Windows controls. To use it in most cases it is
sufficiently to add ToGrush into uses clause after the KOL.pas, KOLadd.pas
and other KOL units. Use also symbols defined below to change options.
Note, that with symbol TOGRUSH_OPTIONAL, it is possible to create dual view
project, controlling if  Grush controls are used or not via a variable
NoGrush.

KOLGRushControls are created (C) by Karpinsky Alexander aka homm in 2007.
}

unit ToGRush;

interface

{$I KOLDEF.inc}

{$IFDEF EXTERNAL_DEFINES}
        {$INCLUDE EXTERNAL_DEFINES.INC}
{$ENDIF EXTERNAL_DEFINES}

//{$DEFINE TOGRUSH_AUTO_DISIMAGES} // add this symbol to provide 256 gray images
                                   // based on original ones for Disabled state
                                   // of toolbar buttons
//{$DEFINE TOGRUSH_AUTO_DIS_EQ} // RGB channels of the same level while TOGRUSH_AUTO_DISIMAGES
//{$DEFINE TOGRUSH_DROPBTN2} // Drop button will be placed right to the button
                             // having property DropDown, not in the button
//{$DEFINE TOGRUSH_NO_AUTO_SIZE_BTNS} // not use AutoSize for buttons
           // (sensible only in a case, when only images are in the toolbar)

//{$DEFINE TOGRUSH_NO_MESSAGEBOX} // not use MessageBox replacement
//{$DEFINE TOGRUSH_NO_SCROLLBARS} // not convert scrollbar colors for ScrollBar controls

//{$DEFINE TOGRUSH_OPTIONAL} // define it to allow controlling if actually use GRush controls or not
                             // (via variable NoGRush)

uses Windows, Messages, KOL, KOLGRushControls;

function NewButton( AParent: PControl; const Caption: KOLString ): PControl;

function NewCheckbox( AParent: PControl; const Caption: KOLString ): PControl;
function NewRadiobox( AParent: PControl; const Caption: KOLString ): PControl;

function NewPanel( AParent: PControl; EdgeStyle: TEdgeStyle ): PControl;

function NewSplitter( AParent: PControl; MinSizePrev, MinSizeNext: Integer ): PControl;
function NewSplitterEx( AParent: PControl; MinSizePrev, MinSizeNext: Integer;
         EdgeStyle: TEdgeStyle ): PControl;

var GRush_Force_Flat_Toolbars: Boolean;
function NewToolbar( AParent: PControl; Align: TControlAlign; Options: TToolbarOptions;
                     Bitmap: HBitmap; const Buttons: array of PKOLChar;
                     const BtnImgIdxArray: array of Integer ) : PControl;
procedure ToolbarAddButtons( Toolbar: PControl; const Buttons: array of PKOLChar;
   const BtnImgIdxArray: array of Integer; Bitmap: HBitmap );
function ToolbarButtonRect( Toolbar: PControl; BtnID: Integer ): TRect;
procedure ToolbarSetTooltips( Toolbar: PControl; BtnID1st: Integer; const Tooltips: array of PKOLChar );
function ToolbarButtonEnabled( Toolbar: PControl; BtnID: Integer ): Boolean;
function ToolbarButtonChecked( Toolbar: PControl; BtnID: Integer): Boolean;
procedure ToolbarButtonSetChecked( Toolbar: PControl; BtnID: Integer; Checked: Boolean );
procedure EnableToolbarButton( Toolbar: PControl; BtnID: Integer; Enable: Boolean );
function ToolbarButtonVisible( Toolbar: PControl; BtnID: Integer ): Boolean;
procedure ShowHideToolbarButton( Toolbar: PControl; BtnID: Integer; Show: Boolean );


function NewProgressbar( AParent: PControl ): PControl;
function NewProgressbarEx( AParent: PControl; Options: TProgressbarOptions ): PControl;

{$IFNDEF TOGRUSH_NO_MESSAGEBOX}
function MessageBox( Wnd: HWnd; msg, title: PChar; flags: DWORD ): Integer; stdcall;
{$ENDIF}

{$IFNDEF TOGRUSH_NO_SCROLLBARS}
function NewScrollBar( AParent: PControl; BarSide: TScrollerBar ): PControl;
function  Scrollbar_GetMinPos( sb: PControl ): Integer;
procedure Scrollbar_SetMinPos( sb: PControl; m: Integer );
procedure Scrollbar_SetAll( sb: PControl; min, max, pg, cur: Integer );
function  Scrollbar_GetMaxPos( sb: PControl ): Integer;
procedure Scrollbar_SetMaxPos( sb: PControl; m: Integer );
function  Scrollbar_GetCurPos( sb: PControl ): Integer;
procedure Scrollbar_SetCurPos( sb: PControl; newp: Integer );
procedure Scrollbar_SetPageSz( sb: PControl; psz: Integer );
function  Scrollbar_GetPageSz( sb: PControl ): Integer;
procedure Scrollbar_SetLineSz( sb: PControl; lnz: Integer );
function  Scrollbar_GetLineSz( sb: PControl ): Integer;

{$IFNDEF TOGRUSH_NO_WINDOW_SCROLLBARS}
procedure OverrideScrollbars( C: PControl );
{$ENDIF}

{$ENDIF}

{$IFNDEF TOGRUSH_NO_COMBO_EDIT}
function NewComboBox( AParent: PControl; Options: TComboOptions ): PControl;
function NewEditBox( AParent: PControl; Options: TEditOptions ): PControl;
{$ENDIF}

{$IFNDEF TOGRUSH_NO_GRADIENTPANEL}
function NewGradientPanel( AParent: PControl; Color1, Color2: TColor ): PControl;
function NewGradientPanelEx( AParent: PControl; Color1, Color2: TColor;
                             Style: TGradientStyle; Layout: TGradientLayout ): PControl;
{$ENDIF}

{$DEFINE ROUND_RADIOITEMS} // if commented, the same as check boxes
{$DEFINE RED_ACCELERATORS} // if commented, accelerators are drawn using underline as usual
//{$DEFINE RED_GREEN_ACCELS} // too colored!
function OwnerDrawMenuItem( var Msg: TMsg; const Menus: array of PMenu;
         var Rslt: Integer): Boolean;
var   MenuHighlight: TColor = clGRushHiLight;
      MenuBackground: TColor = clGRushLighten; //$EBE3DD
      MenuTextColor: TColor = clBlack;
      MenuTextHighlight: TColor = clWhite;
      MenuTextDisabled: TColor = clGray;
      MenuTextDisabSel: TColor = clSilver;
      MenuLine1Color: TColor = clBlack;
      MenuLine2Color: TColor = clGRushLight;
      MenuCheckBoxBkColor: TColor = clWhite;
      MenuCheckBoxBorder: TColor = clBlack;
      MenucheckBoxCheck: TColor = clGRushHiLight;
      MenuAccelColor: TColor = {$IFDEF RED_GREEN_ACCELS} clRed {$ELSE} clBlue {$ENDIF};
      MenuAccelSelColor: TColor = {$IFDEF RED_GREEN_ACCELS} clLime {$ELSE} clNavy {$ENDIF};
      MenuAccelDisabled: TColor = clDkGray;
      MenuAccelSelDisabled: TColor = clDkGray;
      MenuHotKeyTextColor: TColor = {$IFDEF RED_GREEN_ACCELS} clBlue {$ELSE} clGRushHiLight {$ENDIF};
      MenuHotKeySelTxColor: TColor = clNavy;
      MenuHotKeyTxDisabled: TColor = clDkGray;
      MenuHotKeySelTxDisabled: TColor = clDkGray;

{ To use OwnerDrawMenuItem:
  1. set OwnerDraw to TRUE for all menu items;
  2. in Form.OnMessage, write following code:

function TForm1.KOLForm1Message(var Msg: tagMSG;
  var Rslt: Integer): Boolean;
begin
  Result := FALSE;
  if (Msg.message = WM_DRAWITEM) or (Msg.message = WM_MEASUREITEM) then
  begin
    Result := OwnerDrawMenuItem( Msg, [ PopupMenu1, PopupMenu2, PopupMenu3, PopupMenu4 ],
      Rslt );
  end
    else .......
}

{$IFDEF TOGRUSH_OPTIONAL}
var NoGrush: Boolean;
{$ENDIF TOGRUSH_OPTIONAL}

function TriangleUpBitmap( Horizontal: Boolean ): PBitmap;
function TriangleDnBitmap( Horizontal: Boolean ): PBitmap;

implementation

uses KOLadd;

const
  IS_DRDWN = 16;
type
  PControl_ = ^TControl_;
  TControl_ = object( TControl )
  end;

////////////////////////////////////////////////////////////////////////////////
// BUTTON, CHECK, RADIO CHECK
////////////////////////////////////////////////////////////////////////////////
function NewButton( AParent: PControl; const Caption: KOLString ): PControl;
begin
  {$IFDEF TOGRUSH_OPTIONAL}
  if not NoGrush then
  {$ENDIF TOGRUSH_OPTIONAL}
  begin
    Result := Pointer( NewGRushButton( AParent, Caption ).SetSize( 64, 22 ) );
    {$IFDEF USE_FLAGS} include( PControl_( Result ).fFlagsG5, G5_IsButton );
    {$ELSE} PControl_( Result ).fIsButton := TRUE; {$ENDIF}
  end
  {$IFDEF TOGRUSH_OPTIONAL}
    else
  begin
    Result := Kol.NewButton( AParent, Caption )
  end
  {$ENDIF TOGRUSH_OPTIONAL}
  ;
end;

function NewCheckbox( AParent: PControl; const Caption: KOLString ): PControl;
begin
  {$IFDEF TOGRUSH_OPTIONAL}
  if not NoGrush then
  {$ENDIF TOGRUSH_OPTIONAL}
    Result := Pointer( NewGRushCheckBox( AParent, Caption ).SetSize( 64, 22 ) )
  {$IFDEF TOGRUSH_OPTIONAL}
  else
    Result := Kol.NewCheckBox( AParent, Caption )
  {$ENDIF TOGRUSH_OPTIONAL}
  ;
end;

function NewRadiobox( AParent: PControl; const Caption: KOLString ): PControl;
begin
  {$IFDEF TOGRUSH_OPTIONAL}
  if not NoGrush then
  {$ENDIF TOGRUSH_OPTIONAL}
    Result := Pointer( NewGRushRadioBox( AParent, Caption ).SetSize( 64, 22 ) )
  {$IFDEF TOGRUSH_OPTIONAL}
  else
    Result := Kol.NewRadiobox( AParent, Caption )
  {$ENDIF TOGRUSH_OPTIONAL}
  ;
end;

////////////////////////////////////////////////////////////////////////////////
// PANEL
////////////////////////////////////////////////////////////////////////////////
function NewPanel( AParent: PControl; EdgeStyle: TEdgeStyle ): PControl;
begin
  {$IFDEF TOGRUSH_OPTIONAL}
  if not NoGrush then
  {$ENDIF TOGRUSH_OPTIONAL}
  begin
    if EdgeStyle >= esTransparent then
    begin
      Result := KOL.NewPanel( AParent, esNone ).SetSize( 64, 64 );
      if EdgeStyle = esTransparent then
        Result.Transparent := TRUE;
    end
    else
      Result := Pointer( NewGRushPanel( AParent ) );
  end
  {$IFDEF TOGRUSH_OPTIONAL}
  else Result := Kol.NewPanel( AParent, EdgeStyle )
  {$ENDIF TOGRUSH_OPTIONAL}
  ;
end;

////////////////////////////////////////////////////////////////////////////////
// SPLITTER
////////////////////////////////////////////////////////////////////////////////
function NewSplitter( AParent: PControl; MinSizePrev, MinSizeNext: Integer ): PControl;
begin
  {$IFDEF TOGRUSH_OPTIONAL}
  if not NoGrush then
  {$ENDIF TOGRUSH_OPTIONAL}
  begin
    Result := Pointer( NewGRushSplitter( AParent, MinSizePrev, MinSizeNext ) );
    Result.Transparent := TRUE;
  end
  {$IFDEF TOGRUSH_OPTIONAL}
    else Result := Kol.NewSplitter( AParent, MinSizePrev, MinSizeNext )
  {$ENDIF TOGRUSH_OPTIONAL}
  ;
end;

function NewSplitterEx( AParent: PControl; MinSizePrev, MinSizeNext: Integer;
         EdgeStyle: TEdgeStyle ): PControl;
begin
  {$IFDEF TOGRUSH_OPTIONAL}
  if not NoGrush then
  {$ENDIF TOGRUSH_OPTIONAL}
  begin
    Result := Pointer( NewGRushSplitter( AParent, MinSizePrev, MinSizeNext ) );
    Result.Transparent := TRUE;
  end
  {$IFDEF TOGRUSH_OPTIONAL}
    else Result := Kol.NewSplitterEx( AParent, MinSizePrev, MinSizeNext, EdgeStyle )
  {$ENDIF TOGRUSH_OPTIONAL}
  ;
end;

////////////////////////////////////////////////////////////////////////////////
// TOOL BAR
////////////////////////////////////////////////////////////////////////////////
type
  TTBButtonEvent = packed Record
    BtnID: DWORD;
    Event: TOnToolbarButtonClick;
  end;
  PTBButtonEvent = ^TTBButtonEvent;
procedure ToGR_ClickToolbarBtn( Dummy, Sender: PControl );
var D: DWORD;
    Idx: Integer;
    IsCheck, Checked: Boolean;
    Toolbar: PControl_;
    EventRec: PTBButtonEvent;
begin
  D := GetProp( Sender.Handle, 'GRBTN' );
  Idx := LoWord( D );
  IsCheck := HiWord( D ) and 2 <> 0;
  Checked := HiWord( D ) and 4 <> 0;
  if IsCheck then
  begin
      Checked := not Checked;
      D := D xor $40000;
      SetProp( Sender.Handle, 'GRBTN', D );
      PGrushControl( Sender ).Checked := Checked;
  end;
  Toolbar := Pointer( Sender.Parent );
  if  Assigned( Toolbar.DF.fTBEvents ) and
      (Toolbar.DF.fTBevents.Count > Idx) then
  begin
      EventRec := Toolbar.DF.fTBevents.Items[ Idx ];
      if  Assigned( EventRec.Event ) then
          EventRec.Event( Toolbar, EventRec.BtnID );
  end
    else
  {$IFDEF NIL_EVENTS}
  if Assigned( Toolbar.EV.fOnClick ) then
  {$ENDIF}
  begin
      PControl_( Toolbar ).fCurIndex := PControl_( Toolbar ).fChildren.IndexOf( Sender );
      Toolbar.EV.fOnClick( Toolbar );
  end;
end;

procedure ToGR_ButtonMouseMove( Dummy, Sender: PControl; var M: TMouseEventData );
var P: TPoint;
    M1: TMouseEventData;
begin
  if Assigned( Sender.Parent.OnMouseMove ) then
  begin
    P := MakePoint( M.X, M.Y );
    P := Sender.Client2Screen( P );
    P := Sender.Parent.Screen2Client( P );
    M1 := M;
    M1.X := P.X;
    M1.Y := P.Y;
    Sender.Parent.OnMouseMove( Sender.Parent, M1 );
  end;
end;

procedure ToGR_ClickToolbarBtnDD( Dummy, Sender: PControl );
var D: DWORD;
    Idx: Integer;
    Toolbar: PControl_;
    EventRec: PTBButtonEvent;
begin
  D := GetProp( Sender.Handle, 'GRBTN' );
  Idx := LoWord( D );
  Toolbar := Pointer( Sender.Parent );
  {$IFDEF TOGRUSH_DROPBTN2}
  {$ELSE}
  Toolbar := Pointer( Toolbar.Parent );
  {$ENDIF}
  Toolbar.DF.fTBCurItem := Idx;
  Toolbar.fCurIndex := Idx;
  Toolbar.DF.fTBDropped := TRUE;
  if Assigned( Toolbar.DF.fTBevents ) and
     (Toolbar.DF.fTBevents.Count > Idx) then
  begin
    EventRec := Toolbar.DF.fTBevents.Items[ Idx ];
    Toolbar.DF.fTBCurItem := EventRec.BtnID;
  end;
  if Assigned( Toolbar.EV.fOnDropDown ) then
  begin
    Toolbar.EV.fOnDropDown( Toolbar );
  end
    else
  {$IFDEF NIL_EVENTS}
  if Assigned( Toolbar.EV.fOnClick ) then
  {$ENDIF}
  begin
    Toolbar.EV.fOnClick( Toolbar );
  end;
  Toolbar.DF.fTBDropped := FALSE;
end;

procedure Provide_DIS_images( var B: PBitmap );
var B2: PBitmap;
    y, y_to, x, c: Integer;
    Src, Dst: PRGBQuad;
    first_pixel: Boolean;
    Transp: DWORD;
begin
  if (B =nil) or B.Empty then Exit;
  B2 := NewDIBBitmap( B.Width, B.Height * 2, pf32bit );
  TRY
    B.Draw( B2.Canvas.Handle, 0, 0 );
    y_to := B.Height;
    first_pixel := TRUE;
    Transp := 0;
    for y := 0 to B.Height-1 do
    begin
      Src := B2.ScanLine[ y ];
      Dst := B2.ScanLine[ y_to ];
      for x := 0 to B2.Width-1 do
      begin
        if first_pixel then
          Transp := PDWORD( Src )^ and $FFFFFF;
        first_pixel := FALSE;
        if PDWORD( Src )^ and $FFFFFF = Transp then
          PDWORD( Dst )^ := Transp
        else
        begin
          {$IFDEF TOGRUSH_AUTO_DIS_BAL}
          c := (Src.rgbRed * 64 + Src.rgbGreen * 128 + Src.rgbBlue * (128 + 64))
               div 256;
          {$ELSE}
          c := (Src.rgbRed * 64 + Src.rgbGreen * 64 + Src.rgbBlue * 64)
               div 100;
          {$ENDIF}
          if c > 255 then c := 255;
          Dst.rgbBlue := c;
          Dst.rgbGreen := c;
          Dst.rgbRed := c;
        end;
        inc( Src );
        inc( Dst );
      end;
      inc( y_to );
    end;
  FINALLY
    B.Assign( B2 );
    B2.Free;
  END;
end;

var DrDownBmp: PBitmap;
function NewToolbar( AParent: PControl; Align: TControlAlign; Options: TToolbarOptions;
                     Bitmap: HBitmap; const Buttons: array of PKOLChar;
                     const BtnImgIdxArray: array of Integer ) : PControl;
var //i, BtnID: Integer;
    //B, B2: PGRushControl;
    {$IFDEF GRAPHCTL_XPSTYLES}
    pb: PControl;
    {$ENDIF}
    //C: String;
    //IsSep: Boolean;
    //IsDropDown: Boolean;
    //IsCheck, Checked, IsRadio: Boolean;
    //Idx: Integer;
    //D: DWORD;
    //imgW, imgH, W,
    H: Integer;
    Bmp: PBitmap;
    //DD_dst: PByte;
    //y: Integer;
    ES: TEdgeStyle;
const DD_img: array[ 0..6 ] of Byte = ( $0, $F8, $F8, $70, $70, $20, $20 );
begin
  {$IFDEF TOGRUSH_OPTIONAL}
  if NoGrush then
  begin
    Result := Kol.NewToolbar( AParent, Align, Options, Bitmap, Buttons, BtnImgIdxArray );
    Exit;
  end;
  {$ENDIF TOGRUSH_OPTIONAL}
  if Align = caNone then Align := caTop;
  H := 0;

  //imgW := 0;
  //imgH := 0;
  Bmp := nil;
  if (Bitmap <> 0) and (Bitmap <> THandle( -1 )) then
  begin
    Bmp := NewBitmap( 0, 0 );
    Bmp.Handle := Bitmap;
    //imgH := Bmp.Height;
    //imgW := imgH;
    H := Bmp.Height + 12;
    //Bmp.PixelFormat := pf32bit;
    //Bmp.SaveToFile( GetStartDir + 'test_toolbar1.bmp' );
    {$IFDEF TOGRUSH_AUTO_DISIMAGES}
    Provide_DIS_images( Bmp );
    {$ENDIF}
  end;

  ES := esNone;
  if ([tboTransparent, tboFlat] * Options <> []) or GRush_Force_Flat_Toolbars then
  begin
    ES := esTransparent;
    {if not( tboTransparent in Options ) then
      ES := esSolid;}
  end;
  Result := Pointer( NewPanel( AParent, ES ).SetSize( 0, H ).SetAlign(Align) );
  ToolbarAddButtons( Result, Buttons, BtnImgIdxArray, Bitmap );
  (*
  Idx := -1;
  for i := 0 to High( Buttons ) do
  begin
    C := Buttons[ i ];
    IsSep := C = '-';
    IsDropDown := FALSE;
    IsCheck := FALSE;
    Checked := FALSE;
    BtnID := i;
    if IsSep then C := ''
    else
    begin
      Inc( Idx );
      IsDropDown := (C <> '') and (C[ 1 ] = '^');
      if IsDropDown then Delete( C, 1, 1 );
      IsCheck := (C <> '') and (C[ 1 ] in [ '+', '-' ]);
      if IsCheck then
      begin
        Checked := C[ 1 ] = '+';
        Delete( C, 1, 1 );
        IsRadio := (C <> '') and (C[ 1 ] = '!');
        if IsRadio then Delete( C, 1, 1 );
      end;
      {$IFDEF TOOLBAR_DOT_NOAUTOSIZE_BUTTON}
      if (C <> '') and (C[ 1 ] = '.') then
        Delete( C, 1, 1 );
     {$ENDIF TOOLBAR_DOT_NOAUTOSIZE_BUTTON}
    end;
    if Trim( C ) = '' then C := '';
    if IsSep then
      {$IFDEF GRAPHCTL_XPSTYLES}
      begin
        pb := NewPaintbox( Result ).SetSize( 6, 0 ).SetAlign( caLeft );
        pb.Transparent := TRUE;
      end
      {$ELSE}
      NewPanel( Result, esTransparent ).SetSize( 6, 0 ).SetAlign( caLeft )
        {$IFNDEF TOGRUSH_TOOLBAR_NOMOUSEMOVE}
        .MouseTransparent
        {$ENDIF}
      {$ENDIF}
    else
    begin
      if C = '' then
      begin
        W := 32;
        if H <> 0 then W := H;
      end
        else
      begin
        W := 64;
      end;
      B := Pointer( NewButton( Result, C ).SetSize( W, 0 ).SetAlign( caLeft ) );
      {$IFDEF USE_NAMES}
      //B.Name := 'TB' + Int2Str( Idx+1 );
      {$ENDIF USE_NAMES}
      B.Tabstop := FALSE;
      B.LikeSpeedButton;
      {$IFNDEF TOGRUSH_TOOLBAR_NOMOUSEMOVE}
      B.OnMouseMove := TOnMouse( MakeMethod( nil, @ ToGR_ButtonMouseMove ) );
      {$ENDIF}
      B.Transparent := TRUE;
      if IsSep then B.Enabled := FALSE;
      if B.GetWindowHandle <> 0 then
      begin
        D := i   or Integer( IsSep ) shl 16
                 or Integer( IsCheck ) shl 17
                 or Integer( Checked ) shl 18
                 or Integer( IsDropDown ) shl 19
                 ;
        SetProp( B.Handle, 'GRBTN', D );
      end;
      SetProp( B.Handle, 'BTNID', BtnID );
      B.OnClick := TOnEvent( MakeMethod( nil, @ ToGR_ClickToolbarBtn ) );
      if Bmp <> nil then
      begin
        B.All_GlyphItemX := idx;
        B.All_GlyphItemY := 0;
        B.All_GlyphBitmap := Bmp;
        B.All_GlyphWidth := ImgW;
        B.All_GlyphHeight := ImgH;
        //B.All_GlyphAttached := TRUE;
        {$IFDEF TOGRUSH_AUTO_DISIMAGES}
        B.Dis_GlyphItemX := idx;
        B.Dis_GlyphItemY := 1;
        B.All_GlyphBitmap := Bmp;
        B.All_GlyphWidth := ImgW;
        B.All_GlyphHeight := ImgH;
        {$ENDIF}
        if not IsDropDown and (C = '') then
          B.All_GlyphHAlign := haCenter;
      end;
      {$IFNDEF TOGRUSH_NO_AUTO_SIZE_BTNS}
        B.aAutoSzX := 10 + ImgW;
        if ImgW > 0 then inc( B.aAutoSzX, 5 );
        if IsDropDown then inc( B.aAutoSzX, 10 );
        B.AutoSize( TRUE );
      {$ENDIF}
      if IsDropDown then
      begin
        {$IFDEF TOGRUSH_DROPBTN2}
        B2 := Pointer( NewButton( Result, C ).SetSize( 5 + 8, 0 ).SetAlign( caLeft ) );
        {$ELSE}
        //B.AutoSize( FALSE );
        //B.Width := W + 13;
        B.All_TextHAlign := haLeft;
        B.Border := 2;
        B2 := Pointer( NewButton( B, C ).SetSize( 5 + 8, 0 ).SetAlign( caRight ) );
        {$ENDIF}
        {$IFDEF USE_NAMES}
        //B2.Name := 'TB_dd' + Int2Str( Idx+1 );
        {$ENDIF USE_NAMES}
        B2.Tabstop := FALSE;
        B2.LikeSpeedButton;
        B2.Transparent := TRUE;
        PGrushControl( B2 ).All_BorderWidth := 0;
        PGrushControl( B2 ).Over_BorderWidth := 1;
        if B2.GetWindowHandle <> 0 then
        begin
          D := i   or Integer( IsSep ) shl 16
                   or Integer( IsCheck ) shl 17
                   or Integer( Checked ) shl 18
                   or Integer( IsDropDown ) shl 19
                   or IS_DRDWN shl 16;
          SetProp( B2.Handle, 'GRBTN', D );
        end;
        B2.OnClick := TOnEvent( MakeMethod( nil, @ ToGR_ClickToolbarBtnDD ) );
        if DrDownBmp = nil then
        begin
          DrDownBmp := NewDIBBitmap( 5, High( DD_img )+1, pf1bit );
          DrDownBmp.DIBPalEntries[ 0 ] := $686868;
          DrDownBmp.DIBPalEntries[ 1 ] := $FFFFFF;
          for y := 0 to High( DD_img ) do
          begin
            DD_dst := DrDownBmp.ScanLine[ y ];
            DD_dst^ := not DD_img[ y ];
          end;
          B2.All_GlyphBitmap := DrDownBmp;
        end
          else
        begin
          B2.All_GlyphBitmap := DrDownBmp;
        end;
        B2.All_GlyphWidth := 5;
        B2.All_GlyphHeight := High( DD_img )+1;
        B2.All_GlyphHAlign := haCenter;
        B2.All_GlyphVAlign := vaBottom;
      end;
    end;
  end;
  *)
  if Bmp <> nil then
  begin
    Bmp.Free;
  end;
end;

procedure ToolbarAddButtons( Toolbar: PControl; const Buttons: array of PKOLChar;
   const BtnImgIdxArray: array of Integer; Bitmap: HBitmap );
var i, Idx, BtnID, W, H, ImgH, ImgW, y: Integer;
    IsSep, IsDropDown, IsCheck, Checked, IsRadio: Boolean;
    C: KOLString;
    B, B2: PGrushControl;
    D: DWORD;
    Bmp: PBitmap;
    DD_dst: PByte;
const DD_img: array[ 0..6 ] of Byte = ( $0, $F8, $F8, $70, $70, $20, $20 );
begin
  H := 0;
  imgW := 0;
  imgH := 0;
  Bmp := nil;
  if (Bitmap <> 0) and (Bitmap <> THandle( -1 )) then
  begin
    Bmp := NewBitmap( 0, 0 );
    Bmp.Handle := Bitmap;
    imgH := Bmp.Height;
    imgW := imgH;
    H := Bmp.Height + 12;
    {$IFDEF TOGRUSH_AUTO_DISIMAGES}
    Provide_DIS_images( Bmp );
    {$ENDIF}
  end;
  Idx := -1;
  for i := 0 to High( Buttons ) do
  begin
    C := Buttons[ i ];
    IsSep := C = '-';
    IsDropDown := FALSE;
    IsCheck := FALSE;
    Checked := FALSE;
    BtnID := i;
    if IsSep then C := ''
    else
    begin
      Inc( Idx );
      IsDropDown := (C <> '') and (C[ 1 ] = '^');
      if IsDropDown then Delete( C, 1, 1 );
      IsCheck := (C <> '') and CharIn(C[1], [ '+', '-' ]);
      if IsCheck then
      begin
        Checked := C[ 1 ] = '+';
        Delete( C, 1, 1 );
        IsRadio := (C <> '') and (C[ 1 ] = '!');
        if IsRadio then Delete( C, 1, 1 );
      end;
      {$IFDEF TOOLBAR_DOT_NOAUTOSIZE_BUTTON}
      if (C <> '') and (C[ 1 ] = '.') then
        Delete( C, 1, 1 );
     {$ENDIF TOOLBAR_DOT_NOAUTOSIZE_BUTTON}
    end;
    if Trim( C ) = '' then C := '';
    if IsSep then
      {$IFDEF GRAPHCTL_XPSTYLES}
      begin
        pb := NewPaintbox( Result ).SetSize( 6, 0 ).SetAlign( caLeft );
        pb.Transparent := TRUE;
      end
      {$ELSE}
      NewPanel( Toolbar, esTransparent ).SetSize( 6, 0 ).SetAlign( caLeft )
        {$IFNDEF TOGRUSH_TOOLBAR_NOMOUSEMOVE}
        .MouseTransparent
        {$ENDIF}
      {$ENDIF}
    else
    begin
      if C = '' then
      begin
        W := 32;
        if H <> 0 then W := H;
      end
        else
      begin
        W := 64;
      end;
      B := Pointer( NewButton( Toolbar, C ).SetSize( W, 0 ).SetAlign( caLeft ) );
      {$IFDEF USE_NAMES}
      //B.Name := 'TB' + Int2Str( Idx+1 );
      {$ENDIF USE_NAMES}
      B.Tabstop := FALSE;
      B.LikeSpeedButton;
      {$IFNDEF TOGRUSH_TOOLBAR_NOMOUSEMOVE}
      B.OnMouseMove := TOnMouse( MakeMethod( nil, @ ToGR_ButtonMouseMove ) );
      {$ENDIF}
      B.Transparent := TRUE;
      if IsSep then B.Enabled := FALSE;
      if B.GetWindowHandle <> 0 then
      begin
        D := i   or Integer( IsSep ) shl 16
                 or Integer( IsCheck ) shl 17
                 or Integer( Checked ) shl 18
                 or Integer( IsDropDown ) shl 19
                 ;
        SetProp( B.Handle, 'GRBTN', D );
      end;
      SetProp( B.Handle, 'BTNID', BtnID );
      B.OnClick := TOnEvent( MakeMethod( nil, @ ToGR_ClickToolbarBtn ) );
      if Bmp <> nil then
      begin
        B.All_GlyphItemX := idx;
        B.All_GlyphItemY := 0;
        B.All_GlyphBitmap := Bmp;
        B.All_GlyphWidth := ImgW;
        B.All_GlyphHeight := ImgH;
        //B.All_GlyphAttached := TRUE;
        {$IFDEF TOGRUSH_AUTO_DISIMAGES}
        B.Dis_GlyphItemX := idx;
        B.Dis_GlyphItemY := 1;
        B.All_GlyphBitmap := Bmp;
        B.All_GlyphWidth := ImgW;
        B.All_GlyphHeight := ImgH;
        {$ENDIF}
        if not IsDropDown and (C = '') then
          B.All_GlyphHAlign := haCenter;
      end;
      {$IFNDEF TOGRUSH_NO_AUTO_SIZE_BTNS}
        B.aAutoSzX := 10 + ImgW;
        if ImgW > 0 then inc( B.aAutoSzX, 5 );
        if IsDropDown then inc( B.aAutoSzX, 10 );
        B.AutoSize( TRUE );
      {$ENDIF}
      if IsDropDown then
      begin
        {$IFDEF TOGRUSH_DROPBTN2}
        B2 := Pointer( NewButton( Result, C ).SetSize( 5 + 8, 0 ).SetAlign( caLeft ) );
        {$ELSE}
        //B.AutoSize( FALSE );
        //B.Width := W + 13;
        B.All_TextHAlign := haLeft;
        B.Border := 2;
        B2 := Pointer( NewButton( B, C ).SetSize( 5 + 8, 0 ).SetAlign( caRight ) );
        {$ENDIF}
        {$IFDEF USE_NAMES}
        //B2.Name := 'TB_dd' + Int2Str( Idx+1 );
        {$ENDIF USE_NAMES}
        B2.Tabstop := FALSE;
        B2.LikeSpeedButton;
        B2.Transparent := TRUE;
        PGrushControl( B2 ).All_BorderWidth := 0;
        PGrushControl( B2 ).Over_BorderWidth := 1;
        if B2.GetWindowHandle <> 0 then
        begin
          D := i   or Integer( IsSep ) shl 16
                   or Integer( IsCheck ) shl 17
                   or Integer( Checked ) shl 18
                   or Integer( IsDropDown ) shl 19
                   or IS_DRDWN shl 16;
          SetProp( B2.Handle, 'GRBTN', D );
        end;
        B2.OnClick := TOnEvent( MakeMethod( nil, @ ToGR_ClickToolbarBtnDD ) );
        if DrDownBmp = nil then
        begin
          DrDownBmp := NewDIBBitmap( 5, High( DD_img )+1, pf1bit );
          DrDownBmp.DIBPalEntries[ 0 ] := $686868;
          DrDownBmp.DIBPalEntries[ 1 ] := $FFFFFF;
          for y := 0 to High( DD_img ) do
          begin
            DD_dst := DrDownBmp.ScanLine[ y ];
            DD_dst^ := not DD_img[ y ];
          end;
          B2.All_GlyphBitmap := DrDownBmp;
        end
          else
        begin
          B2.All_GlyphBitmap := DrDownBmp;
        end;
        B2.All_GlyphWidth := 5;
        B2.All_GlyphHeight := High( DD_img )+1;
        B2.All_GlyphHAlign := haCenter;
        B2.All_GlyphVAlign := vaBottom;
      end;
    end;
  end;
  if  Bmp <> nil then
      Bmp.Free;
end;

function ToolbarButtonRect( Toolbar: PControl; BtnID: Integer ): TRect;
var i: Integer;
    B: PControl;
begin
  {$IFDEF TOGRUSH_OPTIONAL}
  if NoGrush then
  begin
    Result := Kol.ToolbarButtonRect( Toolbar, BtnID );
    Exit;
  end;
  {$ENDIF TOGRUSH_OPTIONAL}
  for i := 0 to Toolbar.ChildCount-1 do
  begin
    B := Toolbar.Children[ i ];
    if (B.GetWindowHandle <> 0) and
       (Integer( GetProp( B.Handle, 'BTNID' ) ) = BtnID) then
    begin
      Result := B.BoundsRect;
      Exit;
    end;
  end;
  Result := MakeRect( 0, 0, 0, 0 );
end;

procedure ToolbarSetTooltips( Toolbar: PControl; BtnID1st: Integer; const Tooltips: array of PKOLChar );
{$IFDEF USE_MHTOOLTIP}
var i, j: Integer;
    B: PControl;
    found: Boolean;
{$ENDIF}
begin
  Toolbar.TBSetTooltips( BtnID1st, Tooltips );
  {$IFDEF TOGRUSH_OPTIONAL}
  if NoGrush then Exit;
  {$ENDIF TOGRUSH_OPTIONAL}
  {$IFDEF USE_MHTOOLTIP}
  found := FALSE;
  j := 0;
  if BtnID1st < 0 then BtnID1st := 0;
  for i := 0 to Toolbar.ChildCount-1 do
  begin
    if j > High( Tooltips ) then break;
    B := Toolbar.Children[ i ];
    //if not B.IsButton then continue;
    if HiWord( GetProp( B.Handle, 'GRBTN' ) ) and IS_DRDWN <> 0 then
      continue;
    if found or (Integer( GetProp( B.Handle, 'BTNID' ) ) = BtnID1st) then
    begin
      found := TRUE;
      if Trim( Tooltips[ j ] ) <> '' then
        NewHint( B ).Text := Tooltips[ j ];
      inc( BtnID1st );
      inc( j );
    end;
  end;
  {$ENDIF USE_MHTOOLTIP}
end;

function ToolbarButtonEnabled( Toolbar: PControl; BtnID: Integer ): Boolean;
var i: Integer;
    B: PControl;
begin
  {$IFDEF TOGRUSH_OPTIONAL}
  if NoGrush then
  begin
    Result := Kol.ToolbarButtonEnabled( Toolbar, BtnID );
    Exit;
  end;
  {$ENDIF TOGRUSH_OPTIONAL}
  for i := 0 to Toolbar.ChildCount-1 do
  begin
    B := Toolbar.Children[ i ];
    if (B.GetWindowHandle <> 0) and
       (Integer( GetProp( B.Handle, 'BTNID' ) ) = BtnID) then
    begin
      Result := B.Enabled;
      Exit;
    end;
  end;
  Result := FALSE;
end;

function ToolbarButtonChecked( Toolbar: PControl; BtnID: Integer): Boolean;
var i: Integer;
    B: PControl;
begin
  {$IFDEF TOGRUSH_OPTIONAL}
  if NoGrush then
  begin
    Result := Toolbar.TBButtonChecked[ BtnID ];
    Exit;
  end;
  {$ENDIF TOGRUSH_OPTIONAL}
  for i := 0 to Toolbar.ChildCount-1 do
  begin
    B := Toolbar.Children[ i ];
    if (B.GetWindowHandle <> 0) and
       (Integer( GetProp( B.Handle, 'BTNID' ) ) = BtnID) then
    begin
      Result := B.Checked;
      Exit;
    end;
  end;
  Result := FALSE;
end;

procedure ToolbarButtonSetChecked( Toolbar: PControl; BtnID: Integer; Checked: Boolean );
var i: Integer;
    B: PControl;
begin
  {$IFDEF TOGRUSH_OPTIONAL}
  if NoGrush then
  begin
    Toolbar.TBButtonChecked[ BtnID ] := Checked;
    Exit;
  end;
  {$ENDIF TOGRUSH_OPTIONAL}
  for i := 0 to Toolbar.ChildCount-1 do
  begin
    B := Toolbar.Children[ i ];
    if (B.GetWindowHandle <> 0) and
       (Integer( GetProp( B.Handle, 'BTNID' ) ) = BtnID) then
    begin
      B.Checked := Checked;
      Exit;
    end;
  end;
end;

procedure EnableToolbarButton( Toolbar: PControl; BtnID: Integer; Enable: Boolean );
var i: Integer;
    B: PControl;
begin
  {$IFDEF TOGRUSH_OPTIONAL}
  if NoGrush then
  begin
    Kol.EnableToolbarButton( Toolbar, BtnID, Enable );
    Exit;
  end;
  {$ENDIF TOGRUSH_OPTIONAL}
  for i := 0 to Toolbar.ChildCount-1 do
  begin
    B := Toolbar.Children[ i ];
    if (B.GetWindowHandle <> 0) and
       (Integer( GetProp( B.Handle, 'BTNID' ) ) = BtnID) then
    begin
      B.Enabled := Enable;
      Exit;
    end;
  end;
end;

function ToolbarButtonVisible( Toolbar: PControl; BtnID: Integer ): Boolean;
var i: Integer;
    B: PControl;
begin
  {$IFDEF TOGRUSH_OPTIONAL}
  if NoGrush then
  begin
    Result := Kol.ToolbarButtonVisible( Toolbar, BtnID );
    Exit;
  end;
  {$ENDIF TOGRUSH_OPTIONAL}
  for i := 0 to Toolbar.ChildCount-1 do
  begin
    B := Toolbar.Children[ i ];
    if (B.GetWindowHandle <> 0) and
       (Integer( GetProp( B.Handle, 'BTNID' ) ) = BtnID) then
    begin
      Result := B.Visible;
      Exit;
    end;
  end;
  Result := FALSE;
end;

procedure ShowHideToolbarButton( Toolbar: PControl; BtnID: Integer; Show: Boolean );
var i: Integer;
    B: PControl;
begin
  {$IFDEF TOGRUSH_OPTIONAL}
  if NoGrush then
  begin
    Kol.ShowHideToolbarButton( Toolbar, BtnID, Show );
    Exit;
  end;
  {$ENDIF TOGRUSH_OPTIONAL}
  for i := 0 to Toolbar.ChildCount-1 do
  begin
    B := Toolbar.Children[ i ];
    if (B.GetWindowHandle <> 0) and
       (Integer( GetProp( B.Handle, 'BTNID' ) ) = BtnID) then
    begin
      B.Visible := Show;
      Exit;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// PROGRESS BAR
////////////////////////////////////////////////////////////////////////////////
function NewProgressbar( AParent: PControl ): PControl;
begin
  {$IFDEF TOGRUSH_OPTIONAL}
  if not NoGrush then
  {$ENDIF TOGRUSH_OPTIONAL}
  begin
    Result := Pointer( NewGRushProgressBar( AParent ).SetSize( 300, 20 ) );
    PGRushControl( Result ).Def_BorderRoundWidth := 10;
    PGRushControl( Result ).Def_BorderRoundHeight := 10;
  end
  {$IFDEF TOGRUSH_OPTIONAL}
  else
    Result := Kol.NewProgressbar( AParent )
  {$ENDIF TOGRUSH_OPTIONAL}
  ;
end;

function NewProgressbarEx( AParent: PControl; Options: TProgressbarOptions ): PControl;
begin
  {$IFDEF TOGRUSH_OPTIONAL}
  if not NoGrush then
  {$ENDIF TOGRUSH_OPTIONAL}
    Result := NewProgressbar( AParent )
  {$IFDEF TOGRUSH_OPTIONAL}
  else
    Result := Kol.NewProgressbarEx( AParent, Options );
  {$ENDIF TOGRUSH_OPTIONAL}
  ;
end;

////////////////////////////////////////////////////////////////////////////////
// MessageBox replacement
////////////////////////////////////////////////////////////////////////////////
{$IFNDEF TOGRUSH_NO_MESSAGEBOX}
function MessageBox( Wnd: HWnd; msg, title: PChar; flags: DWORD ): Integer; stdcall;
var Answers: String;
begin
  {$IFDEF TOGRUSH_OPTIONAL}
  if NoGrush then
  begin
    Result := Windows.MessageBox( Wnd, msg, title, flags );
    Exit;
  end;
  {$ENDIF TOGRUSH_OPTIONAL}
  CASE flags and 7 OF
  MB_OK                : Answers := 'OK';
  MB_OKCANCEL          : Answers := 'OK/Cancel';
  MB_ABORTRETRYIGNORE  : Answers := 'Abort/Retry/Ignore';
  MB_YESNOCANCEL       : Answers := 'Yes/No/Cancel';
  MB_YESNO             : Answers := 'Yes/No';
  MB_RETRYCANCEL       : Answers := 'Retry/Cancel';
  END;
  Result := ShowQuestionEx( msg, Answers, nil );
  CASE flags and 7 OF
  MB_OK                : Result := ID_OK;
  MB_OKCANCEL          : if Result <> ID_OK then Result := ID_CANCEL;
  MB_ABORTRETRYIGNORE  : CASE Result OF
                         1: Result := ID_ABORT;
                         2: Result := ID_RETRY;
                         else Result := ID_IGNORE;
                         END;
  MB_YESNOCANCEL       : CASE Result OF
                         1: Result := ID_YES;
                         2: Result := ID_NO;
                         else Result := ID_CANCEL;
                         END;
  MB_YESNO             : CASE Result OF
                         1: Result := ID_YES;
                         else Result := ID_NO;
                         END;
  MB_RETRYCANCEL       : CASE Result OF
                         1: Result := ID_RETRY;
                         else Result := ID_CANCEL;
                         END;
  else Result := 0;
  END;
end;
{$ENDIF}


var SBBrush: HBrush;
function WndProc_RecolorScrollbars( Sender: PControl; var M: TMsg; var Rslt: Integer ): Boolean;
//var OldPaintDC: HDC;
begin
  Result := FALSE;
  CASE M.message OF
  WM_CTLCOLORSCROLLBAR:
    begin
      //SetBkColor( M.wParam, clGRushLighten );
      if SBBrush = 0 then
        SBBrush := CreateSolidBrush( ColorsMix( clGRushLighten, clGRushLight ) );
      Rslt := SBBrush;
      Result := TRUE;
    end;
  {WM_PAINT, WM_PRINT, WM_NCPAINT:
    begin
      Rslt := 0;
      Result := TRUE;
    end;}
  END;
end;

{$IFNDEF TOGRUSH_NO_SCROLLBARS}
type PSBObj = ^TSBObj;
     TSBObj = object( TObj )
       sbar: PControl;
       orientation: TScrollerBar;
       b_up, b_dn, thumb: PGRushControl;
       minpos, maxpos, oldpos, curpos: Integer;
       pagesz, linesz: Integer;
       Timer: PTimer;
       how_scroll_by_timer, cmd_timer: Integer;
       th_click_mouse, th_delta: Integer;
       th_click_curpos: Integer;
       th_clicked: Boolean;
       procedure Init; virtual;
       destructor Destroy; virtual;
       procedure Adjust;
       procedure Resized( Sender: PObj );
       procedure UpClick( Sender: PControl; var Mouse: TMouseEventData );
       procedure DnClick( Sender: PControl; var Mouse: TMouseEventData );
       procedure ThumbClick( Sender: PControl; var Mouse: TMouseEventData );
       procedure ThumbTrack( Sender: PControl; var Mouse: TMouseEventData );
       procedure TimerOff( Sender: PControl; var Mouse: TMouseEventData );
       procedure Release_Capture( Sender: PControl; var Mouse: TMouseEventData );
       procedure Scrolled( cmd: Integer );
       procedure ScrollByTimer( Sender: PObj );
       function WndProc( var M: TMsg; var Rslt: Integer ): Boolean;
     end;

function WndProcScrollbar( Sender: PControl; var M: TMsg; var Rslt: Integer ): Boolean;
var SBObj: PSBObj;
begin
  SBObj :=  Pointer( Sender.CustomObj );
  Result := SBObj.WndProc( M, Rslt );
end;

    function TriangleBitmap( const PtsVert, PtsHorz: array of Integer; Horizontal: Boolean ): PBitmap;
    type TIntArray = array[0..100] of Integer;
         PIntArray = ^TIntArray;
    var Pts: PIntArray;
    begin
      Result := NewDIBBitmap( 8, 8, pf1bit );
      Result.DIBPalEntries[ 1 ] := $FFFFFF;
      Result.Canvas.Brush.Color := clWhite;
      Result.Canvas.FillRect( Result.BoundsRect );
      if Horizontal then Pts := Pointer( @ PtsHorz[ 0 ] )
                    else Pts := Pointer( @ PtsVert[ 0 ] );
      Result.Canvas.Brush.Color := clBlack;
      Result.Canvas.Polygon( [ MakePoint( Pts[0],Pts[1] ),
                               MakePoint( Pts[2],Pts[3] ),
                               MakePoint( Pts[4],Pts[5] ),
                               MakePoint( Pts[6],Pts[7] ) ] );
    end;

    function TriangleUpBitmap( Horizontal: Boolean ): PBitmap;
    begin
      Result := TriangleBitmap( [ 0,5, 3,2, 4,2, 7,5 ],
                                [ 5,0, 2,3, 2,4, 5,7 ], Horizontal );
    end;

    function TriangleDnBitmap( Horizontal: Boolean ): PBitmap;
    begin
      Result := TriangleBitmap( [ 0,2, 3,5, 4,5, 7,2 ],
                                [ 2,0, 5,3, 5,4, 2,7 ], Horizontal );
    end;

function NewScrollBar( AParent: PControl; BarSide: TScrollerBar ): PControl;
var SBObj: PSBObj;
    W, H: Integer;
    Bup, Bdn, Bth: PBitmap;
    R: TRect;

    procedure FillThumbBmp( x, y: Integer );
    var i, dx, dy: Integer;
    begin
      dx := 0;
      dy := 0;
      if BarSide = sbHorizontal then dx := 1
                                else dy := 1;
      for i := 1 to 4 do
      begin
        Bth.Canvas.MoveTo( x, y );
        Bth.Canvas.LineTo( x + dy * 8, y + dx * 8 );
        inc( x, dx * 2 );
        inc( y, dy * 2 );
      end;
    end;

var A: TControlAlign;
begin
  {$IFDEF TOGRUSH_OPTIONAL}
  if not NoGrush then
  {$ENDIF TOGRUSH_OPTIONAL}
  begin
    {Result := KOL.NewScrollBar(AParent, BarSide);
    AParent.AttachProc( @ WndProc_RecolorScrollbars );}
    W := GetSystemMetrics( SM_CXVSCROLL );
    H := GetSystemMetrics( SM_CYHSCROLL );
    Result := KOL.NewPanel( AParent, esNone );
    Result.Border := 0;
    Result.Color := ColorsMix( clGRushLighten, clGRushLight );
    if BarSide = sbHorizontal then
      Result.SetSize( 0, W )
    else
      Result.SetSize( H, 0 );
    new( SBObj, Create );
    Result.CustomObj := SBObj;
    SBObj.sbar := Result;
    SBObj.orientation := BarSide;
    SBObj.maxpos := 100;
    SBObj.pagesz := 1;
    SBObj.linesz := 1;
    SBObj.b_up := NewGRushButton( Result, '' );
    A := caTop; if BarSide = sbHorizontal then A := caLeft;
    SBObj.b_up.SetSize( W, H ).SetAlign( A ).LikeSpeedButton;
    SBObj.b_dn := NewGRushButton( Result, '' );
    A := caBottom; if BarSide = sbHorizontal then A := caRight;
    SBObj.b_dn.SetSize( W, H ).SetAlign( A ).LikeSpeedButton;
    SBObj.thumb := NewGRushButton( Result, '' );
    SBObj.thumb.SetSize( W, H ).LikeSpeedButton;
    Bup := TriangleUpBitmap( BarSide = sbHorizontal );
    Bdn := TriangleDnBitmap( BarSide = sbHorizontal );
    Bth := NewDIBBitmap( 10, 10, pf32bit );
    Bth.Canvas.Pen.Color := SBObj.b_up.Def_ColorFrom;
    FillThumbBmp( 1, 1 );
    Bth.Canvas.Pen.Color := SBObj.b_up.Def_ColorTo;
    FillThumbBmp( 2, 2 );

    Result.OnResize := SBObj.Resized;

    SBObj.b_up.All_GlyphBitmap := Bup; Bup.Free;
    SBObj.b_up.All_GlyphHAlign := haCenter;
    SBObj.b_dn.All_GlyphBitmap := Bdn; Bdn.Free;
    SBObj.b_dn.All_GlyphHAlign := haCenter;
    SBObj.thumb.All_GlyphBitmap := Bth; Bth.Free;
    SBObj.thumb.All_GlyphHAlign := haCenter;
    //SBObj.thumb.All_ContentOffsets := MakeRect( -1, -1, 1, 1 );
    R := MakeRect( 1, 1, -1, -1 );
    SBObj.b_up.All_ContentOffsets := R;
    SBObj.b_dn.All_ContentOffsets := R;
    SBObj.thumb.All_ContentOffsets := R;
    if BarSide = sbHorizontal then SBObj.thumb.Left := W
                              else SBObj.thumb.Top := H;

    SBObj.b_up.OnMouseDown := SBObj.UpClick;
    SBObj.b_dn.OnMouseDown := SBObj.DnClick;
    SBObj.thumb.OnMouseDown := SBObj.ThumbClick;
    SBObj.b_up.OnMouseUp := SBObj.TimerOff;
    SBObj.b_dn.OnMouseUp := SBObj.TimerOff;
    SBObj.thumb.OnMouseUp := SBObj.Release_Capture;
    SBObj.thumb.OnMouseMove := SBObj.ThumbTrack;
    Result.AttachProc( WndProcScrollbar );
    SBObj.Timer := NewTimer( 400 );
    SBObj.Timer.OnTimer := SBObj.ScrollByTimer;
  end
  {$IFDEF TOGRUSH_OPTIONAL}
    else Result := Kol.NewScrollBar( AParent, BarSide )
  {$ENDIF TOGRUSH_OPTIONAL}
  ;
end;

function  Scrollbar_GetMinPos( sb: PControl ): Integer;
var SBObj: PSBObj;
begin
  {$IFDEF TOGRUSH_OPTIONAL}
  if NoGRush then
  begin
    Result := sb.SBMin;
    Exit;
  end;
  {$ENDIF}
  SBObj := Pointer( sb.CustomObj );
  Result := SBObj.minpos;
end;

procedure Scrollbar_SetMinPos( sb: PControl; m: Integer );
var SBObj: PSBObj;
begin
  {$IFDEF TOGRUSH_OPTIONAL}
  if NoGRush then
  begin
    sb.SBMin := m;
    Exit;
  end;
  {$ENDIF}
  SBObj := Pointer( sb.CustomObj );
  SBObj.minpos := m;
  SBObj.Adjust;
end;

procedure Scrollbar_SetAll( sb: PControl; min, max, pg, cur: Integer );
var SBObj: PSBObj;
begin
  {$IFDEF TOGRUSH_OPTIONAL}
  if NoGRush then
  begin
    sb.SBMin := min;
    sb.SBMax := max;
    sb.SBPageSize := pg;
    sb.SBPosition := cur;
    Exit;
  end;
  {$ENDIF}
  SBObj := Pointer( sb.CustomObj );
  SBObj.minpos := min;
  SBObj.maxpos := max;
  SBObj.pagesz := pg;
  SBObj.curpos := cur;
  SBObj.Adjust;
end;

procedure Scrollbar_SetMaxPos( sb: PControl; m: Integer );
var SBObj: PSBObj;
begin
  {$IFDEF TOGRUSH_OPTIONAL}
  if NoGRush then
  begin
    sb.SBMax := m;
    Exit;
  end;
  {$ENDIF}
  SBObj := Pointer( sb.CustomObj );
  SBObj.maxpos := m;
  SBObj.Adjust;
end;

function  Scrollbar_GetMaxPos( sb: PControl ): Integer;
var SBObj: PSBObj;
begin
  {$IFDEF TOGRUSH_OPTIONAL}
  if NoGrush then
  begin
    Result := sb.SBMax;
    Exit;
  end;
  {$ENDIF}
  SBObj := Pointer( sb.CustomObj );
  Result := SBObj.maxpos;
end;

function  Scrollbar_GetCurPos( sb: PControl ): Integer;
var SBObj: PSBObj;
begin
  {$IFDEF TOGRUSH_OPTIONAL}
  if NoGrush then
  begin
    Result := sb.SBPosition;
    Exit;
  end;
  {$ENDIF}
  SBObj := Pointer( sb.CustomObj );
  if SBObj <> nil then
    Result := SBObj.curpos
  else Result := 0;
end;

procedure Scrollbar_SetCurPos( sb: PControl; newp: Integer );
var SBObj: PSBObj;
begin
  {$IFDEF TOGRUSH_OPTIONAL}
  if NoGRush then
  begin
    sb.SBPosition := newp;
    Exit;
  end;
  {$ENDIF}
  SBObj := Pointer( sb.CustomObj );
  SBObj.curpos := newp;
  SBObj.Adjust;
end;

procedure Scrollbar_SetPageSz( sb: PControl; psz: Integer );
var SBObj: PSBObj;
begin
  {$IFDEF TOGRUSH_OPTIONAL}
  if NoGRush then
  begin
    sb.SBPageSize := psz;
    Exit;
  end;
  {$ENDIF}
  SBObj := Pointer( sb.CustomObj );
  SBObj.pagesz := psz;
  SBObj.Adjust;
end;

function  Scrollbar_GetPageSz( sb: PControl ): Integer;
var SBObj: PSBObj;
begin
  {$IFDEF TOGRUSH_OPTIONAL}
  if NoGRush then
  begin
    Result := sb.SBPageSize;
    Exit;
  end;
  {$ENDIF}
  SBObj := Pointer( sb.CustomObj );
  Result := SBObj.pagesz;
end;

procedure Scrollbar_SetLineSz( sb: PControl; lnz: Integer );
var SBObj: PSBObj;
begin
  {$IFDEF TOGRUSH_OPTIONAL}
  if NoGRush then
  begin
    Exit;
  end;
  {$ENDIF}
  SBObj := Pointer( sb.CustomObj );
  SBObj.linesz := lnz;
end;

function  Scrollbar_GetLineSz( sb: PControl ): Integer;
var SBObj: PSBObj;
begin
  {$IFDEF TOGRUSH_OPTIONAL}
  if NoGRush then
  begin
    Result := 1;
    Exit;
  end;
  {$ENDIF}
  SBObj := Pointer( sb.CustomObj );
  Result := SBObj.linesz;
end;

{ TSBObj }

procedure TSBObj.Init;
begin

end;

destructor TSBObj.Destroy;
begin
  Timer.Free;
  inherited;
end;

procedure TSBObj.Adjust;
var total_sz, side_sz, button_sz, thumb_sz, thumb_pos, min_thumb: Integer;
    R: TRect;

    procedure ProvideWindow( ctl: PControl );
    begin
      if {(ctl.Handle = 0) and} (ctl.Width > 0) and (ctl.Height > 0) then
      begin
        ctl.Visible := TRUE;
        ctl.CreateWindow;
      end
        else
        ctl.Visible := FALSE;
    end;
begin
  if orientation = sbHorizontal then
  begin
    total_sz := min( sbar.Width, sbar.Parent.Width );
    side_sz := min( sbar.Height, sbar.Parent.Height );
  end
    else
  begin
    total_sz := min( sbar.Height, sbar.Parent.Height );
    side_sz := min( sbar.Width, sbar.Parent.Width );
  end;
  min_thumb := Max( 4, Min( 8, side_sz ) );
  if total_sz - min_thumb >= 2 * side_sz then
  begin
    button_sz := side_sz;
    dec( total_sz, 2 * side_sz );
  end
    else
  begin
    button_sz := total_sz div 2;
    total_sz := 0;
  end;
  if (total_sz > 8) and (maxpos > minpos) then
  begin
    if minpos < maxpos then
    begin
      thumb_sz := Round( pagesz * total_sz /
                         (maxpos - minpos + pagesz) );
      if (thumb_sz < side_sz) and
         ((total_sz - side_sz) * 10 div (maxpos - minpos) > 1)  then
         thumb_sz := side_sz;
      if thumb_sz < min_thumb then thumb_sz := min_thumb;
    end
    else
      thumb_sz := side_sz;
    if thumb_sz > total_sz then
      thumb_sz := total_sz;
    dec( total_sz, thumb_sz );
  end
    else thumb_sz := 0;
  if total_sz > 0 then
  begin
    if minpos < maxpos then
      thumb_pos := (total_sz{-1}) * (curpos - minpos) div (maxpos - minpos)
    else thumb_pos := 0;
  end
    else thumb_pos := 0;
  if orientation = sbHorizontal then
  begin
    b_up.BoundsRect := MakeRect( 0, 0, button_sz, side_sz );
    b_dn.BoundsRect := MakeRect( sbar.Width - button_sz, 0, sbar.Width, side_sz );
    R := MakeRect( button_sz + thumb_pos, 0,
                   button_sz + thumb_pos + thumb_sz, side_sz );
  end
    else
  begin
    b_up.BoundsRect := MakeRect( 0, 0, side_sz, button_sz );
    b_dn.BoundsRect := MakeRect( 0, sbar.Height - button_sz, side_sz, sbar.Height );
    R := MakeRect( 0, button_sz + thumb_pos, side_sz,
                   button_sz + thumb_pos + thumb_sz );
  end;
  if not RectsEqual( R, thumb.BoundsRect ) then
  begin
    thumb.BoundsRect := R;
    if Assigned( sbar.OnSBScroll ) then
      sbar.OnSBScroll( sbar, SB_THUMBTRACK );
  end;
  ProvideWindow( b_up );
  ProvideWindow( b_dn );
  ProvideWindow( thumb );
  sbar.Update;
end;

procedure TSBObj.DnClick(Sender: PControl; var Mouse: TMouseEventData);
begin
  how_scroll_by_timer := linesz;
  cmd_timer := SB_LINERIGHT;
  ScrollByTimer( nil );
  Timer.Interval := 400;
  Timer.Enabled := TRUE;
end;

procedure TSBObj.Release_Capture( Sender: PControl; var Mouse: TMouseEventData );
begin
  th_clicked := FALSE;
  ReleaseCapture;
end;

procedure TSBObj.Resized(Sender: PObj);
begin
  Adjust;
end;

procedure TSBObj.ScrollByTimer(Sender: PObj);
begin
  oldpos := curpos;
  inc( curpos, how_scroll_by_timer );
  if curpos < minpos then curpos := minpos;
  if curpos > maxpos then curpos := maxpos;
  Adjust;
  Scrolled( cmd_timer );
  Timer.Interval := 100;
end;

procedure TSBObj.Scrolled( cmd: Integer );
var Allow: Boolean;
begin
  Allow := TRUE;
  if Assigned( sbar.OnSBBeforeScroll ) then
    sbar.OnSBBeforeScroll( sbar, oldpos, curpos, cmd, Allow );
  if Assigned( sbar.OnSBScroll ) then
    sbar.OnSBScroll( sbar, cmd )
  else
  if Assigned( sbar.OnScroll ) then
    sbar.OnScroll( sbar, orientation, cmd, curpos );
end;

procedure TSBObj.ThumbClick(Sender: PControl; var Mouse: TMouseEventData);
var P: TPoint;
begin
  SetCapture( thumb.Handle );
  th_clicked := TRUE;
  P := thumb.Client2Screen( MakePoint( Mouse.X, Mouse.Y ) );
  if orientation = sbHorizontal then
  begin
    th_click_mouse := P.X;
    th_delta := -Mouse.X;
  end
  else
  begin
    th_click_mouse := P.Y;
    th_delta := -Mouse.Y;
  end;
  th_click_curpos := curpos
end;

procedure TSBObj.ThumbTrack(Sender: PControl; var Mouse: TMouseEventData);
var new_pos, new_top, total_sz, button_sz, thumb_sz: Integer;
    P: TPoint;
begin
  if not th_clicked then Exit;
  oldpos := curpos;
  P := Sender.Client2Screen( MakePoint( Mouse.X, Mouse.Y ) );
  P := sbar.Screen2Client(P);
  if orientation = sbHorizontal then
  begin
    new_top := P.X;
    button_sz := b_up.Width;
    thumb_sz := thumb.Width;
    total_sz := sbar.Width;
  end
  else
  begin
    new_top := P.Y;
    button_sz := b_up.Height;
    thumb_sz := thumb.Height;
    total_sz := sbar.Height;
  end;
  new_top := new_top - button_sz + th_delta;
  dec( total_sz, 2 * button_sz + thumb_sz );
  if total_sz > 0 then
    new_pos := minpos + (maxpos - minpos) * new_top div total_sz
  else
    new_pos := 0;
  if new_pos < minpos then new_pos := minpos;
  if new_pos > maxpos then new_pos := maxpos;
  curpos := new_pos;
  Adjust;
  Scrolled( SB_THUMBTRACK );
end;

procedure TSBObj.TimerOff(Sender: PControl; var Mouse: TMouseEventData);
begin
  Timer.Enabled := FALSE;
end;

procedure TSBObj.UpClick(Sender: PControl; var Mouse: TMouseEventData);
begin
  how_scroll_by_timer := -linesz;
  cmd_timer := SB_LINELEFT;
  ScrollByTimer( nil );
  Timer.Interval := 400;
  Timer.Enabled := TRUE;
end;

function TSBObj.WndProc(var M: TMsg; var Rslt: Integer): Boolean;
var X, Y: Integer;
  procedure CalcScrollDirAndStep;
  begin
    how_scroll_by_timer := 0;
   if orientation = sbHorizontal then
   begin
     X := SmallInt( LoWord( M.lParam ) );
     if X < thumb.Left then
       how_scroll_by_timer := -pagesz
     else
     if X > thumb.Left + thumb.Width then
       how_scroll_by_timer := pagesz
     else Exit;
   end
     else
   begin
      Y := SmallInt( HiWord( M.lParam ) );
      if Y < thumb.Top then
        how_scroll_by_timer := -pagesz
      else
      if Y > thumb.Top + thumb.Height then
        how_scroll_by_timer := pagesz
      else Exit;
   end;

   if how_scroll_by_timer < 0 then
     cmd_timer := SB_PAGELEFT
   else
     cmd_timer := SB_PAGERIGHT;
  end;
begin
  Result := FALSE;
  CASE M.message OF
  WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
    begin
      CalcScrollDirAndStep;
      if (how_scroll_by_timer = 0) then Exit;

      SetCapture( sbar.Handle );
      ScrollByTimer( nil );
      Timer.Interval := 400;
      Timer.Enabled := TRUE;
    end;
  WM_LBUTTONUP:
    begin
      ReleaseCapture;
      Timer.Enabled := FALSE;
    end;
  WM_MOUSEMOVE:
    begin
      if (Timer <> nil) and Timer.Enabled then
        CalcScrollDirAndStep;
    end;
  END;
end;

type
  POverrideScrollbars = ^TOverrideScrollbars;
  TOverrideScrollbars = object( TObj )
    Handling: Boolean;
    Added2List: Boolean;
    VBar, HBar, Grip: PControl;
    Control2Override: PControl;
    procedure ReplacedScrollBar2Original( Sender: PControl; Cmd: Word );
    destructor Destroy; virtual;
    procedure PaintGrip( Sender: PControl; DC: HDC );
  end;

var ListOfOverridenSBars: PList;

{ TOverrideScrollbars }

destructor TOverrideScrollbars.Destroy;
begin
  Control2Override.CustomObj := nil; //?
  if Added2List then
  begin
    ListOfOverridenSBars.Remove( Control2Override );
    if ListOfOverridenSBars.Count = 0 then
    begin
      KillTimer( 0, ListOfOverridenSBars.Tag );
      Free_And_Nil( ListOfOverridenSBars );
    end;
  end;
  inherited;
end;

procedure TOverrideScrollbars.PaintGrip(Sender: PControl; DC: HDC);
var R: TRect;
    P: TPoint;
    F: PControl;
    i, j: Integer;
    C: PCanvas;
begin
  R := Sender.ClientRect;
  C := Sender.Canvas;
  C.FillRect( R );
  P := MakePoint( R.Right, R.Bottom );
  P := Sender.Client2Screen( P );
  F := Sender.ParentForm;
  P := F.Screen2Client( P );
  if (F.ClientWidth - P.X < 16) and
     (F.ClientHeight - P.Y < 16) then
  begin
    for j := 0 to 1 do
    begin
      C.Pen.Color := clWhite;
      if j = 1 then
        C.Pen.Color := clGRushDark;
      for i := 0 to 4 do
      begin
        C.MoveTo( 2+j+i*3, R.Bottom-2 );
        C.LineTo( R.Right-2, 2+j+i*3 );
      end;
    end;
  end;
end;

procedure TOverrideScrollbars.ReplacedScrollBar2Original(Sender: PControl;
  Cmd: Word);
var O: POverrideScrollbars;
    Ctl: PControl;
    Msg: DWORD;
    CmdF: DWORD;
    Wnd: HWnd;
var SI: TScrollInfo;
    Bar: Integer;
    NewPos: Integer;
    i, MaxI: Integer;
    Frozen: Boolean;
begin
  Ctl := Pointer( Sender.Tag );
  O := Pointer( Ctl.CustomObj );
  if O.HBar = Sender then Msg := WM_HSCROLL
                     else Msg := WM_VSCROLL;
  {CASE Cmd OF
  SB_LINEUP,
  SB_LINEDOWN,
  SB_THUMBTRACK:
    Cmd := SB_THUMBPOSITION;
  END;}
  CmdF := Cmd;
  NewPos := Scrollbar_GetCurPos( Sender );
  CASE Cmd OF
  SB_THUMBTRACK, SB_THUMBPOSITION:
    CmdF := Cmd or (NewPos shl 16);
  END;
  if not O.Handling then
  begin
    O.Handling := TRUE;
    Frozen := FALSE;
    TRY
      Wnd := Ctl.Handle;
      // В случае, если не удастся и значение останется прежним, то это
      // контрол, который не понимает внешний WM_xSCROLL с SB_THUMBXXXX
      // (ListView). Тогда пробуем откорректировать позию его скроллбара
      // последовательными командами SB_LINEUP / SB_LINEDOWN
      i := 0;
      MaxI := 10;
      while i < MaxI do
      begin
        inc( i );
        SI.cbSize := Sizeof( SI );
        SI.fMask := SIF_PAGE or SIF_POS or SIF_RANGE or SIF_TRACKPOS;
        if O.HBar = Sender then Bar := SB_HORZ
                           else Bar := SB_VERT;
        GetScrollInfo( Wnd, Bar, SI );
        MaxI := max( MaxI, DWORD(SI.nMax - SI.nMin) div (SI.nPage + 1) );
        if (SI.nPos = NewPos) or
           (SI.nPos < NewPos) and (CmdF = SB_LINEUP) or
           (SI.nPos > NewPos) and (CmdF = SB_LINEDOWN) then break;
        SendMessage( Wnd, Msg, CmdF, 0 );
        if SI.nPos < NewPos then CmdF := SB_LINEDOWN
                            else CmdF := SB_LINEUP;
        if DWORD( Abs( SI.nPos - NewPos ) ) > SI.nPage then
        begin
          if SI.nPos < NewPos then CmdF := SB_PAGEDOWN
                              else CmdF := SB_PAGEUP;
          if not Frozen then
          begin
            Ctl.BeginUpdate;
            Frozen := TRUE;
          end;
        end
          else
        if Frozen then
        begin
          Frozen := FALSE;
          Ctl.EndUpdate;
        end;
      end;
    FINALLY
      O.Handling := FALSE;
      if Frozen then
        Ctl.EndUpdate;
    END;
  end;
end;

procedure WindowScrollbar2GrushScrollbar( Ctl: PControl; SBar: PControl; Bar: DWORD );
var SI: TScrollInfo;
    Wnd: HWnd;
begin
  SI.cbSize := Sizeof( SI );
  SI.fMask := SIF_PAGE or SIF_POS or SIF_RANGE or SIF_TRACKPOS;
  Wnd := Ctl.Handle;
  GetScrollInfo( Wnd, Bar, SI );
  Scrollbar_SetAll( SBar, SI.nMin, SI.nMax - Integer( SI.nPage ) + 1,
                    SI.nPage, SI.nPos );
end;

procedure CheckOverridenSBars( wnd: HWnd; msg, event, time: DWORD ); stdcall;
var i: Integer;
    Control2Override: PControl;
begin
  if ListOfOverridenSBars = nil then Exit;
  for i := 0 to ListOfOverridenSBars.Count-1 do
  begin
    Control2Override := ListOfOverridenSBars.Items[ i ];
    Control2Override.Perform( CM_AUTOSIZE, 0, 0 );
  end;
end;

type TGetScrollbarInfo = function( Wnd: HWnd; Obj: Integer; var Info: TScrollBarInfo ): BOOL;
                         stdcall;
var GetScrollbarInfo: TGetScrollbarInfo;

function WndProcOverrideScrollbars( Sender: PControl; var M: TMsg; var Rslt: Integer ): Boolean;
var O: POverrideScrollbars;
    HasHBar, HasVBar: Boolean;

    function CreateScrollbarReplacement( Ctl: PControl; Direction: TScrollerBar;
      Flag: DWORD; var SBar: PControl ): Boolean;
    var SBI: TScrollBarInfo;
        R: TRect;
        ParentWnd: Hwnd;
        Wnd: HWnd;
        //M: TMsg;
        SBarCtl: PControl;
        wasSBarVisible: Boolean;
        E: Boolean;
        M: THandle;
    begin
      Result := FALSE;
      SBI.cbSize := Sizeof( SBI );
      Wnd := Ctl.Handle;
      if not Assigned( GetScrollbarInfo ) then
      begin
        M := GetModuleHandle( 'user32' );
        GetScrollbarInfo := GetProcAddress( M, 'GetScrollBarInfo' );
      end;
      if GetScrollbarInfo( Wnd, Integer( Flag ), SBI ) and
         (SBI.rcScrollBar.Bottom > 0) and
         (SBI.rcScrollBar.Right > 0) then
      begin
        if not IsWindowVisible( Wnd ) or
           (SBI.rgstate[0] and STATE_SYSTEM_INVISIBLE <> 0) then
        begin
          {if not PeekMessage( M, Ctl.Handle, CM_AUTOSIZE, CM_AUTOSIZE, pm_noremove )
             and (SBar = nil) then
            Ctl.Postmsg( WM_USER+1, 0, 0 );}
        end
        else
        begin
          E := not( (SBI.rgstate[1] and STATE_SYSTEM_UNAVAILABLE <> 0) and
                    (SBI.rgstate[5] and STATE_SYSTEM_UNAVAILABLE <> 0) );

          if (SBI.rcScrollBar.Left < SBI.rcScrollBar.Right) and
             (SBI.rcScrollBar.Top < SBI.rcScrollBar.Bottom) then
          begin
            Result := TRUE;
            if Wnd = Ctl.Handle then ParentWnd := Ctl.Parent.Handle
            else begin
                   ParentWnd := Wnd; //GetWindow( Wnd, GW_OWNER );
                   SetWindowLong( Wnd, GWL_STYLE,
                     GetWindowLong( Wnd, GWL_STYLE )
                     or WS_CLIPCHILDREN );
                 end;

            if SBar = nil then
            begin
              SBar := NewScrollBar( Ctl.Parent, Direction );
              if Wnd <> Ctl.Handle then
                SetParent( SBar.Handle, ParentWnd );
              SBar.Tag := DWORD( Ctl );
              SBar.OnSBScroll := O.ReplacedScrollBar2Original;
            end;
            SBarCtl := SBar;
            SBarCtl.RefInc;
            TRY
              R := SBI.rcScrollBar;
              Windows.ScreenToClient( ParentWnd, R.TopLeft );
              Windows.ScreenToClient( ParentWnd, R.BottomRight );
              if not RectsEqual( SBarCtl.BoundsRect, R ) then
                SBarCtl.BoundsRect := R;
              if SBar <> nil then
              begin
              if Wnd = Ctl.Handle then SBarCtl.BringToFront
              else begin
                     SBarCtl.Visible := TRUE;
                     SBarCtl.StayOnTop := TRUE;
                     SBarCtl.BringToFront;
                   end;
              end;
              if E <> SBarCtl.Enabled then
              begin
                SBarCtl.Enabled := E;
                SBarCtl.EnableChildren( E, FALSE );
              end;
            FINALLY
              SBarCtl.RefDec;
            END;
          end;
        end;
      end;
      if not Result and (SBar <> nil) then
      begin
        wasSBarVisible := SBar.Visible;
        SBar.Visible := FALSE;
        if wasSBarVisible then
          Ctl.Invalidate;
        //Free_And_Nil( SBar );
      end;
    end;

var TimerHandle: DWORD;
    R1, R2, RGrip: TRect;
begin
  Result := FALSE;
  CASE M.message OF
  WM_NCPAINT: // нельзя обрабатывать непосредственно: портится участок
              // изображения не-клиентской части (на пересечении скроллов, в
              // нижнем правом углу)
    Sender.Postmsg( CM_AUTOSIZE, 0, 0 );
  WM_SIZE, WM_VSCROLL, WM_HSCROLL, WM_WINDOWPOSCHANGING, WM_WINDOWPOSCHANGED,
  WM_PAINT, CM_AUTOSIZE
  //, WM_MOUSEWHEEL, WM_LBUTTONUP, WM_KEYUP, WM_SYSKEYUP
  :
    if Sender.ToBeVisible then
    begin
      O := Pointer( Sender.CustomObj );
      if not O.Handling then
      begin
        O.Handling := TRUE;
        TRY
          HasHBar := CreateScrollbarReplacement( Sender,
            sbHorizontal, OBJID_HSCROLL, O.HBar );
          HasVBar := CreateScrollbarReplacement( Sender,
            sbVertical, OBJID_VSCROLL, O.VBar );

          if HasHBar then
            WindowScrollbar2GrushScrollbar( Sender,
              O.HBar, SB_HORZ );
          if HasVBar then
            WindowScrollbar2GrushScrollbar( Sender,
              O.VBar, SB_VERT );

          if HasHBar or HasVBar then
          begin
            if not O.Added2List then
            begin
              if ListOfOverridenSBars = nil then
              begin
                ListOfOverridenSBars := NewList;
                TimerHandle := SetTimer( 0, 0, 250, @CheckOverridenSBars );
                ListOfOverridenSBars.Tag := DWORD( TimerHandle );
              end;
              ListOfOverridenSBars.Add( O.Control2Override );
              O.Added2List := TRUE;
            end;
          end;

          if HasHBar and HasVBar then
          begin
            R1 := O.HBar.BoundsRect;
            R2 := O.VBar.BoundsRect;
            RGrip := MakeRect( R2.Left, R1.Top, R2.Right, R1.Bottom );
          end
            else
            RGrip := MakeRect( 0, 0, 0, 0 );
          if (RGrip.Left < RGrip.Right) and
             (RGrip.Top < RGrip.Bottom) then
          begin
            if O.Grip = nil then
              O.Grip := NewPaintbox( Sender.Parent ).MouseTransparent;
            O.Grip.Color := Sender.Parent.Color;
            O.Grip.BoundsRect := RGrip;
            O.Grip.OnPaint := O.PaintGrip;
            O.Grip.BringToFront;
          end
            else
          begin
            if O.Grip <> nil then
              O.Grip.Visible := FALSE;
          end;
        FINALLY
          O.Handling := FALSE;
        END;
      end;
    end;
  END;
end;

procedure OverrideScrollbars( C: PControl );
var O: POverrideScrollbars;
begin
  {$IFDEF TOGRUSH_OPTIONAL}
  if NoGrush then Exit;
  {$ENDIF}
  new( O, Create ); O.Control2Override := C;
  C.CustomObj := O;
  C.AttachProc( WndProcOverrideScrollbars );
end;

{$ENDIF TOGRUSH_NO_SCROLLBARS}

////////////////////////////////////////////////////////////////////////////////
// COMBO BOX
////////////////////////////////////////////////////////////////////////////////
type
  PFixComboButton = ^TFixComboButton;
  TFixComboButton = object( TObj )
    Fixed: Boolean;
    Button: PControl;
    Combo: PControl;
    Form, LB: PControl;
    SzIncrease, TargetSz: Integer;
    TimerAnimation: PTimer;
    ClosedTime: DWORD;
    procedure DoDropDownList;
    procedure LBData( Sender: PControl; Idx, SubItem: Integer;
              var Txt: KOL_String; var ImgIdx: Integer; var State: DWORD;
              var Store: Boolean );
    procedure SelectItemUnderCursor( Sender: PControl; var Mouse: TMouseEventData );
    procedure KeyPressed( Sender: PControl; var Key: KOLChar; Shift: DWORD );
    procedure CloseDropDown( SelectOK: Boolean );
    procedure SelectItemByMouse( Sender: PControl; var Mouse: TMouseEventData );
    procedure SelectItemByMouse2( Sender: PControl; var Mouse: TMouseEventData );
    procedure AnimateDropDown( Sender: PObj );
    function Deactivation( var M: TMsg; var Rslt: Integer ): Boolean;
  end;

{ TFixComboButton }

procedure TFixComboButton.LBData(Sender: PControl; Idx, SubItem: Integer;
  var Txt: KOL_String; var ImgIdx: Integer; var State: DWORD;
  var Store: Boolean);
begin
  Txt := Combo.Items[ Idx ];
end;

procedure TFixComboButton.SelectItemUnderCursor(Sender: PControl;
  var Mouse: TMouseEventData);
var i: Integer;
    P: TPoint;
begin
  if Form = nil then Exit;
  P := MakePoint( Mouse.X, Mouse.Y );
  P := Form.Client2Screen( P );
  P := LB.Screen2Client( P );
  i := LB.LVItemAtPos( Mouse.X, Mouse.Y );
  if i >= 0 then
    LB.LVCurItem := i;
end;

procedure TFixComboButton.KeyPressed(Sender: PControl; var Key: KOLChar;
  Shift: DWORD);
begin
  CASE Key OF
  #13: if LB.LVCurItem >= 0 then CloseDropDown( TRUE );
  #27: CloseDropDown( FALSE );
  END;
end;

procedure TFixComboButton.SelectItemByMouse(Sender: PControl;
  var Mouse: TMouseEventData);
var i: Integer;
    P: TPoint;
begin
  P := MakePoint( Mouse.X, Mouse.Y );
  P := Form.Client2Screen( P );
  P := LB.Screen2Client( P );
  i := LB.LVItemAtPos( P.X, P.Y );
  if i >= 0 then
    LB.LVCurItem := i;
  {if Mouse.Button = mbLeft then
    SetCapture( Form.Handle );}
  {if Mouse.Button = mbLeft then
    if i >= 0 then CloseDropDown( TRUE );}
end;

procedure TFixComboButton.SelectItemByMouse2(Sender: PControl;
  var Mouse: TMouseEventData);
var i: Integer;
    P: TPoint;
begin
  P := MakePoint( Mouse.X, Mouse.Y );
  P := Form.Client2Screen( P );
  P := LB.Screen2Client( P );
  i := LB.LVItemAtPos( P.X, P.Y );
  if i >= 0 then
    LB.LVCurItem := i;
  if Mouse.Button = mbLeft then
    if i >= 0 then CloseDropDown( TRUE );
end;

procedure TFixComboButton.CloseDropDown(SelectOK: Boolean);
var i: Integer;
    F: PControl;
begin
  if TimerAnimation = nil then Exit;
  TimerAnimation.Enabled := FALSE;
  ClosedTime := GetTickCount;
  i := LB.LVCurItem;
  if SelectOK then Combo.CurIndex := i;
  Free_And_Nil( TimerAnimation );
  F := Form;
  Form := nil;
  LB := nil;
  F.Close;
  Combo.Focused := TRUE;
  Applet.ActiveControl := Combo.ParentForm;
  if SelectOK and Assigned( Combo.OnSelChange ) then
    Combo.OnSelChange( Combo );
end;

function TFixComboButton.Deactivation(var M: TMsg; var Rslt: Integer): Boolean;
begin
  Result := FALSE;
  if M.message = WM_KILLFOCUS then
  begin
    CloseDropDown( FALSE );
  end;
end;

procedure TFixComboButton.DoDropDownList;
var R: TRect;
    n, h: Integer;
begin
  if Assigned( Combo.OnDropDown ) then
    Combo.OnDropDown( Combo );
  R := Combo.BoundsRect;
  if Combo.DroppedWidth > 0 then
    R.Right := R.Left + Combo.DroppedWidth;
  Windows.ClientToScreen( Combo.ParentWindow, R.TopLeft );
  Windows.ClientToScreen( Combo.ParentWindow, R.BottomRight );
  {$IFDEF USE_DROPDOWNCOUNT}
  n := Combo.DropDownCount;
  {$ELSE}
  n := 8;
  {$ENDIF}
  if n > Combo.Count then n := Combo.Count;
  if n < 1 then n := 1;

  Form := NewForm( Applet, '' ).SetSize( R.Right - R.Left, 1 );
  h := Combo.Font.FontHeight;
  if h = 0 then h := 16;
  TargetSz := n * (h+1);
  SzIncrease := Max( 6, TargetSz div 5 );
  if ScreenHeight - R.Bottom < n * (h + 1) then
  begin
    SzIncrease := -SzIncrease;
    Form.SetPosition( R.Left, R.Top-1 );
  end
    else
    Form.SetPosition( R.Left, R.Bottom );
  Form.HasBorder := FALSE;
  Form.Border := 0;
  LB := NewListView( Form, lvsDetailNoHeader, [ lvoRowSelect, lvoInfoTip, lvoOwnerData ], nil, nil, nil )
    .SetAlign( caClient );
  LB.Ctl3D := False;
  LB.Color := Combo.Color;
  LB.Font.Assign( Combo.Font );
  LB.LVColAdd( '', taLeft, R.Right - R.Left - 4 );
  LB.OnLVData := LBData;
  LB.MouseTransparent;
  Form.OnMouseMove := SelectItemUnderCursor;
  LB.OnKeyChar := KeyPressed;
  Form.OnMouseDown := SelectItemByMouse;
  Form.OnMouseUp := SelectItemByMouse2;
  LB.LVCount := max( 1, Combo.Count );
  LB.OnMessage := Deactivation;
  OverrideScrollbars( LB );
  TimerAnimation := NewTimer( 20 );
  TimerAnimation.OnTimer := AnimateDropDown;
  TimerAnimation.Enabled := TRUE;
  Form.StayOnTop := TRUE;
  Form.Show;
  n := Combo.CurIndex;
  if n >= 0 then
    LB.LVMakeVisible( n, FALSE );
end;

procedure TFixComboButton.AnimateDropDown(Sender: PObj);
var BR: TRect;
begin
  BR := Form.BoundsRect;
  if SzIncrease < 0 then
    inc( BR.Top, SzIncrease )
  else
    inc( BR.Bottom, SzIncrease );
  if BR.Bottom - BR.Top > TargetSz+2 then
  if SzIncrease < 0 then
    BR.Top := Br.Bottom - TargetSz-2
  else
    BR.Bottom := Br.Top + TargetSz+2;
  if not RectsEqual( Form.BoundsRect, BR ) then
    Form.BoundsRect := BR
  else
    TimerAnimation.Enabled := FALSE;
end;

procedure ClickDropDownCombo( _Self, Sender: PControl );
{$IFNDEF TOGRUSH_NO_WINDOW_SCROLLBARS}
var F: PFixComboButton;
{$ENDIF}
begin
  {$IFDEF TOGRUSH_NO_WINDOW_SCROLLBARS}
  _Self.Perform( CB_SHOWDROPDOWN, 1 - _Self.Perform( CB_GETDROPPEDSTATE, 0, 0 ), 0 );
  {$ELSE}
  F := Pointer( _Self.CustomObj );
  if GetTickCount - F.ClosedTime > 200 then
  begin
    F.Combo := _Self;
    F.DoDropDownList;
  end;
  {$ENDIF}
end;

function WndProcComboToGRush( Sender: PControl; var M: TMsg; var Rslt: Integer ): Boolean;
var wnd: HWnd;
    R: TRect;
    C2: PControl;
    Bdn: PBitmap;
    F: PFixComboButton;
begin
  Result := FALSE;
  CASE M.message OF
  WM_SIZE:
    begin
      F := Pointer( Sender.CustomObj );
      if not F.Fixed then
      begin
        wnd := Sender.Handle;
        if wnd <> 0 then
        begin
          wnd := GetWindow( wnd, GW_CHILD );
          if wnd <> 0 then
            SetWindowLong( wnd, GWL_EXSTYLE,
              GetWindowLong( wnd, GWL_EXSTYLE ) and not WS_EX_CLIENTEDGE );

          Sender.MarginTop := 1;
          Sender.MarginLeft := 1;
          Sender.MarginRight := 1;
          Sender.MarginBottom := 1;
          C2 := NewGRushButton( Sender, '' ).LikeSpeedButton //.SetAlign( caRight )
            .SetSize( 18, 0 );
          Bdn := TriangleDnBitmap( FALSE );
          PGrushControl( C2 ).All_GlyphBitmap := Bdn;
          PGrushControl( C2 ).All_GlyphHAlign := haCenter;
          Bdn.Free;
          C2.OnClick := TOnEvent( MakeMethod( Sender, @ClickDropDownCombo ) );
          C2.BringToFront;
          Sender.Invalidate;
          F.Button := C2;
          F.Fixed := TRUE;
        end;
      end;
      if F.Fixed then
      begin
        C2 := F.Button;
        C2.BringToFront;
        R := Sender.ClientRect;
        R.Left := R.Right - 18;
        C2.BoundsRect := R;
      end;
    end;
  END;
end;

{$IFNDEF TOGRUSH_NO_COMBO_EDIT}
function NewComboBox( AParent: PControl; Options: TComboOptions ): PControl;
var F: PFixComboButton;
begin
  {$IFDEF TOGRUSH_OPTIONAL}
  if not NoGrush then
  {$ENDIF TOGRUSH_OPTIONAL}
  begin
    Result := Kol.NewComboBox( AParent, Options );
    new( F, Create ); Result.CustomObj := F;
    Result.AttachProc( WndProcComboToGRush );
  end
  {$IFDEF TOGRUSH_OPTIONAL}
    else Result := Kol.NewComboBox( AParent, Options )
  {$ENDIF TOGRUSH_OPTIONAL}
  ;
end;
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
// EDIT BOX
////////////////////////////////////////////////////////////////////////////////

{$IFNDEF TOGRUSH_NO_COMBO_EDIT}
function NewEditBox( AParent: PControl; Options: TEditOptions ): PControl;
begin
  {$IFDEF TOGRUSH_OPTIONAL}
  if not NoGrush then
  {$ENDIF TOGRUSH_OPTIONAL}
  begin
    Result := Kol.NewEditBox( AParent, Options );
    Result.Ctl3D := FALSE;
  end
  {$IFDEF TOGRUSH_OPTIONAL}
    else Result := Kol.NewEditBox( AParent, Options )
  {$ENDIF TOGRUSH_OPTIONAL}
  ;
end;
{$ENDIF}

{$IFNDEF TOGRUSH_NO_GRADIENTPANEL}
function NewGradientPanel( AParent: PControl; Color1, Color2: TColor ): PControl;
var G: PGRushControl;
begin
  {$IFDEF TOGRUSH_OPTIONAL}
  if not NoGrush then
  {$ENDIF TOGRUSH_OPTIONAL}
  begin
    G := NewGRushPanel( AParent );
    Result := PControl( G );
    G.Def_ColorFrom := Color1;
    G.Def_ColorTo := Color2;
    G.Def_BorderWidth := 0;
    G.Def_BorderRoundWidth := 0;
    G.Def_BorderRoundHeight := 0;
  end
  {$IFDEF TOGRUSH_OPTIONAL}
    else Result := Kol.NewGradientPanel( AParent, Color1, Color2 );
  {$ENDIF TOGRUSH_OPTIONAL}
  ;
end;

function NewGradientPanelEx( AParent: PControl; Color1, Color2: TColor;
                             Style: TGradientStyle; Layout: TGradientLayout ): PControl;
var G: PGRushControl;
begin
  {$IFDEF TOGRUSH_OPTIONAL}
  if not NoGrush then
  {$ENDIF TOGRUSH_OPTIONAL}
  begin
    G := NewGRushPanel( AParent );
    Result := PControl( G );
    G.Def_ColorFrom := Color1;
    G.Def_ColorTo := Color2;
    G.Def_BorderWidth := 0;
    G.Def_BorderRoundWidth := 0;
    G.Def_BorderRoundHeight := 0;
    CASE Layout OF
    glTopLeft : G.Def_GradientStyle := gsFromTopLeft;
    glTop     : G.Def_GradientStyle := gsVertical;
    glTopRight: G.Def_GradientStyle := gsFromTopRight;
    glLeft    : G.Def_GradientStyle := gsHorizontal;
    glCenter  : G.Def_GradientStyle := gsDoubleVert;
    glRight   : begin
                  G.Def_ColorFrom := Color2;
                  G.Def_ColorTo := Color1;
                  G.Def_GradientStyle := gsHorizontal;
                end;
    glBottomLeft: begin
                    G.Def_ColorFrom := Color2;
                    G.Def_ColorTo := Color1;
                    G.Def_GradientStyle := gsFromTopRight;
                  end;
    glBottom  : begin
                  G.Def_ColorFrom := Color2;
                  G.Def_ColorTo := Color1;
                  G.Def_GradientStyle := gsVertical;
                end;
    glBottomRight:
                begin
                  G.Def_ColorFrom := Color2;
                  G.Def_ColorTo := Color1;
                  G.Def_GradientStyle := gsFromTopRight;
                end;
    END;
  end
  {$IFDEF TOGRUSH_OPTIONAL}
    else Result := Kol.NewGradientPanelEx( AParent, Color1, Color2, Style,
      Layout );
  {$ENDIF TOGRUSH_OPTIONAL}
  ;
end;
{$ENDIF}

function FindMenuItemByID( Menu: PMenu; ID: DWORD; var MaxTabulation: Integer ): PMenu;
var i, j, t: Integer;
begin
  Result := nil;
  for j := 0 to Menu.Count-1 do
  begin
    if Menu.ItemHandle[ j ] = ID then
    begin
      Result := Menu.Items[ j ];
      break;
    end;
    if Menu.Count > 0 then
    begin
      Result := FindMenuItemByID( Menu.Items[ j ], ID, MaxTabulation );
      if Result <> nil then break;
    end;
  end;
  if Result <> nil then
  begin
    MaxTabulation := 6;
    Menu := Result.Parent;
    for i := 0 to Menu.Count-1 do
    begin
      t := pos( #9, Menu.ItemText[ i ] );
      if t > MaxTabulation then MaxTabulation := t;
    end;
  end;
end;

var Menubmp: PBitmap;
function OwnerDrawMenuItem( var Msg: TMsg; const Menus: array of PMenu;
         var Rslt: Integer): Boolean;
var Menu, Item: PMenu;
    i, w1, y, m: Integer;
    DC: HDC;
    Sav: DWORD;
    IsCheckItem: Boolean;
    R: TRect;
    MaxTabulation: Integer;
    C: PCanvas;
    s: KOLString;
    Cside: Integer;
    B_Color: TColor;
    DI: PDrawItemStruct;
    MI: PMeasureItemStruct;
    ell: Integer;

    procedure SetupCanvasFont;
    begin
      C := Menubmp.Canvas;
      C.Font.FontName := 'Arial';
      C.Font.FontHeight := Max( 6, GetSystemMetrics( SM_CYMENU ) - 4 );
      C.Font.FontStyle := [ fsBold ];
      C.Font.Color := MenuTextColor;
    end;
begin
  Result := FALSE;
  if (Msg.message = WM_DRAWITEM) and (Msg.wParam = 0) then
  begin
    DI := Pointer( Msg.lParam );
    Item := nil;
    // выбор меню
    for m := 0 to High( Menus ) do
    begin
      Menu := Menus[ m ];
      Item := FindMenuItemByID( Menu, DI.itemID, MaxTabulation );
      if Item <> nil then break;
    end;
    if Item = nil then Exit;
    IsCheckItem := Item.IsCheckItem;
    // выбор цветов
    R := DI.rcItem;
    OffsetRect( R, -R.Left, -R.Top );
    DC := DI.hDC;
    Sav := SaveDC( DC );
    // подготовка изображения
    if (Menubmp <> nil) and (
       (Menubmp.Width < R.Right) or
       (Menubmp.Height < R.Bottom)
       ) then Free_And_Nil( Menubmp );
    Menubmp := NewDibBitmap( R.Right, R.Bottom, pf32bit );

    SetupCanvasFont;
    C.Font.Color := MenuTextColor;
    if not Item.Enabled then
      C.Font.Color := MenuTextDisabled;
    {if WinVer < wvXP then} C.Font.FontQuality := fqAntialiased
                     {else C.Font.FontQuality := fqClearType};

    if DI.itemState and ODS_SELECTED <> 0 then
    begin
      C.Brush.Color := MenuHighlight;
      C.Font.Color := MenuTextHighlight;
      if not Item.Enabled then
      begin
        C.Font.Color := MenuTextDisabSel;
        C.Brush.Color := ColorsMix( C.Brush.Color, clSilver );
      end;
    end
    else
      C.Brush.Color := MenuBackground;
    B_Color := C.Brush.Color;

    C.FillRect( R );
    Cside := R.Bottom - 4;
    if IsCheckItem then
    begin // чек-бокс
      C.Pen.Color := MenuCheckBoxBorder;
      C.Pen.PenWidth := 1;
      C.Brush.Color := MenuCheckBoxBkColor;
      {$IFDEF ROUND_RADIOITEMS}
      if Item.RadioGroup <> 0 then
        C.Ellipse( 2, 2, Cside+2, Cside+2 )
      else
      {$ENDIF ROUND_RADIOITEMS}
      begin
        C.FillRect( MakeRect( 2, 2, Cside+2, Cside+2 ) );
        C.Brush.Color := MenuCheckBoxBorder;
        C.FrameRect( MakeRect( 2, 2, Cside+2, Cside+2 ) );
      end;
      if Item.Checked then
      begin
        {$IFDEF ROUND_RADIOITEMS}
        if Item.RadioGroup <> 0 then
        begin
          C.Pen.Color := MenuCheckBoxCheck;
          C.Brush.Color := MenucheckBoxCheck;
          ell := Max( 2, Min( Cside div 4, Cside-4 ) );
          C.Ellipse( 2+ell, 2+ell, Cside+2-ell, Cside+2-ell );
        end
          else
        {$ENDIF ROUND_RADIOITEMS}
        begin
          C.Pen.Color := MenuCheckBoxCheck;
          C.Pen.PenWidth := 2;
          C.MoveTo( 2 + 1, 2 + Cside div 2  );
          C.LineTo( 2 + Cside div 2, 2 + Cside - 2 );
          C.LineTo( 2 + Cside - 1, 3 );
        end;
      end;
    end;

    C.Brush.Color := B_Color;

    s := Item.Caption;
    if s = '' then
    begin
      C.Brush.Color := MenuLine1Color;
      y := R.Bottom div 2;
      C.FillRect( MakeRect( 2, y, R.Right-2, y + 1 ) );
      C.Brush.Color := MenuLine2Color;
      C.FillRect( MakeRect( 2, y+1, R.Right-2, y+2 ) );
    end
      else
    begin
      s := Parse( s, #9 );
      C.RequiredState( HandleValid or FONTVALID or BrushValid or ChangingCanvas );
      R.Left := Cside + 4;
      R.Top := 1;
      DrawTextEx( C.Handle, PKOLChar( s ), Length( s ),
        R, DT_LEFT or DT_SINGLELINE {$IFDEF RED_ACCELERATORS} or DT_HIDEPREFIX {$ENDIF}, nil );
      {$IFDEF RED_ACCELERATORS}
      i := pos( '&', s );
      if i > 0 then
      begin
        w1 := C.TextWidth( Copy( s, 1, i-1 ) );
        C.DeselectHandles;
        C.Font.Color := MenuAccelColor;
        if not Item.Enabled then
          C.Font.Color := //ColorsMix( C.Font.Color, clSilver );
                          MenuAccelDisabled;
        if DI.itemState and ODS_SELECTED <> 0 then
        begin
          C.Font.Color := MenuAccelSelColor;
          if not Item.Enabled then
            C.Font.Color := //ColorsMix( C.Font.Color, clSilver );
                            MenuAccelSelDisabled;
        end;
        C.TextOut( R.Left + w1, R.Top, Copy( s, i+1, 1 ) );
      end;
      {$ENDIF RED_ACCELERATORS}
      {if s <> '' then w1 := C.TextWidth( 'Abcw' ) div 4
      else} w1 := 10;
      s := Item.Caption;
      Parse( s, #9 );
      if s <> '' then
      begin
        C.Font.Color := MenuHotKeyTextColor;
        if not Item.Enabled then
          C.Font.Color := //ColorsMix( C.Font.Color, clSilver );
                          MenuHotKeyTxDisabled;
        if DI.itemState and ODS_SELECTED <> 0 then
        begin
          C.Font.Color := MenuHotKeySelTxColor;
          if not Item.Enabled then
            C.Font.Color := //ColorsMix( C.Font.Color, clSilver );
                            MenuHotKeySelTxDisabled;
        end;
        C.Brush.BrushStyle := bsClear;
        C.TextOut( (Cside + 4) + w1 * MaxTabulation, 1, s );
        C.Brush.BrushStyle := bsSolid;
      end;
    end;
    //Menubmp.SaveToFile( GetStartDir + 'test_custom_menu.bmp' );
    R := DI.rcItem;
    //C.DeselectHandles;
    RestoreDC( DC, Sav );
    BitBlt( DC, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top,
            Menubmp.Canvas.Handle, 0, 0, SRCCOPY );
    //SetBkColor( DC, clGRushNormal );

    //Result := TRUE;
    Rslt := 1;
  end
    else
  if Msg.message = WM_MEASUREITEM then
  begin
    MI := Pointer( Msg.lParam );
    if MI.CtlType <> ODT_MENU then Exit;
    //Result := FALSE;
    Item := nil;
    // выбор меню
    for i := 0 to High( Menus ) do
    begin
      Menu := Menus[ i ];
      // выбор элемента
      Item := FindMenuItemByID( Menu, MI.itemID, MaxTabulation );
      if Item <> nil then break;
    end;
    if Item = nil then Exit;
    // Вычисление размера элемента
    if Menubmp = nil then
      Menubmp := NewDibBitmap( 1, 1, pf32bit );
    SetupCanvasFont;
    s := Item.Caption;
    s := Parse( s, #9 );
    w1 := 10;
    if s <> '' then
      w1 := max( C.TextWidth(s), MaxTabulation * w1 )
    else
      w1 := max( MaxTabulation, 8 ) * w1;
    s := Item.Caption;
    Parse( s, #9 );
    if Item.Caption <> '' then
      MI.itemWidth := 20 + w1 + C.TextWidth(s)
    else
      MI.itemWidth := 20 + w1;

    if Item.Caption <> '' then
      MI.ItemHeight := Menubmp.Canvas.TextHeight( Item.Caption )+2
    else
      MI.itemHeight := 6;

    Result := TRUE;
    Rslt := 1;
  end;
end;

initialization
  KOL.OverrideScrollbars := OverrideScrollbars;

finalization

  Free_And_Nil( DrDownBmp );
  {$IFNDEF TOGRUSH_NO_SCROLLBARS}
  if SBBrush <> 0 then
    DeleteObject( SBBrush );
  {$ENDIF}
  Free_And_Nil( Menubmp );

end.
