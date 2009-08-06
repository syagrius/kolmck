unit KOLProgBar;

interface

uses
  Windows, Messages, KOL;

type

  TBevel = (bvUp, bvDown, bvNone);

  PColorProgBar =^TColorProgBar;
  TColorProgressBar = PColorProgBar;
  TColorProgBar = object(TObj)
  private
    { Private declarations }
    fControl : PControl;
    fPosition: integer;
    fOldPosit: integer;
    fBColor,
    fFColor  : TColor;
    fFirst   : boolean;
    fBorder  : integer;
    fParentCl: boolean;
    fBevel   : TBevel;
    fMin,
    fMax     : integer;
    fStr     : string;
    fFont    : PGraphicTool;
    fCanvas  : PCanvas;
    OldWind,
    NewWind  : longint;
    procedure SetFColor(C: TColor);
    procedure SetBColor(C: TColor);
    procedure SetPos(P: integer);
    procedure SetBorder(B: integer);
    procedure SetParentCl(B: boolean);
    procedure SetBevel(B: TBevel);
    procedure SetMin(M: integer);
    procedure SetMax(M: integer);
  protected
    { Protected declarations }
    procedure NewWndProc(var Msg: TMessage);
    procedure Paint;
{    procedure WMPaint(var Msg: TMessage); message WM_PAINT;
    procedure WMSize (var Msg: TMessage); message WM_SIZE;
    procedure WMActiv(var Msg: TMessage); message WM_SHOWWINDOW;
    procedure CMParCl(var Msg: TMessage); message CM_PARENTCOLORCHANGED;}
  public
    destructor Destroy; virtual;
    function  SetPosition(X, Y: integer): PColorProgBar; overload;
    function  SetSize(X, Y: integer): PColorProgBar; overload;
    function  SetAlign(A: TControlAlign): PColorProgBar; overload;
    function  GetFont: PGraphicTool;
    { Public declarations }
{    constructor Create(Owner: TControl); override;}
    property Font: PGraphicTool read GetFont;
    property FColor: TColor read fFColor write SetFColor;
    property BColor: TColor read fBColor write SetBColor;
    property Border: integer read fBorder write SetBorder;
    property Position: integer read fPosition write SetPos;
    property Max: integer read fMax write SetMax;
    property Min: integer read fMin write SetMin;
    property ParentColor: boolean read fParentCl write SetParentCl;
    property Bevel: TBevel read fBevel write SetBevel;
  end;

function NewTColorProgressBar(AOwner: PControl): PColorProgBar;

implementation

uses objects;

function NewTColorProgressBar;
var p: PColorProgBar;
    c: PControl;
begin
{   New(Result, Create);}
   c := pointer(_NewControl( AOwner, 'STATIC', WS_VISIBLE or WS_CHILD or
                 SS_LEFTNOWORDWRAP or SS_NOPREFIX or SS_NOTIFY,
                 False, nil ));
   c.CreateWindow;
   New(p, create);
   AOwner.Add2AutoFree(p);
   p.fControl  := c;
   p.fFont     := NewFont;
   p.fCanvas   := NewCanvas(GetDC(c.Handle));
   p.fMin      := 0;
   p.fMax      := 100;
   p.fFColor   := clRed;
   p.fBColor   := clBtnFace;
   p.fBorder   := 4;
   p.fBevel    := bvDown;
   p.fFirst    := True;
   p.fPosition := 50;
   p.fFont.FontStyle := [fsBold];
   Result  := p;
   p.OldWind := GetWindowLong(c.Handle, GWL_WNDPROC);
   p.NewWind := LongInt(MakeObjectInstance(p.NewWndProc));
   SetWindowLong(c.Handle, GWL_WNDPROC, p.NewWind);
end;

destructor TColorProgBar.Destroy;
begin
   SetWindowLong(fControl.Handle, GWL_WNDPROC, OldWind);
   FreeObjectInstance(Pointer(NewWind));
   fCanvas.Free;
   fFont.Free;
   inherited;
end;

function TColorProgBar.SetPosition(X, Y: integer): PColorProgBar;
begin
   fControl.Left := X;
   fControl.Top := Y;
   Result := @self;
end;

function TColorProgBar.SetSize(X, Y: integer): PColorProgBar;
begin
   fControl.Width  := X;
   fControl.Height := Y;
   Result := @self;
end;

function TColorProgBar.SetAlign(A: TControlAlign): PColorProgBar;
begin
   fControl.Align := A;
   Result := @self;
end;

function TColorProgBar.GetFont;
begin
   Result := fFont;
end;


procedure TColorProgBar.NewWndProc;
begin
   Msg.Result := CallWindowProc(Pointer(OldWind), fControl.Handle, Msg.Msg, Msg.wParam, Msg.lParam);
   case Msg.Msg of
WM_PAINT: Paint;
WM_SIZE: begin
            fFirst := True;
            Paint;
         end;
WM_ACTIVATE:
         begin
            fFirst := True;
            Paint;
         end;
{CM_PARENTCOLORCHANGED:
         begin
            if fParentCl then begin
               if Msg.wParam <> 0 then
               BColor := TColor(Msg.lParam) else
               BColor := (Parent as TForm).Color;
               FColor := (Parent as TForm).Font.Color;
            end;
         end;}
   end;
end;

procedure TColorProgBar.SetFColor;
begin
   fFColor   := C;
   fFirst  := True;
   Paint;
end;

procedure TColorProgBar.SetBColor;
begin
   fBColor := C;
   fFirst  := True;
   Paint;
end;

procedure TColorProgBar.SetPos;
begin
   fPosition := P;
   Paint;
end;

procedure TColorProgBar.SetBorder;
begin
   fBorder := B;
   fFirst := True;
   Paint;
end;

procedure TColorProgBar.SetParentCl;
begin
   fParentCl := B;
   if B then begin
{      Perform(CM_PARENTCOLORCHANGED, 0, 0);}
      Paint;
   end;
end;

procedure TColorProgBar.SetBevel;
begin
   fBevel := B;
   fFirst := True;
   Paint;
end;

procedure TColorProgBar.SetMin;
begin
   fMin := M;
   fFirst := True;
   if fMax = fMin then fMax := fMin + 1;
   Paint;
end;

procedure TColorProgBar.SetMax;
begin
   fMax := M;
   fFirst := True;
   if fMin = fMax then fMin := fMax - 1;
   Paint;
end;

procedure Frame3D(Canvas: PCanvas; var Rect: TRect; TopColor, BottomColor: TColor;
  Width: Integer);

  procedure DoRect;
  var
    TopRight, BottomLeft: TPoint;
  begin
    with Canvas^, Rect do
    begin
      TopRight.X := Right;
      TopRight.Y := Top;
      BottomLeft.X := Left;
      BottomLeft.Y := Bottom;
      Pen.Color := TopColor;
      PolyLine([BottomLeft, TopLeft, TopRight]);
      Pen.Color := BottomColor;
      Dec(BottomLeft.X);
      PolyLine([TopRight, BottomRight, BottomLeft]);
    end;
  end;

begin
  Dec(Rect.Bottom); Dec(Rect.Right);
  while Width > 0 do
  begin
    Dec(Width);
    DoRect;
    InflateRect(Rect, -1, -1);
  end;
  Inc(Rect.Bottom); Inc(Rect.Right);
end;

function ColorToRGB(Color: TColor): Longint;
begin
  if Color < 0 then
    Result := GetSysColor(Color and $000000FF) else
    Result := Color;
end;

procedure TColorProgBar.Paint;
var Rct: TRect;
    Trc: TRect;
    Twk: TRect;
    Str: string;
    Rht: integer;
    Len: integer;
    Rgn: HRgn;
    Stw: integer;
begin
   GetClientRect(fControl.Handle, Rct);
   Trc := Rct;
   if (fPosition <= fOldPosit) or fFirst then begin
      case fBevel of
  bvUp: begin
           Frame3D(fCanvas, Rct, clWhite, clBlack, 1);
        end;
bvDown: begin
           Frame3D(fCanvas, Rct, clBlack, clWhite, 1);
        end;
      end;

      fFirst := False;
      fCanvas.brush.Color := fBColor;
      fCanvas.FillRect(Rct);
   end;
   Rct    := Trc;

   InflateRect(Rct, -fBorder, -fBorder);
   Rct.Right  := Rct.Left + (Rct.Right - Rct.Left) * fPosition div (Max - Min);

   Str := ' ' + int2str(fPosition * 100 div (fMax - fMin)) + '% ';

   SelectObject(fCanvas.Handle, fFont.Handle);
   Stw       := fCanvas.TextWidth(Str);
   Trc.Left  := (fControl.width - Stw) div 2;
   Trc.Right := (fControl.width + Stw) div 2 + 1;
   Twk       := Rct;

   fCanvas.brush.Color := fFColor;
   if (Rct.Right <= Trc.Left) then begin
      fCanvas.FillRect(Rct);
   end else begin
      Twk.Right := Trc.Left;
      fCanvas.FillRect(Twk);
   end;

   Rht := Rct.Right;
   Len := Length(Str);

   Rct.Left  := (fControl.width - Stw) div 2;
   Rct.Right := (fControl.width + Stw) div 2 + 1;

   if fStr <> Str then begin
      if (Rct.Right > Rht) or (fCanvas.TextHeight(Str) > (Rct.Bottom - Rct.Top)) then begin
         Rgn := CreateRectRgn(Rht, Rct.Top, Rct.Right, Rct.Bottom);
         SelectClipRgn(fCanvas.Handle, Rgn);
         SelectObject(fCanvas.Handle, fFont.Handle);
         SetBkColor(fCanvas.Handle, ColorToRGB(fBColor));
         SetTextColor(fCanvas.Handle, ColorToRGB(fFColor));
         DrawText(fCanvas.Handle, @Str[1], Len, Rct, DT_TOP or DT_NOCLIP);
         SelectClipRgn(fCanvas.Handle, 0);
         DeleteObject(Rgn);
      end;
   end;

   if Rht < Rct.Right then begin
      Rct.Right := Rht;
   end;

   Dec(Rct.Left);
   Inc(Rct.Right);

   if (Rct.Right > Rct.Left) then begin
      SelectObject(fCanvas.Handle, fFont.Handle);
      SetBkColor(fCanvas.Handle, ColorToRGB(fFColor));
      SetTextColor(fCanvas.Handle, ColorToRGB(fBColor));
      DrawText(fCanvas.Handle, @Str[1], Len, Rct, DT_TOP);
      if Rct.Right < Trc.Right then begin
         Twk := Rct;
         Twk.Top := Twk.Top + fCanvas.TextHeight(Str);
         fCanvas.brush.Color := fFColor;
         fCanvas.Fillrect(Twk);
      end;
   end;

   if (Rct.Right >= Trc.Right) then begin
      Rct.Left  := Trc.Right - 2;
      Rct.Right := Rht;
      SetBkColor(fCanvas.Handle, ColorToRGB(fFColor));
      fCanvas.FillRect(Rct);
   end;

   fStr := Str;
   fOldPosit := fPosition;
end;

end.
