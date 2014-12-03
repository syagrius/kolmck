//by Roman Vorobets
//
// В mckCtrls y TKOLButton,TKOLLabel,TEditbox,TCheckBox и TRadioBox нyжно добавить метод
//
//TKOL#####=class(TKOLControl)
//...
//protected
//  procedure Paint;override;
//...
//end;
//
//...
//
//procedure TKOL#####.Paint;
//begin
//  Draw#####(self,canvas);
//end;
//dufa
unit mckCtrlDraw;

interface

uses
  Windows, Graphics, mirror, mckCtrls, ExtCtrls, Classes, Themes;

procedure DrawButton(Sender: TKOLButton; aCanvas: TCanvas);
procedure DrawEditBox(Sender: TKOLEditBox; aCanvas: TCanvas);
procedure DrawMemo(Sender: TKOLMemo; aCanvas: TCanvas);
procedure DrawCheckbox(Sender: TKOLCheckbox; aCanvas: TCanvas);
procedure DrawRadiobox(_Radiobox: TKOLRadiobox; Canvas: TCanvas);
procedure DrawCombobox(_Combobox: TKOLCombobox; Canvas: TCanvas);
procedure DrawLabel(_Label: TKOLLabel; Canvas: TCanvas);

implementation

const
  TextHFlags: array[TTextAlign] of DWORD     = (DT_LEFT, DT_RIGHT,   DT_CENTER);
  TextVFlags: array[TVerticalAlign] of DWORD = (DT_TOP,  DT_VCENTER, DT_BOTTOM);
  WordWrapFlags: array[Boolean] of DWORD     = (DT_SINGLELINE, 0);//!
  CheckFlags: array[Boolean] of DWORD        = (0, DFCS_CHECKED);

procedure DrawButton(Sender: TKOLButton; aCanvas: TCanvas);
var
  e: TThemedButton;
  d: TThemedElementDetails;
begin
  // states
  if Sender.Enabled then begin
    if Sender.DefaultBtn then
      e := tbPushButtonDefaulted
    else
      e := tbPushButtonNormal
  end else
    e := tbPushButtonDisabled;
  // get element
  d := ThemeServices.GetElementDetails(e);
  // draw element, text
  ThemeServices.DrawElement(aCanvas.Handle, d, Sender.ClientRect, nil);
  ThemeServices.DrawText(aCanvas.Handle, d, Sender.Caption, Sender.ClientRect,
    TextHFlags[Sender.TextAlign] or TextVFlags[Sender.VerticalAlign] or DT_SINGLELINE, 0);
end;

procedure DrawEditBox(Sender: TKOLEditBox; aCanvas: TCanvas);
var
  e:  TThemedEdit;
  d:  TThemedElementDetails;
  r:  TRect;
  DC: HDC;
  dw: DWORD;
  ss: AnsiString;
begin
  // states
  if Sender.Enabled then
    e := teEditTextNormal
  else
    e := teEditTextDisabled;
  // get element
  d := ThemeServices.GetElementDetails(e);
  // draw element
  r  := aCanvas.ClipRect;
  DC := aCanvas.Handle;
  ThemeServices.DrawElement(DC, d, r, nil);
  // draw text
  Inc(r.Left, 3);
  Inc(r.Top, 3);
  Dec(r.Right, 3);
  Dec(r.Bottom, 3);
  ss := Sender.Caption;
  dw := Length(ss);
  if (dw > 0) and (eoPassword in Sender.Options) then
    FillChar(ss[1], dw, '*');
  dw := TextHFlags[Sender.TextAlign] or DT_SINGLELINE;
  ThemeServices.DrawText(DC, d, ss, r, dw, 0);
end;

procedure DrawMemo(Sender: TKOLMemo; aCanvas: TCanvas);
var
  e:  TThemedEdit;
  d:  TThemedElementDetails;
  r:  TRect;
  DC: HDC;
  ws: WideString;
begin
  // states
  if Sender.Enabled then
    e := teEditTextNormal
  else
    e := teEditTextDisabled;
  // get element
  d := ThemeServices.GetElementDetails(e);
  // draw element
  r  := aCanvas.ClipRect;
  DC := aCanvas.Handle;
  ThemeServices.DrawElement(DC, d, r, nil);
  // draw text
  Inc(r.Left, 3);
  Inc(r.Top, 3);
  Dec(r.Right, 3);
  Dec(r.Bottom, 3);
  ws := Sender.Caption;
  ThemeServices.DrawText(DC, d, ws, r, TextHFlags[Sender.TextAlign], 0);
end;

procedure DrawLabel(_Label: TKOLLabel; Canvas: TCanvas);
var
  r: trect;
  s: string;
begin
  with _Label, Canvas do begin
    r := clientrect;
    s := caption;
    brush.color := clbtnshadow;
    framerect(r);
    setbkmode(handle, windows.TRANSPARENT);
    drawtext(handle, pchar(s), length(s), r, TextHFlags[textalign] or TextVFlags[verticalalign] or WordWrapFlags[wordwrap]);
  end;
end;

procedure DrawCheckbox(Sender: TKOLCheckbox; aCanvas: TCanvas);
var
  e:  TThemedButton;
  d:  TThemedElementDetails;
  r:  TRect;
  rr: TRect;
  DC: HDC;
  ws: WideString;
begin
  // states
  if Sender.Enabled then begin
    if Sender.Checked then
      e := tbCheckBoxCheckedNormal
    else
      e := tbCheckBoxUncheckedNormal
  end else begin
    if Sender.Checked then
      e := tbCheckBoxCheckedDisabled
    else
      e := tbCheckBoxUncheckedDisabled
  end;
  // get element
  d  := ThemeServices.GetElementDetails(e);
  r  := aCanvas.ClipRect;
  DC := aCanvas.Handle;
  // draw edge
  aCanvas.FillRect(r);
  if Sender.HasBorder then
    ThemeServices.DrawEdge(DC, d, r, EDGE_RAISED, BF_RECT or BF_MIDDLE);
  // draw element
  rr := Bounds(-3, -3, 22, 22);
  ThemeServices.DrawElement(DC, d, rr);
  // draw text
  Inc(r.Left, 18);
  ws := Sender.Caption;
  ThemeServices.DrawText(DC, d, ws, r, DT_LEFT, 0);
end;

procedure DrawRadiobox(_Radiobox: TKOLRadiobox; Canvas: TCanvas);
var
  r, rr: trect;
  s: string;
begin
  with _Radiobox, Canvas do begin
    r := clientrect;
    s := caption;

    {brush.color:=clbtnshadow;
    framerect(r);}
    if _Radiobox.hasborder then
      DrawEdge(Canvas.handle, r, EDGE_RAISED, BF_RECT or BF_MIDDLE);

    rr := bounds(r.left + 2, (r.bottom + r.top - 13) div 2, 13, 13);
    drawframecontrol(handle, rr, DFC_BUTTON,
      DFCS_BUTTONRADIO or CheckFlags[checked]);
    Inc(r.left, 17);
    setbkmode(handle, windows.TRANSPARENT);
    drawtext(handle, pchar(s), length(s), r, DT_VCENTER or DT_SINGLELINE);
  end;
end;

procedure DrawCombobox1(aCombobox: TKOLCombobox; aCanvas: TCanvas; r: trect);
var
  w:  Integer;
  s:  string;
  dw: DWORD;
begin
  if (aCombobox.curindex >= 0) and (aCombobox.curindex < aCombobox.items.count) then
    s := aCombobox.items[aCombobox.curindex]
  else
    s := '';
  if aCombobox.hasborder then begin
    frame3d(aCanvas, r, clbtnshadow, clbtnhighlight, 1);
    frame3d(aCanvas, r, clblack, cl3dlight, 1);
  end;
  if not (coSimple in aCombobox.Options) then begin
    w := getsystemmetrics(SM_CXVSCROLL);
    dw := DFCS_SCROLLCOMBOBOX;
    if not aCombobox.Enabled then
      dw := dw or DFCS_INACTIVE;
    DrawFrameControl(aCanvas.Handle, rect(r.right - w, r.top, r.right, r.bottom), DFC_SCROLL, dw);
    Dec(r.right, w);
  end;
  setbkmode(aCanvas.Handle, windows.TRANSPARENT);
  if (s <> '') then begin
    if aCombobox.Enabled then
      aCanvas.Font.Color := clWindowText
    else
      aCanvas.Font.Color := clGrayText;
    drawtext(aCanvas.Handle, pchar(s), length(s), r, DT_VCENTER or DT_SINGLELINE);
  end;
end;

procedure DrawCombobox(_Combobox: TKOLCombobox; Canvas: TCanvas);
var
  r, R1: trect;
  Bot: Integer;
  I: Integer;
  s: string;
begin
  if coSimple in _Combobox.Options then begin
    r := _Combobox.clientrect;
    Bot := r.bottom;
    r.bottom := r.top + Canvas.TextHeight('A') + 8;
    DrawCombobox1(_Combobox, Canvas, r);
    r.top := r.bottom;
    r.bottom := Bot;
    frame3d(Canvas, r, clbtnshadow, clbtnhighlight, 1);
    frame3d(Canvas, r, clblack, cl3dlight, 1);
    Inc(r.left, 2);
    setbkmode(Canvas.handle, windows.TRANSPARENT);
    R1 := r;
    for I := 0 to _Combobox.items.count - 1 do begin
      s := _Combobox.items[I];
      R1.bottom := R1.top + Canvas.TextHeight('A') + 4;
      if R1.bottom > r.bottom then
        R1.bottom := r.bottom;
      drawtext(Canvas.handle, pchar(s), length(s), R1,
        {DT_VCENTER or}DT_SINGLELINE);
      R1.top := R1.bottom;
      if R1.top >= r.bottom then begin
        r.left := r.right - getsystemmetrics(SM_CXVSCROLL);
        //DrawScrollBar_Vertical( Canvas.Handle, R );
        Break;
      end;
    end;
  end
  else begin
    DrawCombobox1(_Combobox, Canvas, _Combobox.clientrect);
  end;
end;

end.

