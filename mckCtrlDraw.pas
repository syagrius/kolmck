// ux themed
//dufa
unit mckCtrlDraw;

interface

uses 
  Windows, Types, KOL, Themes;

const
  TextHFlags: array[TTextAlign] of DWORD     = (DT_LEFT, DT_RIGHT,   DT_CENTER);
  TextVFlags: array[TVerticalAlign] of DWORD = (DT_TOP,  DT_VCENTER, DT_BOTTOM);
  WordWrapFlags: array[Boolean] of DWORD     = (DT_SINGLELINE, 0);//!
  CheckFlags: array[Boolean] of DWORD        = (0, DFCS_CHECKED);

procedure DrawButton(DC: HDC; R: TRect; aEnabled, aDefBtn: Boolean; dwTextFlags: DWORD; aText: WideString);
procedure DrawEditBox(DC: HDC; R: TRect; aEnabled, aIsPwd: Boolean; dwTextFlags: DWORD; aText: WideString);
procedure DrawMemo(DC: HDC; R: TRect; aEnabled: Boolean; dwTextFlags: DWORD; aText: WideString);
procedure DrawCombobox(DC: HDC; R: TRect; aEnabled: Boolean; aText: WideString);
procedure DrawLabel(DC: HDC; R: TRect; dwTextFlags: DWORD; aText: WideString);
procedure DrawCheckbox(DC: HDC; R: TRect; aEnabled, aChecked, aHasBorder: Boolean; aText: WideString);
procedure DrawRadiobox(DC: HDC; R: TRect; aEnabled, aChecked, aHasBorder: Boolean; aText: WideString);
procedure DrawListBox(DC: HDC; R: TRect; aEnabled: Boolean; dwTextFlags: DWORD; aText: WideString);

implementation

procedure DrawButton(DC: HDC; R: TRect; aEnabled, aDefBtn: Boolean; dwTextFlags: DWORD; aText: WideString);
const                   //enb      defbtn
  arrTThemedButton: array[Boolean, Boolean] of TThemedButton =
    ((tbPushButtonDisabled, tbPushButtonDisabled), (tbPushButtonNormal, tbPushButtonDefaulted));

var
  d: TThemedElementDetails;
begin
  // get element
  d := ThemeServices.GetElementDetails(arrTThemedButton[aEnabled, aDefBtn]);
  // draw element
  ThemeServices.DrawElement(DC, d, R, nil);
  // text
  ThemeServices.DrawText(DC, d, aText, R, dwTextFlags or DT_SINGLELINE, 0);
end;

procedure DrawEditBox(DC: HDC; R: TRect; aEnabled, aIsPwd: Boolean; dwTextFlags: DWORD; aText: WideString);
const
  arrThemedEdit: array[Boolean] of TThemedEdit = (teEditTextDisabled, teEditTextNormal);

var
  d:  TThemedElementDetails;
//  ss: WideString;
begin
  // get element
  d := ThemeServices.GetElementDetails(arrThemedEdit[aEnabled]);
  // draw element
  ThemeServices.DrawElement(DC, d, r, nil);
  // password text style
//  ss := aText;
//  if (Length(ss) > 0) and aIsPwd then
//    ss := StrRepeat('*', Length(ss));
  // draw text
  Inc(r.Left, 6);
  Inc(r.Top, 3);
  Dec(r.Right, 3);
  Dec(r.Bottom, 3);
  ThemeServices.DrawText(DC, d, {ss}aText, r, dwTextFlags or DT_SINGLELINE, 0);
end;

procedure DrawMemo(DC: HDC; R: TRect; aEnabled: Boolean; dwTextFlags: DWORD; aText: WideString);
const
  arrThemedEdit: array[Boolean] of TThemedEdit = (teEditTextDisabled, teEditTextNormal);

var
  d: TThemedElementDetails;
begin
  // get element
  d := ThemeServices.GetElementDetails(arrThemedEdit[aEnabled]);
  // draw element
  ThemeServices.DrawElement(DC, d, r, nil);
  // get element v-track
  d := ThemeServices.GetElementDetails(tsLowerTrackVertDisabled);
  // draw element
  ThemeServices.DrawElement(DC, d, Rect(r.Right - 20, 1, r.Right - 1, r.Bottom - 1), nil);
  // get element btn-up
  d := ThemeServices.GetElementDetails(tsArrowBtnUpDisabled);
  // draw element
  ThemeServices.DrawElement(DC, d, Rect(r.Right - 20, 1, r.Right - 1, 20), nil);
  // get element btn-dn
  d := ThemeServices.GetElementDetails(tsArrowBtnDownDisabled);
  // draw element
  ThemeServices.DrawElement(DC, d, Rect(r.Right - 20, r.Bottom - 40, r.Right - 1, r.Bottom - 20), nil);

  // get element h-track
  d := ThemeServices.GetElementDetails(tsLowerTrackHorzDisabled);
  // draw element
  ThemeServices.DrawElement(DC, d, Rect(1, r.Bottom - 20, r.Right - 20, r.Bottom - 1), nil);
  // get element btn-left
  d := ThemeServices.GetElementDetails(tsArrowBtnLeftDisabled);
  // draw element
  ThemeServices.DrawElement(DC, d, Rect(1, r.Bottom - 20, 20, r.Bottom - 1), nil);
  // get element btn-right
  d := ThemeServices.GetElementDetails(tsArrowBtnRightDisabled);
  // draw element
  ThemeServices.DrawElement(DC, d, Rect(r.Right - 40, r.Bottom - 20, r.Right - 20, r.Bottom - 1), nil);

  // draw text
  Inc(r.Left, 6);
  Inc(r.Top, 3);
  Dec(r.Right, 23);
  Dec(r.Bottom, 23);
  ThemeServices.DrawText(DC, d, aText, r, dwTextFlags, 0);
end;

procedure DrawCombobox(DC: HDC; R: TRect; aEnabled: Boolean; aText: WideString);
const
  arrThemedComboBox: array[Boolean] of TThemedComboBox = (tcDropDownButtonDisabled, tcDropDownButtonNormal);

var
  d: TThemedElementDetails;
begin
  // get element
  d := ThemeServices.GetElementDetails(tcComboBoxRoot);
  // draw element
  ThemeServices.DrawElement(DC, d, r, nil);
  // get element
  d := ThemeServices.GetElementDetails(arrThemedComboBox[aEnabled]);
  // draw element
  r.Left := r.Right - 18;
  Inc(r.Top, 1);
  Dec(r.Right, 1);
  Dec(r.Bottom, 1);
  ThemeServices.DrawElement(DC, d, r, nil);
  // draw text
  r.Left := 6;
  Inc(r.Top, 2);
  Dec(r.Right, 18);
  Inc(r.Bottom, 1);
  ThemeServices.DrawText(DC, d, aText, r, DT_LEFT or DT_SINGLELINE, 0);
end;

//TextHFlags[Sender.TextAlign] or TextVFlags[Sender.VerticalAlign] or WordWrapFlags[Sender.WordWrap]
procedure DrawLabel(DC: HDC; R: TRect; dwTextFlags: DWORD; aText: WideString);
begin
  // draw
  FillRect(DC, r, HBRUSH(COLOR_BTNFACE + 1));
  SetBkMode(DC, TRANSPARENT);
  DrawTextW(DC, PWideChar(aText), Length(aText), r, dwTextFlags);
end;

procedure DrawCheckbox(DC: HDC; R: TRect; aEnabled, aChecked, aHasBorder: Boolean; aText: WideString);
const                   //enb      chk
  arrTThemedButton: array[Boolean, Boolean] of TThemedButton =
    ((tbCheckBoxUncheckedDisabled, tbCheckBoxCheckedDisabled), (tbCheckBoxUncheckedNormal, tbCheckBoxCheckedNormal));

var
  d:  TThemedElementDetails;
  rr: TRect;
begin
  // get element
  d  := ThemeServices.GetElementDetails(arrTThemedButton[aEnabled, aChecked]);
  // draw back
  FillRect(DC, r, HBRUSH(COLOR_BTNFACE + 1));
  // draw border
  if aHasBorder then
    ThemeServices.DrawEdge(DC, d, r, EDGE_BUMP, BF_RECT or BF_MIDDLE);
  // draw element
  rr := Bounds(-3, -3, 22, 22);
  ThemeServices.DrawElement(DC, d, rr);
  // draw text
  Inc(r.Left, 18);
  ThemeServices.DrawText(DC, d, aText, r, DT_LEFT, 0);
end;

procedure DrawRadiobox(DC: HDC; R: TRect; aEnabled, aChecked, aHasBorder: Boolean; aText: WideString);
const                   //enb      chk
  arrFlags: array[Boolean, Boolean] of DWORD =
    ((DFCS_INACTIVE, DFCS_CHECKED or DFCS_INACTIVE), (0, DFCS_CHECKED));

var
  rr: TRect;
begin
  // draw back
  FillRect(DC, r, HBRUSH(COLOR_BTNFACE + 1));
  // draw border
  if aHasBorder then
    DrawEdge(DC, r, EDGE_RAISED, BF_RECT or BF_MIDDLE);
  // draw element
  rr := Bounds(r.Left + 2, (r.Bottom + r.Top - 13) div 2, 13, 13);
  DrawFrameControl(DC, rr, DFC_BUTTON, DFCS_BUTTONRADIO or arrFlags[aEnabled, aChecked]);
  // draw text
  Inc(r.left, 17);
  SetBkMode(DC, TRANSPARENT);
  DrawTextW(DC, PWideChar(aText), Length(aText), r, DT_VCENTER or DT_SINGLELINE);
end;

procedure DrawListBox(DC: HDC; R: TRect; aEnabled: Boolean; dwTextFlags: DWORD; aText: WideString);
const
  arrThemedEdit: array[Boolean] of TThemedListview = (tlListItemDisabled, tlListItemDisabled);

var
  d: TThemedElementDetails;
begin
  // get element
  d := ThemeServices.GetElementDetails(arrThemedEdit[aEnabled]);
  // draw element
  ThemeServices.DrawElement(DC, d, r, nil);
  // draw text
  Inc(r.Left, 6);
  Inc(r.Top, 3);
  Dec(r.Right, 3);
  Dec(r.Bottom, 3);
  ThemeServices.DrawText(DC, d, aText, r, dwTextFlags, 0);
end;


end.
