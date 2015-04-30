// ux themed. dufa
unit mckCtrlDraw;

interface

uses 
  Windows, Types, KOL, Themes;

type
  TScrollStyle = (ssNone, ssHorz, ssVert, ssBoth);

const
  TextHFlags: array[TTextAlign] of DWORD     = (DT_LEFT, DT_RIGHT,   DT_CENTER);
  TextVFlags: array[TVerticalAlign] of DWORD = (DT_TOP,  DT_VCENTER, DT_BOTTOM);
  WordWrapFlags: array[Boolean] of DWORD     = (DT_SINGLELINE, 0);//!
  CheckFlags: array[Boolean] of DWORD        = (0, DFCS_CHECKED);

procedure DrawButton(aUX: Boolean; DC: HDC; R: TRect; aEnabled, aDefBtn: Boolean; dwTextFlags: DWORD; aText: WideString);
procedure DrawEditBox(aUX: Boolean; DC: HDC; R: TRect; aEnabled, aIsPwd: Boolean; dwTextFlags: DWORD; aText: WideString);
procedure DrawMemo(aUX: Boolean; DC: HDC; R: TRect; aColor: Integer; aEnabled: Boolean; aScrollStyle: TScrollStyle; dwTextFlags: DWORD; aText: WideString);
procedure DrawCombobox(DC: HDC; R: TRect; aEnabled: Boolean; aText: WideString);
procedure DrawLabel(DC: HDC; R: TRect; dwTextFlags: DWORD; aText: WideString);
procedure DrawCheckbox(DC: HDC; R: TRect; aEnabled, aChecked, aHasBorder: Boolean; aText: WideString);
procedure DrawRadiobox(aUX: Boolean; DC: HDC; R: TRect; aEnabled, aChecked, aHasBorder: Boolean; aText: WideString);
procedure DrawListBox(aUX: Boolean; DC: HDC; R: TRect; aEnabled: Boolean; aText: WideString);
procedure DrawTreeView(aUX: Boolean; DC: HDC; R: TRect; aEnabled: Boolean; aText: WideString);
procedure DrawListView(aUX: Boolean; DC: HDC; R: TRect; aEnabled: Boolean; aColumns: KOLWideString);
procedure DrawProgressBar(DC: HDC; R: TRect; aVertical: Boolean; aProgress, aMaxProgress: Integer);
procedure DrawTrackBar(DC: HDC; R: TRect; aVertical: Boolean; aProgress, aMaxProgress: Integer);
procedure DrawGroupBox(aUX: Boolean; DC: HDC; R: TRect; aText: WideString);
procedure DrawScrollBar(aUX: Boolean; DC: HDC; R: TRect; aEnabled, aVertical: Boolean; aPos, aMin, aMax: Integer);
procedure DrawScrollBox(aUX: Boolean; DC: HDC; R: TRect; aScrollStyle: TScrollStyle);
// not yet
procedure DrawToolbar(aUX: Boolean; DC: HDC; R: TRect; aEnabled: Boolean; aText: WideString);

implementation

const
  arrThemedEdit: array[Boolean] of TThemedEdit = (teEditTextDisabled, teEditTextNormal);

function pos2pix(aPos, aMin, aMax, aPixelMax, aPixelX: Integer): Integer;
begin
  Result := aPixelX + Round((aPos + Abs(aMin)) / (aMax + Abs(aMin)) * (aPixelMax - aPixelX * 3));
end;

procedure DrawScrollBar(aUX: Boolean; DC: HDC; R: TRect; aEnabled, aVertical: Boolean; aPos, aMin, aMax: Integer);
const                  //enb    btn
  arrThemedSBLU: array[Boolean, Boolean] of TThemedScrollBar = (
    (tsArrowBtnLeftDisabled, tsArrowBtnUpDisabled), (tsArrowBtnLeftNormal, tsArrowBtnUpNormal));
  arrThemedSBRD: array[Boolean, Boolean] of TThemedScrollBar = (
    (tsArrowBtnRightDisabled, tsArrowBtnDownDisabled), (tsArrowBtnRightNormal, tsArrowBtnDownNormal));
  arrThemedSBTH: array[Boolean] of TThemedScrollBar = (tsThumbBtnHorzNormal,  tsThumbBtnVertNormal);
  arrSBLU: array[Boolean, Boolean] of DWORD = (
    (DFCS_SCROLLLEFT or DFCS_INACTIVE,DFCS_SCROLLUP or DFCS_INACTIVE), (DFCS_SCROLLLEFT,  DFCS_SCROLLUP));
  arrSBRD: array[Boolean, Boolean] of DWORD = (
    (DFCS_SCROLLRIGHT or DFCS_INACTIVE, DFCS_SCROLLDOWN or DFCS_INACTIVE), (DFCS_SCROLLRIGHT, DFCS_SCROLLDOWN));

var
  w:  Integer;
  h:  Integer;
  rr: TRect;
begin
  // get btn size
  w := GetSystemMetrics(SM_CXVSCROLL);
  h := GetSystemMetrics(SM_CYVSCROLL);
  // pos2pix
  if aVertical then
    rr := Bounds(R.Left, pos2pix(aPos, aMin, aMax, R.Bottom, h), R.Right - R.Left, GetSystemMetrics(SM_CYVTHUMB))
  else
    rr := Bounds(pos2pix(aPos, aMin, aMax, R.Right, w), R.Top, GetSystemMetrics(SM_CXHTHUMB), R.Bottom - R.Top);
  // ux
  if ThemeServices.ThemesAvailable and aUX then begin
    // draw track
    ThemeServices.DrawElement(DC, ThemeServices.GetElementDetails(tsLowerTrackVertNormal), R, nil);
    // draw btn-left/up
    ThemeServices.DrawElement(DC, ThemeServices.GetElementDetails(arrThemedSBLU[aEnabled, aVertical]), Bounds(R.Left, R.Top, w, h), nil);
    // draw btn-right/down
    ThemeServices.DrawElement(DC, ThemeServices.GetElementDetails(arrThemedSBRD[aEnabled, aVertical]), Rect(R.Right - w, R.Bottom - h, R.Right, R.Bottom), nil);
    // draw thumb
    if aEnabled then
      ThemeServices.DrawElement(DC, ThemeServices.GetElementDetails(arrThemedSBTH[aVertical]), rr, nil);
  end else begin
    // draw track
    FillRect(DC, R, GetSysColorBrush(COLOR_SCROLLBAR));
    // draw btn-left/up
    DrawFrameControl(DC, Bounds(R.Left, R.Top, w, h), DFC_SCROLL, arrSBLU[aEnabled, aVertical]);
    // draw btn-right/down
    DrawFrameControl(DC, Rect(R.Right - w, R.Bottom - h, R.Right, R.Bottom), DFC_SCROLL, arrSBRD[aEnabled, aVertical]);
    // draw thumb
    if aEnabled then
      DrawFrameControl(DC, rr, DFC_BUTTON, DFCS_BUTTONPUSH);
  end;
end;

procedure DrawButton(aUX: Boolean; DC: HDC; R: TRect; aEnabled, aDefBtn: Boolean; dwTextFlags: DWORD; aText: WideString);
const                   //enb      defbtn
  arrThemedButton: array[Boolean, Boolean] of TThemedButton =
    ((tbPushButtonDisabled, tbPushButtonDisabled), (tbPushButtonNormal, tbPushButtonDefaulted));

  arrSimpleButton: array[Boolean] of DWORD = (DFCS_BUTTONPUSH or DFCS_INACTIVE, DFCS_BUTTONPUSH);

var
  d: TThemedElementDetails;
begin
  if ThemeServices.ThemesAvailable and aUX then begin
    // get element
    d := ThemeServices.GetElementDetails(arrThemedButton[aEnabled, aDefBtn]);
    // draw element
    ThemeServices.DrawElement(DC, d, R, nil);
    // text
    ThemeServices.DrawText(DC, d, aText, R, dwTextFlags or DT_SINGLELINE, 0);
  end else begin
    // draw defbtn
    if aDefBtn then begin
      // draw the defaulted border
      FrameRect(DC, R, GetSysColorBrush(COLOR_WINDOWFRAME));
      InflateRect(R, -1, -1);
    end;
    // draw element
    DrawFrameControl(DC, R, DFC_BUTTON, arrSimpleButton[aEnabled]);
    // draw text
    SetBkMode(DC, TRANSPARENT);
    SetTextColor(DC, GetSysColor(COLOR_GRAYTEXT + Ord(aEnabled)));
    DrawTextW(DC, PWideChar(aText), Length(aText), R, dwTextFlags or DT_SINGLELINE);
  end;
end;

procedure DrawEditBox(aUX: Boolean; DC: HDC; R: TRect; aEnabled, aIsPwd: Boolean; dwTextFlags: DWORD; aText: WideString);
begin
  if ThemeServices.ThemesAvailable and aUX then begin
    // draw element
    ThemeServices.DrawElement(DC, ThemeServices.GetElementDetails(arrThemedEdit[True]), r, nil);
    // draw text
    Inc(r.Left, 6);
    Inc(r.Top, 3);
    Dec(r.Right, 3);
    Dec(r.Bottom, 3);
    ThemeServices.DrawText(DC, ThemeServices.GetElementDetails(arrThemedEdit[aEnabled]), aText, r, dwTextFlags or DT_SINGLELINE, 0);
  end else begin
    // draw back
    FillRect(DC, R, GetSysColorBrush(COLOR_WINDOW));
    DrawEdge(DC, R, EDGE_SUNKEN, BF_RECT or BF_ADJUST);
    InflateRect(R, -4, -1);
    // draw text
    SetBkMode(DC, TRANSPARENT);
    SetTextColor(DC, GetSysColor(COLOR_GRAYTEXT + Ord(aEnabled)));
    DrawTextW(DC, PWideChar(aText), Length(aText), R, dwTextFlags or DT_SINGLELINE);
  end;
end;

procedure DrawMemo(aUX: Boolean; DC: HDC; R: TRect; aColor: Integer; aEnabled: Boolean; aScrollStyle: TScrollStyle; dwTextFlags: DWORD; aText: WideString);
var
  w: Integer;
  h: Integer;
  b: HBRUSH;
begin
  // get btn size
  w := GetSystemMetrics(SM_CXVSCROLL);
  h := GetSystemMetrics(SM_CYVSCROLL);
  if ThemeServices.ThemesAvailable and aUX then begin
    // draw border
    ThemeServices.DrawElement(DC, ThemeServices.GetElementDetails(arrThemedEdit[True]), R, nil);
    InflateRect(R, -1, -1);
    // draw back
    b := CreateSolidBrush(Color2RGB(aColor));
    FillRect(DC, R, b);
    DeleteObject(b);
    InflateRect(R, -1, -1);
    // draw scrolls
    case aScrollStyle of
      ssHorz: DrawScrollBar(aUX, DC, Rect(R.Left, R.Bottom - h, R.Right, R.Bottom), False, False, 2, 0, 100);
      ssVert: DrawScrollBar(aUX, DC, Rect(R.Right - w, R.Top, R.Right, R.Bottom), False, True, 2, 0, 100);
      ssBoth:
      begin
        ThemeServices.DrawElement(DC, ThemeServices.GetElementDetails(tsLowerTrackVertNormal), Rect(R.Left, R.Bottom - h, R.Right, R.Bottom), nil);
        DrawScrollBar(aUX, DC, Rect(R.Left, R.Bottom - h, R.Right - w, R.Bottom), False, False, 2, 0, 100);
        DrawScrollBar(aUX, DC, Rect(R.Right - w, R.Top, R.Right, R.Bottom - h), False, True, 2, 0, 100);
      end;
    end;
    // draw text
    Inc(R.Left, 4);
    Inc(R.Top, 1);
    Dec(R.Right, 21);
    Dec(R.Bottom, 21);
    ThemeServices.DrawText(DC, ThemeServices.GetElementDetails(arrThemedEdit[aEnabled]), aText, R, dwTextFlags, 0);
  end else begin
    // draw back
    b := CreateSolidBrush(Color2RGB(aColor));
    FillRect(DC, R, b);
    DrawEdge(DC, R, EDGE_SUNKEN, BF_RECT or BF_ADJUST);
    DeleteObject(b);
    // draw scrolls
    case aScrollStyle of
      ssHorz: DrawScrollBar(aUX, DC, Rect(R.Left, R.Bottom - h, R.Right, R.Bottom), False, False, 2, 0, 100);
      ssVert: DrawScrollBar(aUX, DC, Rect(R.Right - w, R.Top, R.Right, R.Bottom), False, True, 2, 0, 100);
      ssBoth:
      begin
        FillRect(DC, Rect(R.Left, R.Bottom - h, R.Right, R.Bottom), GetSysColorBrush(COLOR_BTNFACE));
        DrawScrollBar(aUX, DC, Rect(R.Left, R.Bottom - h, R.Right - w, R.Bottom), False, False, 2, 0, 100);
        DrawScrollBar(aUX, DC, Rect(R.Right - w, R.Top, R.Right, R.Bottom - h), False, True, 2, 0, 100);
      end;
    end;
    // draw text
    InflateRect(R, -4, -1);
    SetBkMode(DC, TRANSPARENT);
    SetTextColor(DC, GetSysColor(COLOR_GRAYTEXT + Ord(aEnabled)));
    DrawTextW(DC, PWideChar(aText), Length(aText), R, dwTextFlags);
  end;
end;

procedure DrawCombobox(DC: HDC; R: TRect; aEnabled: Boolean; aText: WideString);
const
  arrThemedComboBox: array[Boolean] of TThemedComboBox = (tcDropDownButtonDisabled, tcDropDownButtonNormal);
begin
  // draw element
  ThemeServices.DrawElement(DC, ThemeServices.GetElementDetails(tcComboBoxRoot), r, nil);
  // draw element
  r.Left := r.Right - 18;
  Inc(r.Top, 1);
  Dec(r.Right, 1);
  Dec(r.Bottom, 1);
  ThemeServices.DrawElement(DC, ThemeServices.GetElementDetails(arrThemedComboBox[aEnabled]), r, nil);
  // draw text
  r.Left := 6;
  Inc(r.Top, 2);
  Dec(r.Right, 18);
  Inc(r.Bottom, 1);
  ThemeServices.DrawText(DC, ThemeServices.GetElementDetails(arrThemedEdit[aEnabled]), aText, r, DT_LEFT or DT_SINGLELINE, 0);
end;

//TextHFlags[Sender.TextAlign] or TextVFlags[Sender.VerticalAlign] or WordWrapFlags[Sender.WordWrap]
procedure DrawLabel(DC: HDC; R: TRect; dwTextFlags: DWORD; aText: WideString);
begin
  // draw
  FillRect(DC, r, GetSysColorBrush(COLOR_BTNFACE));
  SetBkMode(DC, TRANSPARENT);
  DrawTextW(DC, PWideChar(aText), Length(aText), r, dwTextFlags);
end;

procedure DrawCheckbox(DC: HDC; R: TRect; aEnabled, aChecked, aHasBorder: Boolean; aText: WideString);
const                   //enb      chk
  arrThemedCB: array[Boolean, Boolean] of TThemedButton =
    ((tbCheckBoxUncheckedDisabled, tbCheckBoxCheckedDisabled), (tbCheckBoxUncheckedNormal, tbCheckBoxCheckedNormal));

var
  d:  TThemedElementDetails;
  rr: TRect;
begin
  // draw back
  FillRect(DC, r, GetSysColorBrush(COLOR_BTNFACE));
  // get element
  d := ThemeServices.GetElementDetails(arrThemedCB[aEnabled, aChecked]);
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

procedure DrawRadiobox(aUX: Boolean; DC: HDC; R: TRect; aEnabled, aChecked, aHasBorder: Boolean; aText: WideString);
const                   //enb      chk
  arrFlags: array[Boolean, Boolean] of DWORD =
    ((DFCS_INACTIVE, DFCS_CHECKED or DFCS_INACTIVE), (0, DFCS_CHECKED));
                        //enb      chk
  arrThemedRB: array[Boolean, Boolean] of TThemedButton =
    ((tbRadioButtonUncheckedDisabled, tbRadioButtonCheckedDisabled),
    (tbRadioButtonUncheckedNormal, tbRadioButtonCheckedNormal));

var
  d:  TThemedElementDetails;
  rr: TRect;
begin
  // draw back
  FillRect(DC, R, GetSysColorBrush(COLOR_BTNFACE));
  // draw other
  if aUX then begin
    // get element
    d := ThemeServices.GetElementDetails(arrThemedRB[aEnabled, aChecked]);
    // draw border
    if aHasBorder then
      ThemeServices.DrawEdge(DC, d, R, EDGE_BUMP, BF_RECT or BF_MIDDLE);
    // draw element
    rr := Bounds(0, 0, 15, 15);
    ThemeServices.DrawElement(DC, d, rr);
    // draw text
    r.Left := rr.Right + 2;
    ThemeServices.DrawText(DC, d, aText, R, DT_LEFT, 0);
  end else begin
    // draw border
    if aHasBorder then
      DrawEdge(DC, r, EDGE_RAISED, BF_RECT or BF_MIDDLE);
    // draw element
    rr := Bounds(R.Left + 2, (R.Bottom + R.Top - 13) div 2, 13, 13);
    DrawFrameControl(DC, rr, DFC_BUTTON, DFCS_BUTTONRADIO or arrFlags[aEnabled, aChecked]);
    // draw text
    Inc(R.Left, 17);
    SetBkMode(DC, TRANSPARENT);
    DrawTextW(DC, PWideChar(aText), Length(aText), R, DT_VCENTER or DT_SINGLELINE);
  end;
end;

procedure DrawListBox(aUX: Boolean; DC: HDC; R: TRect; aEnabled: Boolean; aText: WideString);
begin
  if ThemeServices.ThemesAvailable and aUX then begin
    // draw element
    ThemeServices.DrawElement(DC, ThemeServices.GetElementDetails(tlListviewRoot), R, nil);
    // draw text
    Inc(R.Left, 3);
    Inc(R.Top, 3);
    Dec(R.Right, 3);
    Dec(R.Bottom, 3);
    ThemeServices.DrawText(DC, ThemeServices.GetElementDetails(arrThemedEdit[aEnabled]), aText, R, DT_LEFT, 0);
  end else begin
    // draw back
    FillRect(DC, R, GetSysColorBrush(COLOR_WINDOW));
    DrawEdge(DC, R, EDGE_SUNKEN, BF_RECT or BF_ADJUST);
    InflateRect(R, -1, 0);
    // draw text
    SetBkMode(DC, TRANSPARENT);
    SetTextColor(DC, GetSysColor(COLOR_GRAYTEXT + Ord(aEnabled)));
    DrawTextW(DC, PWideChar(aText), Length(aText), R, DT_LEFT);
  end;
end;

procedure DrawTreeView(aUX: Boolean; DC: HDC; R: TRect; aEnabled: Boolean; aText: WideString);
begin
  DrawListBox(aUX, DC, R, aEnabled, aText);
end;

procedure DrawListView(aUX: Boolean; DC: HDC; R: TRect; aEnabled: Boolean; aColumns: KOLWideString);
var
  w: WideString;
  d: TThemedElementDetails;
begin
  // draw main
  DrawListBox(aUX, DC, R, aEnabled, '');
  // columns
  if (aColumns <> '') then begin
    // draw head back
    R := Bounds(2, 2, R.Right - 4, 20);
    ThemeServices.DrawElement(DC, ThemeServices.GetElementDetails(thHeaderRoot), R, nil);
    // draw columns
    R := Bounds(2, 2, 50, 20);
    repeat
      w := ParseW(aColumns, #13);
      if (aColumns = '') and (w = '') then
        Break
      else begin
        // get element
        d := ThemeServices.GetElementDetails(thHeaderItemNormal);
        // draw head column
        ThemeServices.DrawElement(DC, d, R, nil);
        // draw text
        Inc(R.Left, 10);
        ThemeServices.DrawText(DC, d, w, R, DT_LEFT or DT_VCENTER or DT_SINGLELINE, 0);
        // next
        Inc(R.Left, 40);
        Inc(R.Right, 50);
      end;
    until False;
  end;
end;

procedure DrawProgressBar(DC: HDC; R: TRect; aVertical: Boolean; aProgress, aMaxProgress: Integer);
begin
  // draw bar
  ThemeServices.DrawElement(DC, ThemeServices.GetElementDetails(TThemedProgress(Ord(tpBar) + Ord(aVertical))), R, nil);
  // draw progress
  if aVertical then
    R.Top := Trunc(R.Bottom - R.Bottom * aProgress / aMaxProgress)
  else
    R.Right := Trunc(R.Right * aProgress / aMaxProgress);
  ThemeServices.DrawElement(DC, ThemeServices.GetElementDetails(TThemedProgress(Ord(tpChunk) + Ord(aVertical))), R, nil);
end;

procedure DrawTrackBar(DC: HDC; R: TRect; aVertical: Boolean; aProgress, aMaxProgress: Integer);
var
  a: TRect;
  x: Integer;
begin
  // draw root
  ThemeServices.DrawElement(DC, ThemeServices.GetElementDetails(ttbTrackBarRoot), R, nil);
  // draw bar
  a := Bounds(8, 14, R.Right - 16, 4);
  x := GetSystemMetrics(SM_CXHTHUMB);
  ThemeServices.DrawElement(DC, ThemeServices.GetElementDetails(ttbTrack), a, nil);
  // draw progress
  a := Bounds(5 + Trunc(aProgress / aMaxProgress * (a.Right - 19)), 0, x, R.Bottom);
  ThemeServices.DrawElement(DC, ThemeServices.GetElementDetails(ttbThumbBottomNormal), a, nil);
end;

procedure DrawGroupBox(aUX: Boolean; DC: HDC; R: TRect; aText: WideString);
begin
  // draw background
  FillRect(DC, R, GetSysColorBrush(COLOR_BTNFACE));
  Inc(R.Top, 8);
  // draw border
  if ThemeServices.ThemesAvailable and aUX then begin
    ThemeServices.DrawElement(DC, ThemeServices.GetElementDetails(tbGroupBoxNormal), R, nil);
    SetTextColor(DC, GetSysColor(COLOR_HIGHLIGHT));
  end else begin
    DrawEdge(DC, R, EDGE_ETCHED, BF_RECT);
    SetTextColor(DC, GetSysColor(COLOR_BTNTEXT));
  end;
  // draw text
  Inc(R.Left, 9);
  Dec(R.Top, 8);
  SetBkColor(DC, GetSysColor(COLOR_BTNFACE));
  SetBkMode(DC, OPAQUE);
  DrawTextW(DC, PWideChar(aText), Length(aText), R, DT_LEFT);
end;

procedure DrawScrollBox(aUX: Boolean; DC: HDC; R: TRect; aScrollStyle: TScrollStyle);
var
  w: Integer;
  h: Integer;
begin
  // get btn size
  w := GetSystemMetrics(SM_CXVSCROLL);
  h := GetSystemMetrics(SM_CYVSCROLL);
  // draw back
  FillRect(DC, R, GetSysColorBrush(COLOR_BTNFACE));
  DrawEdge(DC, R, EDGE_SUNKEN, BF_RECT or BF_ADJUST);
  case aScrollStyle of
    ssHorz: DrawScrollBar(aUX, DC, Rect(R.Left, R.Bottom - h, R.Right, R.Bottom), True, False, 2, 0, 100);
    ssVert: DrawScrollBar(aUX, DC, Rect(R.Right - w, R.Top, R.Right, R.Bottom), True, True, 2, 0, 100);
    ssBoth:
    begin
      DrawScrollBar(aUX, DC, Rect(R.Left, R.Bottom - h, R.Right - w, R.Bottom), True, False, 2, 0, 100);
      DrawScrollBar(aUX, DC, Rect(R.Right - w, R.Top, R.Right, R.Bottom - h), True, True, 2, 0, 100);
    end;
  end;
end;

// not yet
procedure DrawToolbar(aUX: Boolean; DC: HDC; R: TRect; aEnabled: Boolean; aText: WideString);
var
  d: TThemedElementDetails;
begin
  if ThemeServices.ThemesAvailable and aUX then begin
    // get element
    d := ThemeServices.GetElementDetails(ttbToolBarRoot);
    // draw element
    ThemeServices.DrawElement(DC, d, R, nil);

    // get element
    d := ThemeServices.GetElementDetails(ttbSplitButtonNormal);
    // draw element
    ThemeServices.DrawElement(DC, d, R, nil);
    // text
    ThemeServices.DrawText(DC, d, aText, R, 0, 0);
  end else begin

  end;
end;


end.
