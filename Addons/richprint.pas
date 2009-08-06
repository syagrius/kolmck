unit RichPrint;
{* By Savva. A unit to print rich edit control content. }

interface

uses Windows, KOL, {$IFNDEF NOT_USE_PRINTER_OBJ}
     {$IFDEF USE_MHPRINTER} KOLMHPrinters {$ELSE} KOLPrinters {$ENDIF}
     ,{$ENDIF}
     RichEdit, CommDlg;

procedure FilePrint(ACaption : string;fRichEdit : PControl);
{* ������ ��� ������������� ������� Printer }

{$IFNDEF NOT_USE_PRINTER_OBJ}
procedure PrintRichEdit(CONST fRichEdit : PControl;const Caption: string);
{* ������ c ��������������  ������� Printer }
{$ENDIF}

implementation

//*****************************************************
// ������ ��� ������������� ������� Printer
// -----------------------------------------------------
// ������� FilePrint
// -----------------------------------------------------
procedure FilePrint(ACaption : string;fRichEdit : PControl);
var
  fr : FORMATRANGE;
  docInfo : TDOCINFO;
  lLastChar, lTextSize :integer ;
  pd : TPRINTDLG ;
  nRc : integer ;
  hPrintDC : HDC ;

  szErr : string;
  dwErr :DWORD ;
  //TextLenEx: TGetTextLengthEx;

begin
  // �������������� ���� ��������� PRITDLG

  ZeroMemory(@pd, sizeof(pd));
  pd.lStructSize := sizeof(TPRINTDLG);
  pd.hwndOwner   := fRichEdit.Handle;
  pd.hInstance   := HInstance;
  pd.Flags       := PD_RETURNDC or PD_NOPAGENUMS or PD_NOSELECTION or  PD_PRINTSETUP or
PD_ALLPAGES;
  pd.nFromPage   := $ffff;
  pd.nToPage     := $ffff;
  pd.nMinPage    := 0;
  pd.nMaxPage    := $ffff;
  pd.nCopies     := 1;


  // ������� �� ����� ���������� ������, ���������������
  // ��� ������ ���������
  if PrintDlg(pd) then begin
//  if(TRUE) then begin
    hPrintDC := pd.hDC;

    // �������������� ���� ��������� FORMATRANGE
    ZeroMemory(@fr, sizeof(fr));

    // ����� �������� � �������������� ���������
    // ��������, ����������� �� ������� PrintDlg
    fr.hdc := hPrintDC;
    fr.hdcTarget:=fr.hdc;

    // �������� ���� ��������
    fr.chrg.cpMin := 0;
    fr.chrg.cpMax := -1;

    // ������������� ������� �������� � TWIPS-��
    fr.rcPage.top := 0;
    fr.rcPage.left := 0;
    fr.rcPage.right :=
      MulDiv(GetDeviceCaps(hPrintDC, PHYSICALWIDTH),
      1440, GetDeviceCaps(hPrintDC, LOGPIXELSX));

    fr.rcPage.bottom := MulDiv(GetDeviceCaps(hPrintDC,
      PHYSICALHEIGHT),1440,
      GetDeviceCaps(hPrintDC, LOGPIXELSY));
    fr.rc := fr.rcPage;

    // ��������� ����
    if (fr.rcPage.right > 2*3*1440/4+1440) then begin
      fr.rc.left := 3*1440 div 4;
      fr.rc.right :=fr.rc.right - (fr.rc.left);
    end;
    if(fr.rcPage.bottom > 3*1440) then begin
      fr.rc.top := 1440;
      fr.rc.bottom:=fr.rc.bottom - (fr.rc.top);
    end;

    // ��������� ���� ��������� DOCINFO
    ZeroMemory(@docInfo, sizeof(DOCINFO));
    docInfo.cbSize := sizeof(DOCINFO);
    docInfo.lpszOutput := nil;
    docInfo.lpszDocName := PChar(ACaption);


    // �������� ������ ���������
    nRc := StartDoc(hPrintDC, docInfo);

    // ���� ��������� ������, �������� � ������� �� �����
    // ��� ������
    if (nRc < 0) then begin
      dwErr := GetLastError();

      szErr:=format( 'Print Error %ld \r\n %s', [dwErr,SysErrorMessage(dwErr)]);

      MessageBox(0, PChar(szErr),
        'Error printing', MB_OK or MB_ICONEXCLAMATION);

      DeleteDC(hPrintDC);
      exit;
    end;

    // �������� ������ ��������
    StartPage(hPrintDC);

    lLastChar := 0;

    // ���������� ����� ������
    lTextSize := fRichEdit.RE_TextSizePrecise;
    // ���� �� ���� ��������� ���������
    while (lLastChar < lTextSize) do begin
      // ����������� ������ ��� �������� � �������� ��
      lLastChar := SendMessage(fRichEdit.Handle, EM_FORMATRANGE, DWORD(TRUE),
        LPARAM( @fr));

      if(lLastChar < lTextSize) then begin
        // ��������� ������ ��������� ��������
        EndPage(hPrintDC);

        // �������� ����� ��������
        StartPage(hPrintDC);
        fr.chrg.cpMin := lLastChar;
        fr.chrg.cpMax := -1;
      end;
    end;

    // ������� ����������, ������� �������� �
    // ������ ���������� Rich Edit
    SendMessage(fRichEdit.Handle, EM_FORMATRANGE, DWORD(TRUE), LPARAM(nil));

    // ��������� ������ ��������
    EndPage(hPrintDC);

    // ��������� ������ ���������
    EndDoc(hPrintDC);

    // ������� �������� ��������
    DeleteDC(hPrintDC);
    end;
end;

{$IFNDEF NOT_USE_PRINTER_OBJ}
//*****************************************************
// ������ c ��������������  ������� Printer
procedure PrintRichEdit(CONST fRichEdit : PControl;const Caption: string);
var
  Range: TFormatRange;
  LastChar, MaxLen, LogX, LogY, OldMap: Integer;
  SaveRect: TRect;
begin
  FillChar(Range, SizeOf(TFormatRange), 0);
  Printer.Title := Caption;
  Printer.BeginDoc;
  Range.hdc := Printer.Handle;
  Range.hdcTarget := Range.hdc;
  LogX := GetDeviceCaps(Printer.Handle, LOGPIXELSX);
  LogY := GetDeviceCaps(Printer.Handle, LOGPIXELSY);

  Range.rc.right := Printer.PageWidth * 1440 div LogX;
  Range.rc.bottom := Printer.PageHeight * 1440 div LogY;
  Range.rcPage := Range.rc;
  SaveRect := Range.rc;
  LastChar := 0;
//    MaxLen := fRichEdit.Perform(WM_GETTEXTLENGTH, 0, 0);
    MaxLen := fRichEdit.RE_TextSizePrecise;
  Range.chrg.cpMax := -1;
  // ensure printer DC is in text map mode
  OldMap := SetMapMode(range.hdc, MM_TEXT);
  fRichEdit.Perform(EM_FORMATRANGE, 0, 0);    // flush buffer
    try
      repeat
        Range.rc := SaveRect;
        Range.chrg.cpMin := LastChar;
        LastChar := fRichEdit.Perform(EM_FORMATRANGE, 1, Longint(@Range));
        if (LastChar < MaxLen) and (LastChar <> -1) then Printer.NewPage;
      until (LastChar >= MaxLen) or (LastChar = -1);
      Printer.EndDoc;
    finally
      fRichEdit.Perform(EM_FORMATRANGE, 0, 0);  // flush buffer
      SetMapMode(Range.hdc, OldMap);       // restore previous map mode
    end;
end;
{$ENDIF}


end.
