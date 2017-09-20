/// types ///
type
  HDROP      = THandle;
  HImageList = THandle;

  TFNTimeCallBack = procedure(uTimerID, uMessage: UINT; dwUser, dw1, dw2: DWORD_PTR) stdcall;

  PImageInfo = ^TImageInfo;
  TImageInfo = {packed} record
    hbmImage: HBitmap;
    hbmMask:  HBitmap;
    Unused1:  Integer;
    Unused2:  Integer;
    rcImage:  TRect;
  end;

  PSHFileInfoA = ^TSHFileInfoA;
  PSHFileInfoW = ^TSHFileInfoW;
  PSHFileInfo  = PSHFileInfoA;
  _SHFILEINFOA = record
    hIcon: HICON;                      { out: icon }
    iIcon: Integer;                    { out: icon index }
    dwAttributes: DWORD;               { out: SFGAO_ flags }
    szDisplayName: array [0..MAX_PATH-1] of  AnsiChar; { out: display name (or path) }
    szTypeName: array [0..79] of AnsiChar;             { out: type name }
  end;
  TSHFileInfoA = _SHFILEINFOA;
  SHFILEINFOA  = _SHFILEINFOA;

  _SHFILEINFOW = record
    hIcon: HICON;                      { out: icon }
    iIcon: Integer;                    { out: icon index }
    dwAttributes: DWORD;               { out: SFGAO_ flags }
    szDisplayName: array [0..MAX_PATH-1] of WideChar; { out: display name (or path) }
    szTypeName: array [0..79] of WideChar;             { out: type name }
  end;
  TSHFileInfoW = _SHFILEINFOW;
  SHFILEINFOW  = _SHFILEINFOW;

  _SHFILEINFO = {$IFDEF UNICODE_CTRLS}_SHFILEINFOW{$ELSE}_SHFILEINFOA{$ENDIF};
  TSHFileInfo = {$IFDEF UNICODE_CTRLS}TSHFileInfoW{$ELSE}TSHFileInfoA{$ENDIF};
  SHFILEINFO  = {$IFDEF UNICODE_CTRLS}SHFILEINFOW{$ELSE}SHFILEINFOA{$ENDIF};

  PSHItemID = ^TSHItemID;
  TSHItemID = {packed} record
    cb: Word;                         { Size of the ID (including cb itself) }
    abID: array[0..0] of Byte;        { The item ID (variable length) }
  end;

  PItemIDList = ^TItemIDList;
  TItemIDList = record
     mkid: TSHItemID;
  end;

  PBrowseInfo = ^TBrowseInfo;
  TBrowseInfoA = record
    hwndOwner:      HWND;
    pidlRoot:       PItemIDList;
    pszDisplayName: PAnsiChar;  { Return display name of item selected. }
    lpszTitle:      PAnsiChar;  { text to go in the banner over the tree. }
    ulFlags:        UINT;       { Flags that control the return stuff }
    lpfn:           Pointer;//-TFNBFFCallBack;
    lParam:         LPARAM;     { extra info that's passed back in callbacks }
    iImage:         Integer;    { output var: where to return the Image index. }
  end;
  
  TBrowseInfoW = record
    hwndOwner:      HWND;
    pidlRoot:       PItemIDList;
    pszDisplayName: PWideChar;  { Return display name of item selected. }
    lpszTitle:      PWideChar;  { text to go in the banner over the tree. }
    ulFlags:        UINT;       { Flags that control the return stuff }
    lpfn:           Pointer;//-TFNBFFCallBack;
    lParam:         LPARAM;     { extra info that's passed back in callbacks }
    iImage:         Integer;    { output var: where to return the Image index. }
  end;
  TBrowseInfo = {$IFDEF UNICODE_CTRLS}TBrowseInfoW{$ELSE}TBrowseInfoA{$ENDIF};

  PSHFileOpStruct = ^TSHFileOpStruct;
  TSHFileOpStruct = {packed} record
    Wnd:                   HWND;
    wFunc:                 UINT;
    pFrom:                 PKOLChar;
    pTo:                   PKOLChar;
    fFlags:                Word; //-FILEOP_FLAGS
    fAnyOperationsAborted: BOOL;
    hNameMappings:         Pointer;
    lpszProgressTitle:     PKOLChar; { only used if FOF_SIMPLEPROGRESS }
  end;

  PNotifyIconData = ^TNotifyIconData;
  TNotifyIconData = record
    cbSize:           DWORD;
    Wnd:              HWND;
    uID:              UINT;
    uFlags:           UINT;
    uCallbackMessage: UINT;
    hIcon:            HICON;
    szTip:            array[0..63] of KOLChar;
  end;

  POpenFilename = ^TOpenFilename;
  TOpenFilename = {packed} record
    lStructSize:  DWORD;
    hWndOwner:    HWND;
    hInstance:    HINST;
    lpstrFilter:        PKOLChar;
    lpstrCustomFilter:  PKOLChar;
    nMaxCustFilter:     DWORD;
    nFilterIndex:       DWORD;
    lpstrFile:          PKOLChar;
    nMaxFile:           DWORD;
    lpstrFileTitle:     PKOLChar;
    nMaxFileTitle:      DWORD;
    lpstrInitialDir:    PKOLChar;
    lpstrTitle:         PKOLChar;
    Flags:              DWORD;
    nFileOffset:        Word;
    nFileExtension:     Word;
    lpstrDefExt:        PKOLChar;
    lCustData:          LPARAM;
    lpfnHook:           function(Wnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): PtrUInt; stdcall;
    lpTemplateName:     PKOLChar;
    //---------- added from Windows2000:
    pvReserved:         Pointer;
    dwReserved:         DWORD;
    FlagsEx:            DWORD;
  end;

  PChooseFont = ^TChooseFont;
  TChooseFont = packed record
    lStructSize:    DWORD;
    hWndOwner:      HWnd;       { caller's window handle }
    hDC:            HDC;        { printer DC/IC or nil }
    lpLogFont:      PLogFont;   { pointer to a LOGFONT struct }
    iPointSize:     Integer;    { 10 * size in points of selected font }
    Flags:          DWORD;      { dialog flags }
    rgbColors:      COLORREF;   { returned text color }
    lCustData:      LPARAM;     { data passed to hook function }
    lpfnHook:       function(Wnd: HWND; Message: UINT; wParam: WPARAM; lParam: LPARAM): UINT; stdcall;
                                { pointer to hook function }
    lpTemplateName: PKOLChar;   { custom template name }
    hInstance:      HINST;      { instance handle of EXE that contains custom dialog template }
    lpszStyle:      PKOLChar;   { return the style field here must be lf_FaceSize or bigger }
    nFontType:      Word;       { same value reported to the EnumFonts
                                  call back with the extra fonttype_ bits added }
    wReserved:      Word;
    nSizeMin:       Integer;    { minimum point size allowed and }
    nSizeMax:       Integer;    { maximum point size allowed if cf_LimitSize is used }
  end;

  PChooseColor = ^TChooseColor;
  TChooseColor = {packed} record
    lStructSize: DWORD;
    hWndOwner:      HWND;
    hInstance:      HWND;
    rgbResult:      COLORREF;
    lpCustColors:   ^COLORREF;
    Flags:          DWORD;
    lCustData:      LPARAM;
    lpfnHook:       function(Wnd: HWND; Message: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
    lpTemplateName: PKOLChar;
  end;

{$IFDEF UNICODE_CTRLS}
  MakeIntAtom = MakeIntAtomW;
  PRecoveryAgentInformation = PRecoveryAgentInformationW;
  TRecoveryAgentInformation = TRecoveryAgentInformationW;
  RECOVERY_AGENT_INFORMATION = RECOVERY_AGENT_INFORMATIONW;
  PWin32FindData = PWin32FindDataW;
  TWin32FindData = TWin32FindDataW;
  PHWProfileInfo = PHWProfileInfoW;
  THWProfileInfo = THWProfileInfoW;
  POSVersionInfo = POSVersionInfoW;
  TOSVersionInfo = TOSVersionInfoW;
  PLogColorSpace = PLogColorSpaceW;
  TLogColorSpace = TLogColorSpaceW;
  PTextMetric = PTextMetricW;
  tagTEXTMETRIC = tagTEXTMETRICW;
  TTextMetric = TTextMetricW;
  TEXTMETRIC = TEXTMETRICW;
  PNewTextMetric = PNewTextMetricW;
  TNewTextMetric = TNewTextMetricW;
  NEWTEXTMETRIC = NEWTEXTMETRICW;
  PNewTextMetricEx = PNewTextMetricExW;
  PLogFont = PLogFontW;
  TLogFont = TLogFontW;
  PEnumLogFont = PEnumLogFontW;
  TEnumLogFont = TEnumLogFontW;
  ENUMLOGFONT = ENUMLOGFONTW;
  PEnumLogFontEx = PEnumLogFontExW;
  TEnumLogFontEx = TEnumLogFontExW;
  ENUMLOGFONTEX = ENUMLOGFONTEXW;
  PExtLogFont = PExtLogFontW;
  tagEXTLOGFONT = tagEXTLOGFONTW;
  TExtLogFont = TExtLogFontW;
  EXTLOGFONT = EXTLOGFONTW;
  PDeviceMode = PDeviceModeW;
  TDeviceMode = TDeviceModeW;
  DEVMODE = DEVMODEW;
  PDisplayDevice = PDisplayDeviceW;
  TDisplayDevice = TDisplayDeviceW;
  POutlineTextmetric = POutlineTextmetricW;
  TOutlineTextmetric = TOutlineTextmetricW;
  OUTLINETEXTMETRIC = OUTLINETEXTMETRICW;
  PPolyText = PPolyTextW;
  tagPOLYTEXT = tagPOLYTEXTW;
  POLYTEXT = POLYTEXTW;
  TPolyText = TPolyTextW;
  PGCPResults = PGCPResultsW;
  TGCPResults = TGCPResultsW;
  GCP_RESULTS = GCP_RESULTSW;
  TFNOldFontEnumProc = TFNOldFontEnumProcW;
  TFNFontEnumProc = TFNFontEnumProcW;
  PAxisInfo = PAxisInfoW;
  PAxesList = PAxesListW;
  PEnumLogFontExDV = PEnumLogFontExDVW;
  PEnumTextMetric = PEnumTextMetricW;
  PDocInfo = PDocInfoW;
  TDocInfo = TDocInfoW;
  DOCINFO = DOCINFOW;
  MakeIntResource = MakeIntResourceW;
  PCreateStruct = PCreateStructW;
  TCreateStruct = TCreateStructW;
  CREATESTRUCT = CREATESTRUCTW;
  PWndClassEx = PWndClassExW;
  TWndClassEx = TWndClassExW;
  WNDCLASSEX = WNDCLASSEXW;
  PWndClass = PWndClassW;
  TWndClass = TWndClassW;
  WNDCLASS = WNDCLASSW;
  PMsgBoxParams = PMsgBoxParamsW;
  TMsgBoxParams = TMsgBoxParamsW;
  MSGBOXPARAMS = MSGBOXPARAMSW;
  PMDICreateStruct = PMDICreateStructW;
  TMDICreateStruct = TMDICreateStructW;
  PMultiKeyHelp = PMultiKeyHelpW;
  TMultiKeyHelp = TMultiKeyHelpW;
  MULTIKEYHELP = MULTIKEYHELPW;
  PHelpWinInfo = PHelpWinInfoW;
  THelpWinInfo = THelpWinInfoW;
  HELPWININFO = HELPWININFOW;
  PNonClientMetrics = PNonClientMetricsW;
  TNonClientMetrics = TNonClientMetricsW;
  NONCLIENTMETRICS = NONCLIENTMETRICSW;
  PIconMetrics = PIconMetricsW;
  TIconMetrics = TIconMetricsW;
  ICONMETRICS = ICONMETRICSW;
  PSerialKeys = PSerialKeysW;
  TSerialKeys = TSerialKeysW;
  SERIALKEYS = SERIALKEYSW;
  PHighContrast = PHighContrastW;
  THighContrast = THighContrastW;
  HIGHCONTRAST = HIGHCONTRASTW;
  PSoundsEntry = PSoundsEntryW;
  TSoundsEntry = TSoundsEntryW;
  SOUNDSENTRY = SOUNDSENTRYW;
  PNumberFmt = PNumberFmtW;
  TNumberFmt = TNumberFmtW;
  NUMBERFMT = NUMBERFMTW;
  PCurrencyFmt = PCurrencyFmtW;
  _currencyfmt = _currencyfmtW;
  TCurrencyFmt = TCurrencyFmtW;
  CURRENCYFMT = CURRENCYFMTW;
  PPValue = PPValueW;
  pvalue = pvalueW;
  TPValue = TPValueW;
  PValueEnt = PValueEntW;
  TValueEnt = TValueEntW;
  VALENT = VALENTW;
  PNetResource = PNetResourceW;
  TNetResource = TNetResourceW;
  NETRESOURCE = NETRESOURCEW;
  PDiscDlgStruct = PDiscDlgStructW;
  _DISCDLGSTRUCT = _DISCDLGSTRUCTW;
  TDiscDlgStruct = TDiscDlgStructW;
  DISCDLGSTRUCT = DISCDLGSTRUCTW;
  PUniversalNameInfo = PUniversalNameInfoW;
  TUniversalNameInfo = TUniversalNameInfoW;
  UNIVERSAL_NAME_INFO = UNIVERSAL_NAME_INFOW;
  PRemoteNameInfo = PRemoteNameInfoW;
  TRemoteNameInfo = TRemoteNameInfoW;
  REMOTE_NAME_INFO = REMOTE_NAME_INFOW;
{$ELSE}
  MakeIntAtom = MakeIntAtomA;
  PWin32FindData = PWin32FindDataA;
  TWin32FindData = TWin32FindDataA;
  PHWProfileInfo = PHWProfileInfoA;
  THWProfileInfo = THWProfileInfoA;
  POSVersionInfo = POSVersionInfoA;
  TOSVersionInfo = TOSVersionInfoA;
  PLogColorSpace = PLogColorSpaceA;
  TLogColorSpace = TLogColorSpaceA;
  PLogFont = PLogFontA;
  TLogFont = TLogFontA;
  PDeviceMode = PDeviceModeA;
  TDeviceMode = TDeviceModeA;
  TFNOldFontEnumProc = TFNOldFontEnumProcA;
  TFNFontEnumProc = TFNFontEnumProcA;
  MakeIntResource = PAnsiChar; // MakeIntResourceA;
  //PMenuItemInfo = PMenuItemInfoA;
  //TMenuItemInfo = TMenuItemInfoA;
  //MENUITEMINFO = MENUITEMINFOA;
  PMsgBoxParams = PMsgBoxParamsA;
  TMsgBoxParams = TMsgBoxParamsA;
  PMsgBoxParamsA = ^TMsgBoxParamsA;
  PMsgBoxParamsW = ^TMsgBoxParamsW;
  // tagMSGBOXPARAMSA}
  tagMSGBOXPARAMSA = {packed} record
    cbSize: UINT;
    hwndOwner: HWND;
    hInstance: HINST;
    lpszText: PAnsiChar;
    lpszCaption: PAnsiChar;
    dwStyle: DWORD;
    lpszIcon: PAnsiChar;
    dwContextHelpId: DWORD;
    lpfnMsgBoxCallback: TPRMsgBoxCallback;
    dwLanguageId: DWORD;
  end;
  // tagMSGBOXPARAMSW}
  tagMSGBOXPARAMSW = {packed} record
    cbSize: UINT;
    hwndOwner: HWND;
    hInstance: HINST;
    lpszText: PWideChar;
    lpszCaption: PWideChar;
    dwStyle: DWORD;
    lpszIcon: PWideChar;
    dwContextHelpId: DWORD;
    lpfnMsgBoxCallback: TPRMsgBoxCallback;
    dwLanguageId: DWORD;
  end;
  // tagMSGBOXPARAMS}
  tagMSGBOXPARAMS = tagMSGBOXPARAMSA;
  TMsgBoxParamsA = tagMSGBOXPARAMSA;
  TMsgBoxParamsW = tagMSGBOXPARAMSW;
  // MSGBOXPARAMSA}
  MSGBOXPARAMSA = tagMSGBOXPARAMSA;
  // MSGBOXPARAMSW}
  MSGBOXPARAMSW = tagMSGBOXPARAMSW;
  // MSGBOXPARAMS}
  MSGBOXPARAMS = MSGBOXPARAMSA;
  PMDICreateStruct = PMDICreateStructA;
  TMDICreateStruct = TMDICreateStructA;
  PMultiKeyHelp = PMultiKeyHelpA;
  TMultiKeyHelp = TMultiKeyHelpA;
  // HELPPOLY}
  HELPPOLY = DWORD;
  PMultiKeyHelpA = ^TMultiKeyHelpA;
  PMultiKeyHelpW = ^TMultiKeyHelpW;
  // tagMULTIKEYHELPA}
  tagMULTIKEYHELPA = record
    mkSize: DWORD;
    mkKeylist: AnsiChar;
    szKeyphrase: array[0..0] of AnsiChar;
  end;
  // tagMULTIKEYHELPW}
  tagMULTIKEYHELPW = record
    mkSize: DWORD;
    mkKeylist: WideChar;
    szKeyphrase: array[0..0] of WideChar;
  end;
  // tagMULTIKEYHELP}
  tagMULTIKEYHELP = tagMULTIKEYHELPA;
  TMultiKeyHelpA = tagMULTIKEYHELPA;
  TMultiKeyHelpW = tagMULTIKEYHELPW;
  // MULTIKEYHELPA}
  MULTIKEYHELPA = tagMULTIKEYHELPA;
  // MULTIKEYHELPW}
  MULTIKEYHELPW = tagMULTIKEYHELPW;
  // MULTIKEYHELP}
  MULTIKEYHELP = MULTIKEYHELPA;
  PHelpWinInfoA = ^THelpWinInfoA;
  PHelpWinInfoW = ^THelpWinInfoW;
  PHelpWinInfo = PHelpWinInfoA;
  // tagHELPWININFOA}
  tagHELPWININFOA = record
    wStructSize: Integer;
    x: Integer;
    y: Integer;
    dx: Integer;
    dy: Integer;
    wMax: Integer;
    rgchMember: array[0..1] of AnsiChar;
  end;
  // tagHELPWININFOW}
  tagHELPWININFOW = record
    wStructSize: Integer;
    x: Integer;
    y: Integer;
    dx: Integer;
    dy: Integer;
    wMax: Integer;
    rgchMember: array[0..1] of WideChar;
  end;
  // tagHELPWININFO}
  tagHELPWININFO = tagHELPWININFOA;
  THelpWinInfoA = tagHELPWININFOA;
  THelpWinInfoW = tagHELPWININFOW;
  THelpWinInfo = THelpWinInfoA;
  // HELPWININFOA}
  HELPWININFOA = tagHELPWININFOA;
  // HELPWININFOW}
  HELPWININFOW = tagHELPWININFOW;
  // HELPWININFO}
  HELPWININFO = HELPWININFOA;
  // tagNONCLIENTMETRICSA}
  tagNONCLIENTMETRICSA = {packed} record
    cbSize: UINT;
    iBorderWidth: Integer;
    iScrollWidth: Integer;
    iScrollHeight: Integer;
    iCaptionWidth: Integer;
    iCaptionHeight: Integer;
    lfCaptionFont: TLogFontA;
    iSmCaptionWidth: Integer;
    iSmCaptionHeight: Integer;
    lfSmCaptionFont: TLogFontA;
    iMenuWidth: Integer;
    iMenuHeight: Integer;
    lfMenuFont: TLogFontA;
    lfStatusFont: TLogFontA;
    lfMessageFont: TLogFontA;
  end;
  // tagNONCLIENTMETRICSW}
  tagNONCLIENTMETRICSW = {packed} record
    cbSize: UINT;
    iBorderWidth: Integer;
    iScrollWidth: Integer;
    iScrollHeight: Integer;
    iCaptionWidth: Integer;
    iCaptionHeight: Integer;
    lfCaptionFont: TLogFontW;
    iSmCaptionWidth: Integer;
    iSmCaptionHeight: Integer;
    lfSmCaptionFont: TLogFontW;
    iMenuWidth: Integer;
    iMenuHeight: Integer;
    lfMenuFont: TLogFontW;
    lfStatusFont: TLogFontW;
    lfMessageFont: TLogFontW;
  end;
  // tagNONCLIENTMETRICS}
  tagNONCLIENTMETRICS = tagNONCLIENTMETRICSA;
  TNonClientMetricsA = tagNONCLIENTMETRICSA;
  TNonClientMetricsW = tagNONCLIENTMETRICSW;
  PNonClientMetricsA = ^TNonClientMetricsA;
  PNonClientMetrics = PNonClientMetricsA;
  TNonClientMetrics = TNonClientMetricsA;
  PNonClientMetricsW = ^TNonClientMetricsW;
  // NONCLIENTMETRICSA}
  NONCLIENTMETRICSA = tagNONCLIENTMETRICSA;
  // NONCLIENTMETRICSW}
  NONCLIENTMETRICSW = tagNONCLIENTMETRICSW;
  // NONCLIENTMETRICS}
  NONCLIENTMETRICS = NONCLIENTMETRICSA;
  // tagICONMETRICSA}
  tagICONMETRICSA = {packed} record
    cbSize: UINT;
    iHorzSpacing: Integer;
    iVertSpacing: Integer;
    iTitleWrap: Integer;
    lfFont: TLogFontA;
  end;
  // tagICONMETRICSW}
  tagICONMETRICSW = {packed} record
    cbSize: UINT;
    iHorzSpacing: Integer;
    iVertSpacing: Integer;
    iTitleWrap: Integer;
    lfFont: TLogFontW;
  end;
  // tagICONMETRICS}
  tagICONMETRICS = tagICONMETRICSA;
  TIconMetricsA = tagICONMETRICSA;
  TIconMetricsW = tagICONMETRICSW;
  PIconMetricsA = ^TIconMetricsA;
  PIconMetricsW = ^TIconMetricsW;
  PIconMetrics = PIconMetricsA;
  TIconMetrics = TIconMetricsA;
  // ICONMETRICSA}
  ICONMETRICSA = tagICONMETRICSA;
  // ICONMETRICSW}
  ICONMETRICSW = tagICONMETRICSW;
  // ICONMETRICS}
  ICONMETRICS = ICONMETRICSA;
  PSerialKeys = PSerialKeysA;
  TSerialKeys = TSerialKeysA;
  PSerialKeysA = ^TSerialKeysA;
  PSerialKeysW = ^TSerialKeysW;
  // tagSERIALKEYSA}
  tagSERIALKEYSA = {packed} record
    cbSize: UINT;
    dwFlags: DWORD;
    lpszActivePort: PAnsiChar;
    lpszPort: PAnsiChar;
    iBaudRate: UINT;
    iPortState: UINT;
    iActive: UINT;
  end;
  // tagSERIALKEYSW}
  tagSERIALKEYSW = {packed} record
    cbSize: UINT;
    dwFlags: DWORD;
    lpszActivePort: PWideChar;
    lpszPort: PWideChar;
    iBaudRate: UINT;
    iPortState: UINT;
    iActive: UINT;
  end;
  // tagSERIALKEYS}
  tagSERIALKEYS = tagSERIALKEYSA;
  TSerialKeysA = tagSERIALKEYSA;
  TSerialKeysW = tagSERIALKEYSW;
  // SERIALKEYSA}
  SERIALKEYSA = tagSERIALKEYSA;
  // SERIALKEYSW}
  SERIALKEYSW = tagSERIALKEYSW;
  // SERIALKEYS}
  SERIALKEYS = SERIALKEYSA;
  PHighContrast = PHighContrastA;
  THighContrast = THighContrastA;
  PHighContrastA = ^THighContrastA;
  PHighContrastW = ^THighContrastW;
  // tagHIGHCONTRASTA}
  tagHIGHCONTRASTA = {packed} record
    cbSize: UINT;
    dwFlags: DWORD;
    lpszDefaultScheme: PAnsiChar;
  end;
  // tagHIGHCONTRASTW}
  tagHIGHCONTRASTW = {packed} record
    cbSize: UINT;
    dwFlags: DWORD;
    lpszDefaultScheme: PWideChar;
  end;
  // tagHIGHCONTRAST}
  tagHIGHCONTRAST = tagHIGHCONTRASTA;
  THighContrastA = tagHIGHCONTRASTA;
  THighContrastW = tagHIGHCONTRASTW;
  // HIGHCONTRASTA}
  HIGHCONTRASTA = tagHIGHCONTRASTA;
  // HIGHCONTRASTW}
  HIGHCONTRASTW = tagHIGHCONTRASTW;
  // HIGHCONTRAST}
  HIGHCONTRAST = HIGHCONTRASTA;
  PSoundsEntry = PSoundsEntryA;
  TSoundsEntry = TSoundsEntryA;
  PSoundsEntryA = ^TSoundsEntryA;
  PSoundsEntryW = ^TSoundsEntryW;
  // tagSOUNDSENTRYA}
  tagSOUNDSENTRYA = {packed} record
    cbSize: UINT;
    dwFlags: DWORD;
    iFSTextEffect: DWORD;
    iFSTextEffectMSec: DWORD;
    iFSTextEffectColorBits: DWORD;
    iFSGrafEffect: DWORD;
    iFSGrafEffectMSec: DWORD;
    iFSGrafEffectColor: DWORD;
    iWindowsEffect: DWORD;
    iWindowsEffectMSec: DWORD;
    lpszWindowsEffectDLL: PAnsiChar;
    iWindowsEffectOrdinal: DWORD;
  end;
  // tagSOUNDSENTRYW}
  tagSOUNDSENTRYW = {packed} record
    cbSize: UINT;
    dwFlags: DWORD;
    iFSTextEffect: DWORD;
    iFSTextEffectMSec: DWORD;
    iFSTextEffectColorBits: DWORD;
    iFSGrafEffect: DWORD;
    iFSGrafEffectMSec: DWORD;
    iFSGrafEffectColor: DWORD;
    iWindowsEffect: DWORD;
    iWindowsEffectMSec: DWORD;
    lpszWindowsEffectDLL: PWideChar;
    iWindowsEffectOrdinal: DWORD;
  end;
  // tagSOUNDSENTRY}
  tagSOUNDSENTRY = tagSOUNDSENTRYA;
  TSoundsEntryA = tagSOUNDSENTRYA;
  TSoundsEntryW = tagSOUNDSENTRYW;
  // SOUNDSENTRYA}
  SOUNDSENTRYA = tagSOUNDSENTRYA;
  // SOUNDSENTRYW}
  SOUNDSENTRYW = tagSOUNDSENTRYW;
  // SOUNDSENTRY}
  SOUNDSENTRY = SOUNDSENTRYA;
  PNumberFmt = PNumberFmtA;
  TNumberFmt = TNumberFmtA;
  PNumberFmtA = ^TNumberFmtA;
  PNumberFmtW = ^TNumberFmtW;
  // _numberfmtA}
  _numberfmtA = {packed} record
    NumDigits: UINT;        { number of decimal digits }
    LeadingZero: UINT;      { if leading zero in decimal fields }
    Grouping: UINT;         { group size left of decimal }
    lpDecimalSep: PAnsiChar;   { ptr to decimal separator AnsiString }
    lpThousandSep: PAnsiChar;  { ptr to thousand separator AnsiString }
    NegativeOrder: UINT;    { negative number ordering }
  end;
  // _numberfmtW}
  _numberfmtW = {packed} record
    NumDigits: UINT;        { number of decimal digits }
    LeadingZero: UINT;      { if leading zero in decimal fields }
    Grouping: UINT;         { group size left of decimal }
    lpDecimalSep: PWideChar;   { ptr to decimal separator WideString }
    lpThousandSep: PWideChar;  { ptr to thousand separator WideString }
    NegativeOrder: UINT;    { negative number ordering }
  end;
  // _numberfmt}
  _numberfmt = _numberfmtA;
  TNumberFmtA = _numberfmtA;
  TNumberFmtW = _numberfmtW;
  // NUMBERFMTA}
  NUMBERFMTA = _numberfmtA;
  // NUMBERFMTW}
  NUMBERFMTW = _numberfmtW;
  // NUMBERFMT}
  NUMBERFMT = NUMBERFMTA;
  PCurrencyFmt = PCurrencyFmtA;
  PCurrencyFmtA = ^TCurrencyFmtA;
  PCurrencyFmtW = ^TCurrencyFmtW;
  // _currencyfmtA}
  _currencyfmtA = {packed} record
    NumDigits: UINT;           { number of decimal digits }
    LeadingZero: UINT;         { if leading zero in decimal fields }
    Grouping: UINT;            { group size left of decimal }
    lpDecimalSep: PAnsiChar;      { ptr to decimal separator AnsiString }
    lpThousandSep: PAnsiChar;     { ptr to thousand separator AnsiString }
    NegativeOrder: UINT;       { negative currency ordering }
    PositiveOrder: UINT;       { positive currency ordering }
    lpCurrencySymbol: PAnsiChar;  { ptr to currency symbol AnsiString }
  end;
  // _currencyfmtW}
  _currencyfmtW = {packed} record
    NumDigits: UINT;           { number of decimal digits }
    LeadingZero: UINT;         { if leading zero in decimal fields }
    Grouping: UINT;            { group size left of decimal }
    lpDecimalSep: PWideChar;      { ptr to decimal separator WideString }
    lpThousandSep: PWideChar;     { ptr to thousand separator WideString }
    NegativeOrder: UINT;       { negative currency ordering }
    PositiveOrder: UINT;       { positive currency ordering }
    lpCurrencySymbol: PWideChar;  { ptr to currency symbol WideString }
  end;
  // _currencyfmt}
  _currencyfmt = _currencyfmtA;
  TCurrencyFmtA = _currencyfmtA;
  TCurrencyFmtW = _currencyfmtW;
  TCurrencyFmt = TCurrencyFmtA;
  // CURRENCYFMTA}
  CURRENCYFMTA = _currencyfmtA;
  // CURRENCYFMTW}
  CURRENCYFMTW = _currencyfmtW;
  // CURRENCYFMT}
  CURRENCYFMT = CURRENCYFMTA;
  PPValue = PPValueA;
{ Provider supplied value/context.}
  PPValueA = ^TPValueA;
  PPValueW = ^TPValueW;
  // pvalueA}
  pvalueA = {packed} record
    pv_valuename: PAnsiChar;           { The value name pointer }
    pv_valuelen: BOOL;
    pv_value_context: Pointer;
    pv_type: DWORD;
  end;
  // pvalueW}
  pvalueW = {packed} record
    pv_valuename: PWideChar;           { The value name pointer }
    pv_valuelen: BOOL;
    pv_value_context: Pointer;
    pv_type: DWORD;
  end;
  // pvalue}
  pvalue = pvalueA;
  TPValueA = pvalueA;
  TPValueW = pvalueW;
  TPValue = TPValueA;
  PValueEnt = PValueEntA;
  TValueEnt = TValueEntA;
  PValueEntA = ^TValueEntA;
  PValueEntW = ^TValueEntW;
  // value_entA}
  value_entA = {packed} record
    ve_valuename: PAnsiChar;
    ve_valuelen: DWORD;
    ve_valueptr: DWORD;
    ve_type: DWORD;
  end;
  // value_entW}
  value_entW = {packed} record
    ve_valuename: PWideChar;
    ve_valuelen: DWORD;
    ve_valueptr: DWORD;
    ve_type: DWORD;
  end;
  // value_ent}
  value_ent = value_entA;
  TValueEntA = value_entA;
  TValueEntW = value_entW;
  // VALENTA}
  VALENTA = value_entA;
  // VALENTW}
  VALENTW = value_entW;
  // VALENT}
  VALENT = VALENTA;
  TValEnt = TValueEnt;
  PValEnt = PValueEnt;
  PNetResource = PNetResourceA;
  TNetResource = TNetResourceA;
  PNetResourceA = ^TNetResourceA;
  PNetResourceW = ^TNetResourceW;
  // _NETRESOURCEA}
  _NETRESOURCEA = {packed} record
    dwScope: DWORD;
    dwType: DWORD;
    dwDisplayType: DWORD;
    dwUsage: DWORD;
    lpLocalName: PAnsiChar;
    lpRemoteName: PAnsiChar;
    lpComment: PAnsiChar;
    lpProvider: PAnsiChar;
  end;
  // _NETRESOURCEW}
  _NETRESOURCEW = {packed} record
    dwScope: DWORD;
    dwType: DWORD;
    dwDisplayType: DWORD;
    dwUsage: DWORD;
    lpLocalName: PWideChar;
    lpRemoteName: PWideChar;
    lpComment: PWideChar;
    lpProvider: PWideChar;
  end;
  // _NETRESOURCE}
  _NETRESOURCE = _NETRESOURCEA;
  TNetResourceA = _NETRESOURCEA;
  TNetResourceW = _NETRESOURCEW;
  // NETRESOURCEA}
  NETRESOURCEA = _NETRESOURCEA;
  // NETRESOURCEW}
  NETRESOURCEW = _NETRESOURCEW;
  // NETRESOURCE}
  NETRESOURCE = NETRESOURCEA;
  PDiscDlgStruct = PDiscDlgStructA;
  PDiscDlgStructA = ^TDiscDlgStructA;
  PDiscDlgStructW = ^TDiscDlgStructW;
  // _DISCDLGSTRUCTA}
  _DISCDLGSTRUCTA = {packed} record
    cbStructure: DWORD;       { size of this structure in bytes }
    hwndOwner: HWND;          { owner window for the dialog }
    lpLocalName: PAnsiChar;       { local device name }
    lpRemoteName: PAnsiChar;      { network resource name }
    dwFlags: DWORD;
  end;
  // _DISCDLGSTRUCTW}
  _DISCDLGSTRUCTW = {packed} record
    cbStructure: DWORD;       { size of this structure in bytes }
    hwndOwner: HWND;          { owner window for the dialog }
    lpLocalName: PWideChar;       { local device name }
    lpRemoteName: PWideChar;      { network resource name }
    dwFlags: DWORD;
  end;
  // _DISCDLGSTRUCT}
  _DISCDLGSTRUCT = _DISCDLGSTRUCTA;
  TDiscDlgStructA = _DISCDLGSTRUCTA;
  TDiscDlgStructW = _DISCDLGSTRUCTW;
  TDiscDlgStruct = TDiscDlgStructA;
  // DISCDLGSTRUCTA}
  DISCDLGSTRUCTA = _DISCDLGSTRUCTA;
  // DISCDLGSTRUCTW}
  DISCDLGSTRUCTW = _DISCDLGSTRUCTW;
  // DISCDLGSTRUCT}
  DISCDLGSTRUCT = DISCDLGSTRUCTA;
  PUniversalNameInfo = PUniversalNameInfoA;
  TUniversalNameInfo = TUniversalNameInfoA;
  PUniversalNameInfoA = ^TUniversalNameInfoA;
  PUniversalNameInfoW = ^TUniversalNameInfoW;
  // _UNIVERSAL_NAME_INFOA}
  _UNIVERSAL_NAME_INFOA = {packed} record
    lpUniversalName: PAnsiChar;
  end;
  // _UNIVERSAL_NAME_INFOW}
  _UNIVERSAL_NAME_INFOW = {packed} record
    lpUniversalName: PWideChar;
  end;
  // _UNIVERSAL_NAME_INFO}
  _UNIVERSAL_NAME_INFO = _UNIVERSAL_NAME_INFOA;
  TUniversalNameInfoA = _UNIVERSAL_NAME_INFOA;
  TUniversalNameInfoW = _UNIVERSAL_NAME_INFOW;
  // UNIVERSAL_NAME_INFOA}
  UNIVERSAL_NAME_INFOA = _UNIVERSAL_NAME_INFOA;
  // UNIVERSAL_NAME_INFOW}
  UNIVERSAL_NAME_INFOW = _UNIVERSAL_NAME_INFOW;
  // UNIVERSAL_NAME_INFO}
  UNIVERSAL_NAME_INFO = UNIVERSAL_NAME_INFOA;
  PRemoteNameInfo = PRemoteNameInfoA;
  TRemoteNameInfo = TRemoteNameInfoA;
  PRemoteNameInfoA = ^TRemoteNameInfoA;
  PRemoteNameInfoW = ^TRemoteNameInfoW;
  // _REMOTE_NAME_INFOA}
  _REMOTE_NAME_INFOA = {packed} record
    lpUniversalName: PAnsiChar;
    lpConnectionName: PAnsiChar;
    lpRemainingPath: PAnsiChar;
  end;
  // _REMOTE_NAME_INFOW}
  _REMOTE_NAME_INFOW = {packed} record
    lpUniversalName: PWideChar;
    lpConnectionName: PWideChar;
    lpRemainingPath: PWideChar;
  end;
  // _REMOTE_NAME_INFO}
  _REMOTE_NAME_INFO = _REMOTE_NAME_INFOA;
  TRemoteNameInfoA = _REMOTE_NAME_INFOA;
  TRemoteNameInfoW = _REMOTE_NAME_INFOW;
  // REMOTE_NAME_INFOA}
  REMOTE_NAME_INFOA = _REMOTE_NAME_INFOA;
  // REMOTE_NAME_INFOW}
  REMOTE_NAME_INFOW = _REMOTE_NAME_INFOW;
  // REMOTE_NAME_INFO}
  REMOTE_NAME_INFO = REMOTE_NAME_INFOA;
  AUDIT_EVENT_TYPE = DWORD;
  PObjectTypeList = ^TObjectTypeList;
  _OBJECT_TYPE_LIST = record
    Level: WORD;
    Sbz: WORD;
    ObjectType: PGUID;
  end;
  TObjectTypeList = _OBJECT_TYPE_LIST;
  OBJECT_TYPE_LIST = _OBJECT_TYPE_LIST;
  { Alt-Tab Switch window information. }
  PAltTabInfo = ^TAltTabInfo;
  tagALTTABINFO = {packed} record
    cbSize: DWORD;
    cItems: Integer;
    cColumns: Integer;
    cRows: Integer;
    iColFocus: Integer;
    iRowFocus: Integer;
    cxItem: Integer;
    cyItem: Integer;
    ptStart: TPoint;
  end;
  TAltTabInfo = tagALTTABINFO;
{$ENDIF UNICODE_CTRLS}

/// consts ///
const
  IDC_ARROW       = MakeIntResource(32512);
  IDC_IBEAM       = MakeIntResource(32513);
  IDC_WAIT        = MakeIntResource(32514);
  IDC_CROSS       = MakeIntResource(32515);
  IDC_UPARROW     = MakeIntResource(32516);
  IDC_SIZE        = MakeIntResource(32640);
  IDC_ICON        = MakeIntResource(32641);
  IDC_SIZENWSE    = MakeIntResource(32642);
  IDC_SIZENESW    = MakeIntResource(32643);
  IDC_SIZEWE      = MakeIntResource(32644);
  IDC_SIZENS      = MakeIntResource(32645);
  IDC_SIZEALL     = MakeIntResource(32646);
  IDC_NO          = MakeIntResource(32648);
  IDC_HAND        = MakeIntResource(32649);
  IDC_APPSTARTING = MakeIntResource(32650);
  IDC_HELP        = MakeIntResource(32651);
  RT_CURSOR       = PKOLChar(1);
  RT_BITMAP       = PKOLChar(2);
  RT_ICON         = PKOLChar(3);
  RT_MENU         = PKOLChar(4);
  RT_DIALOG       = PKOLChar(5);
  RT_STRING       = PKOLChar(6);
  RT_FONTDIR      = PKOLChar(7);
  RT_FONT         = PKOLChar(8);
  RT_ACCELERATOR  = PKOLChar(9);
  RT_RCDATA       = PKOLChar(10);
  RT_MESSAGETABLE = PKOLChar(11);
  RT_VERSION      = PKOLChar(16);
  RT_DLGINCLUDE   = PKOLChar(17);
  RT_PLUGPLAY     = PKOLChar(19);
  RT_VXD          = PKOLChar(20);
  RT_ANICURSOR    = PKOLChar(21);
  RT_ANIICON      = PKOLChar(22);

/// externals ///
function AbortSystemShutdown(lpMachineName: PKOLChar): BOOL; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'AbortSystemShutdownW'{$ELSE}'AbortSystemShutdownA'{$ENDIF};
function AccessCheckAndAuditAlarm(SubsystemName: PKOLChar; HandleId: Pointer; ObjectTypeName, ObjectName: PKOLChar; SecurityDescriptor: PSecurityDescriptor; DesiredAccess: DWORD; const GenericMapping: TGenericMapping;  ObjectCreation: BOOL; var GrantedAccess: DWORD; var AccessStatus, pfGenerateOnClose: BOOL): BOOL; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'AccessCheckAndAuditAlarmW'{$ELSE}'AccessCheckAndAuditAlarmA'{$ENDIF};
function AccessCheckByTypeAndAuditAlarm(SubsystemName: PKOLChar; HandleId: Pointer; ObjectTypeName, ObjectName: PKOLChar; SecurityDescriptor: PSecurityDescriptor; PrincipalSelfSid: PSID; DesiredAccess: DWORD; AuditType: AUDIT_EVENT_TYPE; Flags: DWORD; ObjectTypeList: PObjectTypeList; ObjectTypeListLength: DWORD; const GenericMapping: TGenericMapping;  ObjectCreation: BOOL; var GrantedAccess: DWORD; var AccessStatus, pfGenerateOnClose: BOOL): BOOL; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'AccessCheckByTypeAndAuditAlarmW'{$ELSE}'AccessCheckByTypeAndAuditAlarmA'{$ENDIF};
function AccessCheckByTypeResultListAndAuditAlarm(SubsystemName: PKOLChar; HandleId: Pointer; ObjectTypeName, ObjectName: PKOLChar; SecurityDescriptor: PSecurityDescriptor; PrincipalSelfSid: PSID; DesiredAccess: DWORD; AuditType: AUDIT_EVENT_TYPE; Flags: DWORD; ObjectTypeList: PObjectTypeList; ObjectTypeListLength: DWORD; const GenericMapping: TGenericMapping; ObjectCreation: BOOL; var GrantedAccess: DWORD; var AccessStatusList: DWORD; var pfGenerateOnClose: BOOL): BOOL; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'AccessCheckByTypeResultListAndAuditAlarmW'{$ELSE}'AccessCheckByTypeResultListAndAuditAlarmA'{$ENDIF};
function BackupEventLog(hEventLog: THandle; lpBackupFileName: PKOLChar): BOOL; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'BackupEventLogW'{$ELSE}'BackupEventLogA'{$ENDIF};
function ClearEventLog(hEventLog: THandle; lpBackupFileName: PKOLChar): BOOL; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'ClearEventLogW'{$ELSE}'ClearEventLogA'{$ENDIF};
function CreateProcessAsUser(hToken: THandle; lpApplicationName: PKOLChar; lpCommandLine: PKOLChar; lpProcessAttributes: PSecurityAttributes; lpThreadAttributes: PSecurityAttributes; bInheritHandles: BOOL; dwCreationFlags: DWORD; lpEnvironment: Pointer; lpCurrentDirectory: PKOLChar; const lpStartupInfo: TStartupInfo; var lpProcessInformation: TProcessInformation): BOOL; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'CreateProcessAsUserW'{$ELSE}'CreateProcessAsUserA'{$ENDIF};
function GetCurrentHwProfile(var lpHwProfileInfo: THWProfileInfo): BOOL; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'GetCurrentHwProfileW'{$ELSE}'GetCurrentHwProfileA'{$ENDIF};
function GetFileSecurity(lpFileName: PKOLChar; RequestedInformation: SECURITY_INFORMATION; pSecurityDescriptor: PSecurityDescriptor; nLength: DWORD; var lpnLengthNeeded: DWORD): BOOL; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'GetFileSecurityW'{$ELSE}'GetFileSecurityA'{$ENDIF};
function GetUserName(lpBuffer: PKOLChar; var nSize: DWORD): BOOL; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'GetUserNameW'{$ELSE}'GetUserNameA'{$ENDIF};
function InitiateSystemShutdown(lpMachineName, lpMessage: PKOLChar; dwTimeout: DWORD; bForceAppsClosed, bRebootAfterShutdown: BOOL): BOOL; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'InitiateSystemShutdownW'{$ELSE}'InitiateSystemShutdownA'{$ENDIF};
function LogonUser(lpszUsername, lpszDomain, lpszPassword: PKOLChar; dwLogonType, dwLogonProvider: DWORD; var phToken: THandle): BOOL; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'LogonUserW'{$ELSE}'LogonUserA'{$ENDIF};
function LookupAccountName(lpSystemName, lpAccountName: PKOLChar; Sid: PSID; var cbSid: DWORD; ReferencedDomainName: PKOLChar; var cbReferencedDomainName: DWORD; var peUse: SID_NAME_USE): BOOL; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'LookupAccountNameW'{$ELSE}'LookupAccountNameA'{$ENDIF};
function LookupAccountSid(lpSystemName: PKOLChar; Sid: PSID; Name: PKOLChar; var cbName: DWORD; ReferencedDomainName: PKOLChar; var cbReferencedDomainName: DWORD; var peUse: SID_NAME_USE): BOOL; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'LookupAccountSidW'{$ELSE}'LookupAccountSidA'{$ENDIF};
function LookupPrivilegeDisplayName(lpSystemName, lpName: PKOLChar; lpDisplayName: PKOLChar; var cbDisplayName, lpLanguageId: DWORD): BOOL; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'LookupPrivilegeDisplayNameW'{$ELSE}'LookupPrivilegeDisplayNameA'{$ENDIF};
function LookupPrivilegeName(lpSystemName: PKOLChar; var lpLuid: TLargeInteger; lpName: PKOLChar; var cbName: DWORD): BOOL; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'LookupPrivilegeNameW'{$ELSE}'LookupPrivilegeNameA'{$ENDIF};
function LookupPrivilegeValue(lpSystemName, lpName: PKOLChar; var lpLuid: TLargeInteger): BOOL; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'LookupPrivilegeValueW'{$ELSE}'LookupPrivilegeValueA'{$ENDIF};
function ObjectCloseAuditAlarm(SubsystemName: PKOLChar; HandleId: Pointer; GenerateOnClose: BOOL): BOOL; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'ObjectCloseAuditAlarmW'{$ELSE}'ObjectCloseAuditAlarmA'{$ENDIF};
function ObjectDeleteAuditAlarm(SubsystemName: PKOLChar; HandleId: Pointer; GenerateOnClose: BOOL): BOOL; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'ObjectDeleteAuditAlarmW'{$ELSE}'ObjectDeleteAuditAlarmA'{$ENDIF};
function ObjectOpenAuditAlarm(SubsystemName: PKOLChar; HandleId: Pointer; ObjectTypeName: PKOLChar; ObjectName: PKOLChar; pSecurityDescriptor: PSecurityDescriptor; ClientToken: THandle; DesiredAccess, GrantedAccess: DWORD; var Privileges: TPrivilegeSet; ObjectCreation, AccessGranted: BOOL; var GenerateOnClose: BOOL): BOOL; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'ObjectOpenAuditAlarmW'{$ELSE}'ObjectOpenAuditAlarmA'{$ENDIF};
function ObjectPrivilegeAuditAlarm(SubsystemName: PKOLChar; HandleId: Pointer; ClientToken: THandle; DesiredAccess: DWORD; var Privileges: TPrivilegeSet; AccessGranted: BOOL): BOOL; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'ObjectPrivilegeAuditAlarmW'{$ELSE}'ObjectPrivilegeAuditAlarmA'{$ENDIF};
function OpenBackupEventLog(lpUNCServerName, lpFileName: PKOLChar): THandle; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'OpenBackupEventLogW'{$ELSE}'OpenBackupEventLogA'{$ENDIF};
function OpenEventLog(lpUNCServerName, lpSourceName: PKOLChar): THandle; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'OpenEventLogW'{$ELSE}'OpenEventLogA'{$ENDIF};
function PrivilegedServiceAuditAlarm(SubsystemName, ServiceName: PKOLChar; ClientToken: THandle; var Privileges: TPrivilegeSet; AccessGranted: BOOL): BOOL; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'PrivilegedServiceAuditAlarmW'{$ELSE}'PrivilegedServiceAuditAlarmA'{$ENDIF};
function ReadEventLog(hEventLog: THandle; dwReadFlags, dwRecordOffset: DWORD; lpBuffer: Pointer; nNumberOfBytesToRead: DWORD; var pnBytesRead, pnMinNumberOfBytesNeeded: DWORD): BOOL; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'ReadEventLogW'{$ELSE}'ReadEventLogA'{$ENDIF};
function RegConnectRegistry(lpMachineName: PKOLChar; hKey: HKEY; var phkResult: HKEY): Longint; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'RegConnectRegistryW'{$ELSE}'RegConnectRegistryA'{$ENDIF};
function RegCreateKey(hKey: HKEY; lpSubKey: PKOLChar; var phkResult: HKEY): Longint; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'RegCreateKeyW'{$ELSE}'RegCreateKeyA'{$ENDIF};
function RegCreateKeyEx(hKey: HKEY; lpSubKey: PKOLChar; Reserved: DWORD; lpClass: PKOLChar; dwOptions: DWORD; samDesired: REGSAM; lpSecurityAttributes: PSecurityAttributes; var phkResult: HKEY; lpdwDisposition: PDWORD): Longint; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'RegCreateKeyExW'{$ELSE}'RegCreateKeyExA'{$ENDIF};
function RegDeleteKey(hKey: HKEY; lpSubKey: PKOLChar): Longint; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'RegDeleteKeyW'{$ELSE}'RegDeleteKeyA'{$ENDIF};
function RegDeleteValue(hKey: HKEY; lpValueName: PKOLChar): Longint; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'RegDeleteValueW'{$ELSE}'RegDeleteValueA'{$ENDIF};
function RegEnumKeyEx(hKey: HKEY; dwIndex: DWORD; lpName: PKOLChar; var lpcbName: DWORD; lpReserved: Pointer; lpClass: PKOLChar; lpcbClass: PDWORD; lpftLastWriteTime: PFileTime): Longint; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'RegEnumKeyExW'{$ELSE}'RegEnumKeyExA'{$ENDIF};
function RegEnumKey(hKey: HKEY; dwIndex: DWORD; lpName: PKOLChar; cbName: DWORD): Longint; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'RegEnumKeyW'{$ELSE}'RegEnumKeyA'{$ENDIF};
function RegEnumValue(hKey: HKEY; dwIndex: DWORD; lpValueName: PKOLChar; var lpcbValueName: DWORD; lpReserved: Pointer; lpType: PDWORD; lpData: PByte; lpcbData: PDWORD): Longint; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'RegEnumValueW'{$ELSE}'RegEnumValueA'{$ENDIF};
function RegLoadKey(hKey: HKEY; lpSubKey, lpFile: PKOLChar): Longint; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'RegLoadKeyW'{$ELSE}'RegLoadKeyA'{$ENDIF};
function RegOpenKey(hKey: HKEY; lpSubKey: PKOLChar; var phkResult: HKEY): Longint; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'RegOpenKeyW'{$ELSE}'RegOpenKeyA'{$ENDIF};
function RegOpenKeyEx(hKey: HKEY; lpSubKey: PKOLChar; ulOptions: DWORD; samDesired: REGSAM; var phkResult: HKEY): Longint; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'RegOpenKeyExW'{$ELSE}'RegOpenKeyExA'{$ENDIF};
function RegQueryInfoKey(hKey: HKEY; lpClass: PKOLChar; lpcbClass: PDWORD; lpReserved: Pointer; lpcSubKeys, lpcbMaxSubKeyLen, lpcbMaxClassLen, lpcValues, lpcbMaxValueNameLen, lpcbMaxValueLen, lpcbSecurityDescriptor: PDWORD; lpftLastWriteTime: PFileTime): Longint; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'RegQueryInfoKeyW'{$ELSE}'RegQueryInfoKeyA'{$ENDIF};
function RegQueryMultipleValues(hKey: HKEY; var ValList; NumVals: DWORD; lpValueBuf: PKOLChar; var ldwTotsize: DWORD): Longint; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'RegQueryMultipleValuesW'{$ELSE}'RegQueryMultipleValuesA'{$ENDIF};
function RegQueryValue(hKey: HKEY; lpSubKey: PKOLChar; lpValue: PKOLChar; var lpcbValue: Longint): Longint; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'RegQueryValueW'{$ELSE}'RegQueryValueA'{$ENDIF};
function RegQueryValueEx(hKey: HKEY; lpValueName: PKOLChar; lpReserved: Pointer; lpType: PDWORD; lpData: PByte; lpcbData: PDWORD): Longint; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'RegQueryValueExW'{$ELSE}'RegQueryValueExA'{$ENDIF};
function RegReplaceKey(hKey: HKEY; lpSubKey: PKOLChar; lpNewFile: PKOLChar; lpOldFile: PKOLChar): Longint; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'RegReplaceKeyW'{$ELSE}'RegReplaceKeyA'{$ENDIF};
function RegRestoreKey(hKey: HKEY; lpFile: PKOLChar; dwFlags: DWORD): Longint; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'RegRestoreKeyW'{$ELSE}'RegRestoreKeyA'{$ENDIF};
function RegSaveKey(hKey: HKEY; lpFile: PKOLChar; lpSecurityAttributes: PSecurityAttributes): Longint; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'RegSaveKeyW'{$ELSE}'RegSaveKeyA'{$ENDIF};
function RegSetValue(hKey: HKEY; lpSubKey: PKOLChar; dwType: DWORD; lpData: PKOLChar; cbData: DWORD): Longint; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'RegSetValueW'{$ELSE}'RegSetValueA'{$ENDIF};
function RegSetValueEx(hKey: HKEY; lpValueName: PKOLChar; Reserved: DWORD; dwType: DWORD; lpData: Pointer; cbData: DWORD): Longint; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'RegSetValueExW'{$ELSE}'RegSetValueExA'{$ENDIF};
function RegUnLoadKey(hKey: HKEY; lpSubKey: PKOLChar): Longint; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'RegUnLoadKeyW'{$ELSE}'RegUnLoadKeyA'{$ENDIF};
function RegisterEventSource(lpUNCServerName, lpSourceName: PKOLChar): THandle; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'RegisterEventSourceW'{$ELSE}'RegisterEventSourceA'{$ENDIF};
function ReportEvent(hEventLog: THandle; wType, wCategory: Word; dwEventID: DWORD; lpUserSid: Pointer; wNumStrings: Word; dwDataSize: DWORD; lpStrings, lpRawData: Pointer): BOOL; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'ReportEventW'{$ELSE}'ReportEventA'{$ENDIF};
function SetFileSecurity(lpFileName: PKOLChar; SecurityInformation: SECURITY_INFORMATION; pSecurityDescriptor: PSecurityDescriptor): BOOL; stdcall; external advapi32 name {$IFDEF UNICODE_CTRLS}'SetFileSecurityW'{$ELSE}'SetFileSecurityA'{$ENDIF};
function AddAtom(lpString: PKOLChar): ATOM; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'AddAtomW'{$ELSE}'AddAtomA'{$ENDIF};
function BeginUpdateResource(pFileName: PKOLChar; bDeleteExistingResources: BOOL): THandle; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'BeginUpdateResourceW'{$ELSE}'BeginUpdateResourceA'{$ENDIF};
function BuildCommDCB(lpDef: PKOLChar; var lpDCB: TDCB): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'BuildCommDCBW'{$ELSE}'BuildCommDCBA'{$ENDIF};
function BuildCommDCBAndTimeouts(lpDef: PKOLChar; var lpDCB: TDCB; var lpCommTimeouts: TCommTimeouts): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'BuildCommDCBAndTimeoutsW'{$ELSE}'BuildCommDCBAndTimeoutsA'{$ENDIF};
function CallNamedPipe(lpNamedPipeName: PKOLChar; lpInBuffer: Pointer; nInBufferSize: DWORD; lpOutBuffer: Pointer; nOutBufferSize: DWORD; var lpBytesRead: DWORD; nTimeOut: DWORD): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'CallNamedPipeW'{$ELSE}'CallNamedPipeA'{$ENDIF};
function CommConfigDialog(lpszName: PKOLChar; hWnd: HWND; var lpCC: TCommConfig): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'CommConfigDialogW'{$ELSE}'CommConfigDialogA'{$ENDIF};
function CompareString(Locale: LCID; dwCmpFlags: DWORD; lpString1: PKOLChar; cchCount1: Integer; lpString2: PKOLChar; cchCount2: Integer): Integer; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'CompareStringW'{$ELSE}'CompareStringA'{$ENDIF};
function CopyFile(lpExistingFileName, lpNewFileName: PKOLChar; bFailIfExists: BOOL): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'CopyFileW'{$ELSE}'CopyFileA'{$ENDIF};
function CopyFileEx(lpExistingFileName, lpNewFileName: PKOLChar; lpProgressRoutine: TFNProgressRoutine; lpData: Pointer; pbCancel: PBool; dwCopyFlags: DWORD): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'CopyFileExW'{$ELSE}'CopyFileExA'{$ENDIF};
function CreateDirectory(lpPathName: PKOLChar; lpSecurityAttributes: PSecurityAttributes): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'CreateDirectoryW'{$ELSE}'CreateDirectoryA'{$ENDIF};
function CreateDirectoryEx(lpTemplateDirectory, lpNewDirectory: PKOLChar; lpSecurityAttributes: PSecurityAttributes): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'CreateDirectoryExW'{$ELSE}'CreateDirectoryExA'{$ENDIF};
function CreateEvent(lpEventAttributes: PSecurityAttributes; bManualReset, bInitialState: BOOL; lpName: PKOLChar): THandle; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'CreateEventW'{$ELSE}'CreateEventA'{$ENDIF};
function CreateFile(lpFileName: PKOLChar; dwDesiredAccess, dwShareMode: DWORD; lpSecurityAttributes: PSecurityAttributes; dwCreationDisposition, dwFlagsAndAttributes: DWORD; hTemplateFile: THandle): THandle; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'CreateFileW'{$ELSE}'CreateFileA'{$ENDIF};
function CreateFileW(lpFileName: PWideChar; dwDesiredAccess, dwShareMode: DWORD; lpSecurityAttributes: PSecurityAttributes; dwCreationDisposition, dwFlagsAndAttributes: DWORD; hTemplateFile: THandle): THandle; stdcall; external kernel32 name 'CreateFileW';
function CreateFileMapping(hFile: THandle; lpFileMappingAttributes: PSecurityAttributes; flProtect, dwMaximumSizeHigh, dwMaximumSizeLow: DWORD; lpName: PKOLChar): THandle; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'CreateFileMappingW'{$ELSE}'CreateFileMappingA'{$ENDIF};
function CreateHardLink(lpFileName, lpExistingFileName: PKOLChar; lpSecurityAttributes: PSecurityAttributes): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'CreateHardLinkW'{$ELSE}'CreateHardLinkA'{$ENDIF};
function CreateMailslot(lpName: PKOLChar; nMaxMessageSize: DWORD; lReadTimeout: DWORD; lpSecurityAttributes: PSecurityAttributes): THandle; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'CreateMailslotW'{$ELSE}'CreateMailslotA'{$ENDIF};
function CreateNamedPipe(lpName: PKOLChar; dwOpenMode, dwPipeMode, nMaxInstances, nOutBufferSize, nInBufferSize, nDefaultTimeOut: DWORD; lpSecurityAttributes: PSecurityAttributes): THandle; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'CreateNamedPipeW'{$ELSE}'CreateNamedPipeA'{$ENDIF};
function CreateProcess(lpApplicationName: PKOLChar; lpCommandLine: PKOLChar; lpProcessAttributes, lpThreadAttributes: PSecurityAttributes; bInheritHandles: BOOL; dwCreationFlags: DWORD; lpEnvironment: Pointer; lpCurrentDirectory: PKOLChar; const lpStartupInfo: TStartupInfo; var lpProcessInformation: TProcessInformation): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'CreateProcessW'{$ELSE}'CreateProcessA'{$ENDIF};
function CreateSemaphore(lpSemaphoreAttributes: PSecurityAttributes; lInitialCount, lMaximumCount: Longint; lpName: PKOLChar): THandle; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'CreateSemaphoreW'{$ELSE}'CreateSemaphoreA'{$ENDIF};
function CreateWaitableTimer(lpTimerAttributes: PSecurityAttributes; bManualReset: BOOL; lpTimerName: PKOLChar): THandle; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'CreateWaitableTimerW'{$ELSE}'CreateWaitableTimerA'{$ENDIF};
function DefineDosDevice(dwFlags: DWORD; lpDeviceName, lpTargetPath: PKOLChar): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'DefineDosDeviceW'{$ELSE}'DefineDosDeviceA'{$ENDIF};
function DeleteFile(lpFileName: PKOLChar): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'DeleteFileW'{$ELSE}'DeleteFileA'{$ENDIF};
function EndUpdateResource(hUpdate: THandle; fDiscard: BOOL): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'EndUpdateResourceW'{$ELSE}'EndUpdateResourceA'{$ENDIF};
function EnumCalendarInfo(lpCalInfoEnumProc: TFNCalInfoEnumProc; Locale: LCID; Calendar: CALID; CalType: CALTYPE): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'EnumCalendarInfoW'{$ELSE}'EnumCalendarInfoA'{$ENDIF};
function EnumDateFormats(lpDateFmtEnumProc: TFNDateFmtEnumProc; Locale: LCID; dwFlags: DWORD): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'EnumDateFormatsW'{$ELSE}'EnumDateFormatsA'{$ENDIF};
function EnumResourceLanguages(hModule: HMODULE; lpType, lpName: PKOLChar; lpEnumFunc: ENUMRESLANGPROC; lParam: Longint): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'EnumResourceLanguagesW'{$ELSE}'EnumResourceLanguagesA'{$ENDIF};
function EnumResourceNames(hModule: HMODULE; lpType: PKOLChar; lpEnumFunc: ENUMRESNAMEPROC; lParam: Longint): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'EnumResourceNamesW'{$ELSE}'EnumResourceNamesA'{$ENDIF};
function EnumResourceTypes(hModule: HMODULE; lpEnumFunc: ENUMRESTYPEPROC; lParam: Longint): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'EnumResourceTypesW'{$ELSE}'EnumResourceTypesA'{$ENDIF};
function EnumSystemCodePages(lpCodePageEnumProc: TFNCodepageEnumProc; dwFlags: DWORD): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'EnumSystemCodePagesW'{$ELSE}'EnumSystemCodePagesA'{$ENDIF};
function EnumSystemLocales(lpLocaleEnumProc: TFNLocaleEnumProc; dwFlags: DWORD): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'EnumSystemLocalesW'{$ELSE}'EnumSystemLocalesA'{$ENDIF};
function EnumTimeFormats(lpTimeFmtEnumProc: TFNTimeFmtEnumProc; Locale: LCID; dwFlags: DWORD): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'EnumTimeFormatsW'{$ELSE}'EnumTimeFormatsA'{$ENDIF};
function ExpandEnvironmentStrings(lpSrc: PKOLChar; lpDst: PKOLChar; nSize: DWORD): DWORD; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'ExpandEnvironmentStringsW'{$ELSE}'ExpandEnvironmentStringsA'{$ENDIF};
procedure FatalAppExit(uAction: UINT; lpMessageText: PKOLChar); stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'FatalAppExitW'{$ELSE}'FatalAppExitA'{$ENDIF};
function FillConsoleOutputCharacter(hConsoleOutput: THandle; cCharacter: KOLChar; nLength: DWORD; dwWriteCoord: TCoord; var lpNumberOfCharsWritten: DWORD): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'FillConsoleOutputCharacterW'{$ELSE}'FillConsoleOutputCharacterA'{$ENDIF};
function FindAtom(lpString: PKOLChar): ATOM; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'FindAtomW'{$ELSE}'FindAtomA'{$ENDIF};
function FindFirstChangeNotification(lpPathName: PKOLChar; bWatchSubtree: BOOL; dwNotifyFilter: DWORD): THandle; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'FindFirstChangeNotificationW'{$ELSE}'FindFirstChangeNotificationA'{$ENDIF};
function FindFirstFile(lpFileName: PKOLChar; var lpFindFileData: TWIN32FindData): THandle; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'FindFirstFileW'{$ELSE}'FindFirstFileA'{$ENDIF};
function FindFirstFileEx(lpFileName: PKOLChar; fInfoLevelId: TFindexInfoLevels; lpFindFileData: Pointer; fSearchOp: TFindexSearchOps; lpSearchFilter: Pointer; dwAdditionalFlags: DWORD): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'FindFirstFileExW'{$ELSE}'FindFirstFileExA'{$ENDIF};
function FindNextFile(hFindFile: THandle; var lpFindFileData: TWIN32FindData): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'FindNextFileW'{$ELSE}'FindNextFileA'{$ENDIF};
function FindResource(hModule: HMODULE; lpName, lpType: PKOLChar): HRSRC; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'FindResourceW'{$ELSE}'FindResourceA'{$ENDIF};
function FindResourceEx(hModule: HMODULE; lpType, lpName: PKOLChar; wLanguage: Word): HRSRC; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'FindResourceExW'{$ELSE}'FindResourceExA'{$ENDIF};
function FoldString(dwMapFlags: DWORD; lpSrcStr: PKOLChar; cchSrc: Integer; lpDestStr: PKOLChar; cchDest: Integer): Integer; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'FoldStringW'{$ELSE}'FoldStringA'{$ENDIF};
function FormatMessage(dwFlags: DWORD; lpSource: Pointer; dwMessageId: DWORD; dwLanguageId: DWORD; lpBuffer: PKOLChar; nSize: DWORD; Arguments: Pointer): DWORD; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'FormatMessageW'{$ELSE}'FormatMessageA'{$ENDIF};
function FreeEnvironmentStrings(EnvBlock: PKOLChar): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'FreeEnvironmentStringsW'{$ELSE}'FreeEnvironmentStringsA'{$ENDIF};
function GetAtomName(nAtom: ATOM; lpBuffer: PKOLChar; nSize: Integer): UINT; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GetAtomNameW'{$ELSE}'GetAtomNameA'{$ENDIF};
function GetBinaryType(lpApplicationName: PKOLChar; var lpBinaryType: DWORD): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GetBinaryTypeW'{$ELSE}'GetBinaryTypeA'{$ENDIF};
function GetCommandLine: PKOLChar; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GetCommandLineW'{$ELSE}'GetCommandLineA'{$ENDIF};
function GetCompressedFileSize(lpFileName: PKOLChar; lpFileSizeHigh: PDWORD): DWORD; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GetCompressedFileSizeW'{$ELSE}'GetCompressedFileSizeA'{$ENDIF};
function GetComputerName(lpBuffer: PKOLChar; var nSize: DWORD): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GetComputerNameW'{$ELSE}'GetComputerNameA'{$ENDIF};
function GetConsoleTitle(lpConsoleTitle: PKOLChar; nSize: DWORD): DWORD; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GetConsoleTitleW'{$ELSE}'GetConsoleTitleA'{$ENDIF};
function GetCurrencyFormat(Locale: LCID; dwFlags: DWORD; lpValue: PKOLChar; lpFormat: PCurrencyFmt; lpCurrencyStr: PKOLChar; cchCurrency: Integer): Integer; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GetCurrencyFormatW'{$ELSE}'GetCurrencyFormatA'{$ENDIF};
function GetCurrentDirectory(nBufferLength: DWORD; lpBuffer: PKOLChar): DWORD; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GetCurrentDirectoryW'{$ELSE}'GetCurrentDirectoryA'{$ENDIF};
function GetDateFormat(Locale: LCID; dwFlags: DWORD; lpDate: PSystemTime; lpFormat: PKOLChar; lpDateStr: PKOLChar; cchDate: Integer): Integer; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GetDateFormatW'{$ELSE}'GetDateFormatA'{$ENDIF};
function GetDefaultCommConfig(lpszName: PKOLChar; var lpCC: TCommConfig; var lpdwSize: DWORD): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GetDefaultCommConfigW'{$ELSE}'GetDefaultCommConfigA'{$ENDIF};
function GetDiskFreeSpace(lpRootPathName: PKOLChar; var lpSectorsPerCluster, lpBytesPerSector, lpNumberOfFreeClusters, lpTotalNumberOfClusters: DWORD): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GetDiskFreeSpaceW'{$ELSE}'GetDiskFreeSpaceA'{$ENDIF};
function GetDiskFreeSpaceEx(lpDirectoryName: PKOLChar; var lpFreeBytesAvailableToCaller, lpTotalNumberOfBytes; lpTotalNumberOfFreeBytes: PLargeInteger): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GetDiskFreeSpaceExW'{$ELSE}'GetDiskFreeSpaceExA'{$ENDIF};
function GetDriveType(lpRootPathName: PKOLChar): UINT; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GetDriveTypeW'{$ELSE}'GetDriveTypeA'{$ENDIF};
function GetEnvironmentVariable(lpName: PKOLChar; lpBuffer: PKOLChar; nSize: DWORD): DWORD; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GetEnvironmentVariableW'{$ELSE}'GetEnvironmentVariableA'{$ENDIF};
function GetFileAttributes(lpFileName: PKOLChar): DWORD; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GetFileAttributesW'{$ELSE}'GetFileAttributesA'{$ENDIF};
function GetFileAttributesEx(lpFileName: PKOLChar; fInfoLevelId: TGetFileExInfoLevels; lpFileInformation: Pointer): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GetFileAttributesExW'{$ELSE}'GetFileAttributesExA'{$ENDIF};
function GetFullPathName(lpFileName: PKOLChar; nBufferLength: DWORD; lpBuffer: PKOLChar; lpFilePart: PKOLChar): DWORD; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GetFullPathNameW'{$ELSE}'GetFullPathNameA'{$ENDIF};
function GetLocaleInfo(Locale: LCID; LCType: LCTYPE; lpLCData: PKOLChar; cchData: Integer): Integer; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GetLocaleInfoW'{$ELSE}'GetLocaleInfoA'{$ENDIF};
function GetLogicalDriveStrings(nBufferLength: DWORD; lpBuffer: PKOLChar): DWORD; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GetLogicalDriveStringsW'{$ELSE}'GetLogicalDriveStringsA'{$ENDIF};
function GetModuleFileName(hModule: HINST; lpFilename: PKOLChar; nSize: DWORD): DWORD; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GetModuleFileNameW'{$ELSE}'GetModuleFileNameA'{$ENDIF};
function GetModuleHandle(lpModuleName: PKOLChar): HMODULE; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GetModuleHandleW'{$ELSE}'GetModuleHandleA'{$ENDIF};
function GetNamedPipeHandleState(hNamedPipe: THandle; lpState, lpCurInstances, lpMaxCollectionCount, lpCollectDataTimeout: PDWORD; lpUserName: PKOLChar; nMaxUserNameSize: DWORD): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GetNamedPipeHandleStateW'{$ELSE}'GetNamedPipeHandleStateA'{$ENDIF};
function GetNumberFormat(Locale: LCID; dwFlags: DWORD; lpValue: PKOLChar; lpFormat: PNumberFmt; lpNumberStr: PKOLChar; cchNumber: Integer): Integer; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GetNumberFormatW'{$ELSE}'GetNumberFormatA'{$ENDIF};
function GetPrivateProfileInt(lpAppName, lpKeyName: PKOLChar; nDefault: Integer; lpFileName: PKOLChar): UINT; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GetPrivateProfileIntW'{$ELSE}'GetPrivateProfileIntA'{$ENDIF};
function GetPrivateProfileSection(lpAppName: PKOLChar; lpReturnedString: PKOLChar; nSize: DWORD; lpFileName: PKOLChar): DWORD; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GetPrivateProfileSectionW'{$ELSE}'GetPrivateProfileSectionA'{$ENDIF};
function GetPrivateProfileSectionNames(lpszReturnBuffer: PKOLChar; nSize: DWORD; lpFileName: PKOLChar): DWORD; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GetPrivateProfileSectionNamesW'{$ELSE}'GetPrivateProfileSectionNamesA'{$ENDIF};
function GetPrivateProfileString(lpAppName, lpKeyName, lpDefault: PKOLChar; lpReturnedString: PKOLChar; nSize: DWORD; lpFileName: PKOLChar): DWORD; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GetPrivateProfileStringW'{$ELSE}'GetPrivateProfileStringA'{$ENDIF};
function GetProfileInt(lpAppName, lpKeyName: PKOLChar; nDefault: Integer): UINT; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GetProfileIntW'{$ELSE}'GetProfileIntA'{$ENDIF};
function GetProfileSection(lpAppName: PKOLChar; lpReturnedString: PKOLChar; nSize: DWORD): DWORD; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GetProfileSectionW'{$ELSE}'GetProfileSectionA'{$ENDIF};
function GetProfileString(lpAppName, lpKeyName, lpDefault: PKOLChar; lpReturnedString: PKOLChar; nSize: DWORD): DWORD; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GetProfileStringW'{$ELSE}'GetProfileStringA'{$ENDIF};
function GetShortPathName(lpszLongPath: PKOLChar; lpszShortPath: PKOLChar; cchBuffer: DWORD): DWORD; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GetShortPathNameW'{$ELSE}'GetShortPathNameA'{$ENDIF};
procedure GetStartupInfo(var lpStartupInfo: TStartupInfo); stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GetStartupInfoW'{$ELSE}'GetStartupInfoA'{$ENDIF};
function GetStringTypeEx(Locale: LCID; dwInfoType: DWORD; lpSrcStr: PKOLChar; cchSrc: Integer; var lpCharType): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GetStringTypeExW'{$ELSE}'GetStringTypeExA'{$ENDIF};
function GetSystemDirectory(lpBuffer: PKOLChar; uSize: UINT): UINT; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GetSystemDirectoryW'{$ELSE}'GetSystemDirectoryA'{$ENDIF};
function GetTempFileName(lpPathName, lpPrefixString: PKOLChar; uUnique: UINT; lpTempFileName: PKOLChar): UINT; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GetTempFileNameW'{$ELSE}'GetTempFileNameA'{$ENDIF};
function GetTempPath(nBufferLength: DWORD; lpBuffer: PKOLChar): DWORD; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GetTempPathW'{$ELSE}'GetTempPathA'{$ENDIF};
function GetTimeFormat(Locale: LCID; dwFlags: DWORD; lpTime: PSystemTime; lpFormat: PKOLChar; lpTimeStr: PKOLChar; cchTime: Integer): Integer; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GetTimeFormatW'{$ELSE}'GetTimeFormatA'{$ENDIF};
function GetVersionEx(var lpVersionInformation: TOSVersionInfo): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GetVersionExW'{$ELSE}'GetVersionExA'{$ENDIF};
function GetVolumeInformation(lpRootPathName: PKOLChar; lpVolumeNameBuffer: PKOLChar; nVolumeNameSize: DWORD; lpVolumeSerialNumber: PDWORD; var lpMaximumComponentLength, lpFileSystemFlags: DWORD; lpFileSystemNameBuffer: PKOLChar; nFileSystemNameSize: DWORD): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GetVolumeInformationW'{$ELSE}'GetVolumeInformationA'{$ENDIF};
function GetWindowsDirectory(lpBuffer: PKOLChar; uSize: UINT): UINT; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GetWindowsDirectoryW'{$ELSE}'GetWindowsDirectoryA'{$ENDIF};
function GlobalAddAtom(lpString: PKOLChar): ATOM; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GlobalAddAtomW'{$ELSE}'GlobalAddAtomA'{$ENDIF};
function GlobalFindAtom(lpString: PKOLChar): ATOM; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GlobalFindAtomW'{$ELSE}'GlobalFindAtomA'{$ENDIF};
function GlobalGetAtomName(nAtom: ATOM; lpBuffer: PKOLChar; nSize: Integer): UINT; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GlobalGetAtomNameW'{$ELSE}'GlobalGetAtomNameA'{$ENDIF};
function IsBadStringPtr(lpsz: PKOLChar; ucchMax: UINT): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'IsBadStringPtrW'{$ELSE}'IsBadStringPtrA'{$ENDIF};
function LCMapString(Locale: LCID; dwMapFlags: DWORD; lpSrcStr: PKOLChar; cchSrc: Integer; lpDestStr: PKOLChar; cchDest: Integer): Integer; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'LCMapStringW'{$ELSE}'LCMapStringA'{$ENDIF};
function LoadLibrary(lpLibFileName: PKOLChar): HMODULE; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'LoadLibraryW'{$ELSE}'LoadLibraryA'{$ENDIF};
function LoadLibraryEx(lpLibFileName: PKOLChar; hFile: THandle; dwFlags: DWORD): HMODULE; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'LoadLibraryExW'{$ELSE}'LoadLibraryExA'{$ENDIF};
function MoveFile(lpExistingFileName, lpNewFileName: PKOLChar): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'MoveFileW'{$ELSE}'MoveFileA'{$ENDIF};
function MoveFileEx(lpExistingFileName, lpNewFileName: PKOLChar; dwFlags: DWORD): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'MoveFileExW'{$ELSE}'MoveFileExA'{$ENDIF};
function MoveFileWithProgress(lpExistingFileName, lpNewFileName: PKOLChar; lpProgressRoutine: TFNProgressRoutine; lpData: Pointer; dwFlags: DWORD): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'MoveFileWithProgressW'{$ELSE}'MoveFileWithProgressA'{$ENDIF};
function OpenEvent(dwDesiredAccess: DWORD; bInheritHandle: BOOL; lpName: PKOLChar): THandle; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'OpenEventW'{$ELSE}'OpenEventA'{$ENDIF};
function OpenFileMapping(dwDesiredAccess: DWORD; bInheritHandle: BOOL; lpName: PKOLChar): THandle; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'OpenFileMappingW'{$ELSE}'OpenFileMappingA'{$ENDIF};
function OpenMutex(dwDesiredAccess: DWORD; bInheritHandle: BOOL; lpName: PKOLChar): THandle; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'OpenMutexW'{$ELSE}'OpenMutexA'{$ENDIF};
function OpenSemaphore(dwDesiredAccess: DWORD; bInheritHandle: BOOL; lpName: PKOLChar): THandle; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'OpenSemaphoreW'{$ELSE}'OpenSemaphoreA'{$ENDIF};
function OpenWaitableTimer(dwDesiredAccess: DWORD; bInheritHandle: BOOL; lpTimerName: PKOLChar): THandle; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'OpenWaitableTimerW'{$ELSE}'OpenWaitableTimerA'{$ENDIF};
procedure OutputDebugString(lpOutputString: PKOLChar); stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'OutputDebugStringW'{$ELSE}'OutputDebugStringA'{$ENDIF};
function PeekConsoleInput(hConsoleInput: THandle; var lpBuffer: TInputRecord; nLength: DWORD; var lpNumberOfEventsRead: DWORD): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'PeekConsoleInputW'{$ELSE}'PeekConsoleInputA'{$ENDIF};
function QueryDosDevice(lpDeviceName: PKOLChar; lpTargetPath: PKOLChar; ucchMax: DWORD): DWORD; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'QueryDosDeviceW'{$ELSE}'QueryDosDeviceA'{$ENDIF};
function QueryRecoveryAgents(p1: PKOLChar; var p2: Pointer; var p3: TRecoveryAgentInformation): DWORD; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'QueryRecoveryAgentsW'{$ELSE}'QueryRecoveryAgentsA'{$ENDIF};
function ReadConsole(hConsoleInput: THandle; lpBuffer: Pointer; nNumberOfCharsToRead: DWORD; var lpNumberOfCharsRead: DWORD; lpReserved: Pointer): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'ReadConsoleW'{$ELSE}'ReadConsoleA'{$ENDIF};
function ReadConsoleInput(hConsoleInput: THandle; var lpBuffer: TInputRecord; nLength: DWORD; var lpNumberOfEventsRead: DWORD): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'ReadConsoleInputW'{$ELSE}'ReadConsoleInputA'{$ENDIF};
function ReadConsoleOutput(hConsoleOutput: THandle; lpBuffer: Pointer; dwBufferSize, dwBufferCoord: TCoord; var lpReadRegion: TSmallRect): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'ReadConsoleOutputW'{$ELSE}'ReadConsoleOutputA'{$ENDIF};
function ReadConsoleOutputCharacter(hConsoleOutput: THandle; lpCharacter: PKOLChar; nLength: DWORD; dwReadCoord: TCoord; var lpNumberOfCharsRead: DWORD): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'ReadConsoleOutputCharacterW'{$ELSE}'ReadConsoleOutputCharacterA'{$ENDIF};
function RemoveDirectory(lpPathName: PKOLChar): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'RemoveDirectoryW'{$ELSE}'RemoveDirectoryA'{$ENDIF};
function ScrollConsoleScreenBuffer(hConsoleOutput: THandle; const lpScrollRectangle: TSmallRect; lpClipRectangle: PSmallRect; dwDestinationOrigin: TCoord; var lpFill: TCharInfo): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'ScrollConsoleScreenBufferW'{$ELSE}'ScrollConsoleScreenBufferA'{$ENDIF};
function SearchPath(lpPath, lpFileName, lpExtension: PKOLChar; nBufferLength: DWORD; lpBuffer: PKOLChar; var lpFilePart: PKOLChar): DWORD; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'SearchPathW'{$ELSE}'SearchPathA'{$ENDIF};
function SetComputerName(lpComputerName: PKOLChar): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'SetComputerNameW'{$ELSE}'SetComputerNameA'{$ENDIF};
function SetConsoleTitle(lpConsoleTitle: PKOLChar): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'SetConsoleTitleW'{$ELSE}'SetConsoleTitleA'{$ENDIF};
function SetCurrentDirectory(lpPathName: PKOLChar): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'SetCurrentDirectoryW'{$ELSE}'SetCurrentDirectoryA'{$ENDIF};
function SetDefaultCommConfig(lpszName: PKOLChar; lpCC: PCommConfig; dwSize: DWORD): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'SetDefaultCommConfigW'{$ELSE}'SetDefaultCommConfigA'{$ENDIF};
function SetEnvironmentVariable(lpName, lpValue: PKOLChar): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'SetEnvironmentVariableW'{$ELSE}'SetEnvironmentVariableA'{$ENDIF};
function SetFileAttributes(lpFileName: PKOLChar; dwFileAttributes: DWORD): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'SetFileAttributesW'{$ELSE}'SetFileAttributesA'{$ENDIF};
function SetLocaleInfo(Locale: LCID; LCType: LCTYPE; lpLCData: PKOLChar): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'SetLocaleInfoW'{$ELSE}'SetLocaleInfoA'{$ENDIF};
function SetVolumeLabel(lpRootPathName: PKOLChar; lpVolumeName: PKOLChar): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'SetVolumeLabelW'{$ELSE}'SetVolumeLabelA'{$ENDIF};
function UpdateResource(hUpdate: THandle; lpType, lpName: PKOLChar; wLanguage: Word; lpData: Pointer; cbData: DWORD): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'UpdateResourceW'{$ELSE}'UpdateResourceA'{$ENDIF};
function VerLanguageName(wLang: DWORD; szLang: PKOLChar; nSize: DWORD): DWORD; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'VerLanguageNameW'{$ELSE}'VerLanguageNameA'{$ENDIF};
function WaitNamedPipe(lpNamedPipeName: PKOLChar; nTimeOut: DWORD): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'WaitNamedPipeW'{$ELSE}'WaitNamedPipeA'{$ENDIF};
function WriteConsole(hConsoleOutput: THandle; const lpBuffer: Pointer; nNumberOfCharsToWrite: DWORD; var lpNumberOfCharsWritten: DWORD; lpReserved: Pointer): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'WriteConsoleW'{$ELSE}'WriteConsoleA'{$ENDIF};
function WriteConsoleInput(hConsoleInput: THandle; const lpBuffer: TInputRecord; nLength: DWORD; var lpNumberOfEventsWritten: DWORD): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'WriteConsoleInputW'{$ELSE}'WriteConsoleInputA'{$ENDIF};
function WriteConsoleOutput(hConsoleOutput: THandle; lpBuffer: Pointer; dwBufferSize, dwBufferCoord: TCoord; var lpWriteRegion: TSmallRect): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'WriteConsoleOutputW'{$ELSE}'WriteConsoleOutputA'{$ENDIF};
function WriteConsoleOutputCharacter(hConsoleOutput: THandle;lpCharacter: PKOLChar; nLength: DWORD; dwWriteCoord: TCoord; var lpNumberOfCharsWritten: DWORD): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'WriteConsoleOutputCharacterW'{$ELSE}'WriteConsoleOutputCharacterA'{$ENDIF};
function WritePrivateProfileSection(lpAppName, lpString, lpFileName: PKOLChar): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'WritePrivateProfileSectionW'{$ELSE}'WritePrivateProfileSectionA'{$ENDIF};
function WritePrivateProfileString(lpAppName, lpKeyName, lpString, lpFileName: PKOLChar): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'WritePrivateProfileStringW'{$ELSE}'WritePrivateProfileStringA'{$ENDIF};
function WriteProfileSection(lpAppName, lpString: PKOLChar): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'WriteProfileSectionW'{$ELSE}'WriteProfileSectionA'{$ENDIF};
function WriteProfileString(lpAppName, lpKeyName, lpString: PKOLChar): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'WriteProfileStringW'{$ELSE}'WriteProfileStringA'{$ENDIF};
function lstrcat(lpString1, lpString2: PKOLChar): PKOLChar; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'lstrcatW'{$ELSE}'lstrcatA'{$ENDIF};
function lstrcmp(lpString1, lpString2: PKOLChar): Integer; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'lstrcmpW'{$ELSE}'lstrcmpA'{$ENDIF};
function lstrcmpi(lpString1, lpString2: PKOLChar): Integer; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'lstrcmpiW'{$ELSE}'lstrcmpiA'{$ENDIF};
function lstrcpy(lpString1, lpString2: PKOLChar): PKOLChar; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'lstrcpyW'{$ELSE}'lstrcpyA'{$ENDIF};
function lstrcpyn(lpString1, lpString2: PKOLChar; iMaxLength: Integer): PKOLChar; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'lstrcpynW'{$ELSE}'lstrcpynA'{$ENDIF};
function lstrlen(lpString: PKOLChar): Integer; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'lstrlenW'{$ELSE}'lstrlenA'{$ENDIF};
function MultinetGetConnectionPerformance(lpNetResource: PNetResource; lpNetConnectInfoStruc: PNetConnectInfoStruct): DWORD; stdcall; external mpr name {$IFDEF UNICODE_CTRLS}'MultinetGetConnectionPerformanceW'{$ELSE}'MultinetGetConnectionPerformanceA'{$ENDIF};
function WNetAddConnection2(var lpNetResource: TNetResource; lpPassword, lpUserName: PKOLChar; dwFlags: DWORD): DWORD; stdcall; external mpr name {$IFDEF UNICODE_CTRLS}'WNetAddConnection2W'{$ELSE}'WNetAddConnection2A'{$ENDIF};
function WNetAddConnection3(hwndOwner: HWND; var lpNetResource: TNetResource; lpPassword, lpUserName: PKOLChar; dwFlags: DWORD): DWORD; stdcall; external mpr name {$IFDEF UNICODE_CTRLS}'WNetAddConnection3W'{$ELSE}'WNetAddConnection3A'{$ENDIF};
function WNetAddConnection(lpRemoteName, lpPassword, lpLocalName: PKOLChar): DWORD; stdcall; external mpr name {$IFDEF UNICODE_CTRLS}'WNetAddConnectionW'{$ELSE}'WNetAddConnectionA'{$ENDIF};
function WNetCancelConnection2(lpName: PKOLChar; dwFlags: DWORD; fForce: BOOL): DWORD; stdcall; external mpr name {$IFDEF UNICODE_CTRLS}'WNetCancelConnection2W'{$ELSE}'WNetCancelConnection2A'{$ENDIF};
function WNetCancelConnection(lpName: PKOLChar; fForce: BOOL): DWORD; stdcall; external mpr name {$IFDEF UNICODE_CTRLS}'WNetCancelConnectionW'{$ELSE}'WNetCancelConnectionA'{$ENDIF};
function WNetConnectionDialog1(var lpConnDlgStruct: TConnectDlgStruct): DWORD; stdcall; external mpr name {$IFDEF UNICODE_CTRLS}'WNetConnectionDialog1W'{$ELSE}'WNetConnectionDialog1A'{$ENDIF};
function WNetDisconnectDialog1(var lpConnDlgStruct: TDiscDlgStruct): DWORD; stdcall; external mpr name {$IFDEF UNICODE_CTRLS}'WNetDisconnectDialog1W'{$ELSE}'WNetDisconnectDialog1A'{$ENDIF};
function WNetEnumResource(hEnum: THandle; var lpcCount: DWORD; lpBuffer: Pointer; var lpBufferSize: DWORD): DWORD; stdcall; external mpr name {$IFDEF UNICODE_CTRLS}'WNetEnumResourceW'{$ELSE}'WNetEnumResourceA'{$ENDIF};
function WNetGetConnection(lpLocalName: PKOLChar; lpRemoteName: PKOLChar; var lpnLength: DWORD): DWORD; stdcall; external mpr name {$IFDEF UNICODE_CTRLS}'WNetGetConnectionW'{$ELSE}'WNetGetConnectionA'{$ENDIF};
function WNetGetLastError(var lpError: DWORD; lpErrorBuf: PKOLChar; nErrorBufSize: DWORD; lpNameBuf: PKOLChar; nNameBufSize: DWORD): DWORD; stdcall; external mpr name {$IFDEF UNICODE_CTRLS}'WNetGetLastErrorW'{$ELSE}'WNetGetLastErrorA'{$ENDIF};
function WNetGetNetworkInformation(lpProvider: PKOLChar; var lpNetInfoStruct: TNetInfoStruct): DWORD; stdcall; external mpr name {$IFDEF UNICODE_CTRLS}'WNetGetNetworkInformationW'{$ELSE}'WNetGetNetworkInformationA'{$ENDIF};
function WNetGetProviderName(dwNetType: DWORD; lpProviderName: PKOLChar; var lpBufferSize: DWORD): DWORD; stdcall; external mpr name {$IFDEF UNICODE_CTRLS}'WNetGetProviderNameW'{$ELSE}'WNetGetProviderNameA'{$ENDIF};
function WNetGetResourceParent(lpNetResource: PNetResource; lpBuffer: Pointer; var cbBuffer: DWORD): DWORD; stdcall; external mpr name {$IFDEF UNICODE_CTRLS}'WNetGetResourceParentW'{$ELSE}'WNetGetResourceParentA'{$ENDIF};
function WNetGetUniversalName(lpLocalPath: PKOLChar; dwInfoLevel: DWORD; lpBuffer: Pointer; var lpBufferSize: DWORD): DWORD; stdcall; external mpr name {$IFDEF UNICODE_CTRLS}'WNetGetUniversalNameW'{$ELSE}'WNetGetUniversalNameA'{$ENDIF};
function WNetGetUser(lpName: PKOLChar; lpUserName: PKOLChar; var lpnLength: DWORD): DWORD; stdcall; external mpr name {$IFDEF UNICODE_CTRLS}'WNetGetUserW'{$ELSE}'WNetGetUserA'{$ENDIF};
function WNetOpenEnum(dwScope, dwType, dwUsage: DWORD; lpNetResource: PNetResource; var lphEnum: THandle): DWORD; stdcall; external mpr name {$IFDEF UNICODE_CTRLS}'WNetOpenEnumW'{$ELSE}'WNetOpenEnumA'{$ENDIF};
function WNetSetConnection(lpName: PKOLChar; dwProperties: DWORD; pvValues: Pointer): DWORD; stdcall; external mpr name {$IFDEF UNICODE_CTRLS}'WNetSetConnectionW'{$ELSE}'WNetSetConnectionA'{$ENDIF};
function WNetUseConnection(hwndOwner: HWND; var lpNetResource: TNetResource; lpUserID: PKOLChar; lpPassword: PKOLChar; dwFlags: DWORD; lpAccessName: PKOLChar; var lpBufferSize: DWORD; var lpResult: DWORD): DWORD; stdcall; external mpr name {$IFDEF UNICODE_CTRLS}'WNetUseConnectionW'{$ELSE}'WNetUseConnectionA'{$ENDIF};
function GetFileVersionInfo(lptstrFilename: PKOLChar; dwHandle, dwLen: DWORD; lpData: Pointer): BOOL; stdcall; external version name {$IFDEF UNICODE_CTRLS}'GetFileVersionInfoW'{$ELSE}'GetFileVersionInfoA'{$ENDIF};
function GetFileVersionInfoSize(lptstrFilename: PKOLChar; var lpdwHandle: DWORD): DWORD; stdcall; external version name {$IFDEF UNICODE_CTRLS}'GetFileVersionInfoSizeW'{$ELSE}'GetFileVersionInfoSizeA'{$ENDIF};
function VerFindFile(uFlags: DWORD; szFileName, szWinDir, szAppDir, szCurDir: PKOLChar; var lpuCurDirLen: UINT; szDestDir: PKOLChar; var lpuDestDirLen: UINT): DWORD; stdcall; external version name {$IFDEF UNICODE_CTRLS}'VerFindFileW'{$ELSE}'VerFindFileA'{$ENDIF};
function VerInstallFile(uFlags: DWORD; szSrcFileName, szDestFileName, szSrcDir, szDestDir, szCurDir, szTmpFile: PKOLChar; var lpuTmpFileLen: UINT): DWORD; stdcall; external version name {$IFDEF UNICODE_CTRLS}'VerInstallFileW'{$ELSE}'VerInstallFileA'{$ENDIF};
function VerQueryValue(pBlock: Pointer; lpSubBlock: PKOLChar; var lplpBuffer: Pointer; var puLen: UINT): BOOL; stdcall; external version name {$IFDEF UNICODE_CTRLS}'VerQueryValueW'{$ELSE}'VerQueryValueA'{$ENDIF};
function GetPrivateProfileStruct(lpszSection, lpszKey: PKOLChar; lpStruct: Pointer; uSizeStruct: UINT; szFile: PKOLChar): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'GetPrivateProfileStructW'{$ELSE}'GetPrivateProfileStructA'{$ENDIF};
function WritePrivateProfileStruct(lpszSection, lpszKey: PKOLChar; lpStruct: Pointer; uSizeStruct: UINT; szFile: PKOLChar): BOOL; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'WritePrivateProfileStructW'{$ELSE}'WritePrivateProfileStructA'{$ENDIF};
function AddFontResource(FileName: PKOLChar): Integer; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'AddFontResourceW'{$ELSE}'AddFontResourceA'{$ENDIF};
function AddFontResourceEx(p1: PKOLChar; p2: DWORD; p3: PDesignVector): Integer; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'AddFontResourceExW'{$ELSE}'AddFontResourceExA'{$ENDIF};
function CopyEnhMetaFile(p1: HENHMETAFILE; p2: PKOLChar): HENHMETAFILE; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'CopyEnhMetaFileW'{$ELSE}'CopyEnhMetaFileA'{$ENDIF};
function CopyMetaFile(p1: HMETAFILE; p2: PKOLChar): HMETAFILE; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'CopyMetaFileW'{$ELSE}'CopyMetaFileA'{$ENDIF};
function CreateColorSpace(var ColorSpace: TLogColorSpace): HCOLORSPACE; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'CreateColorSpaceW'{$ELSE}'CreateColorSpaceA'{$ENDIF};
function CreateDC(lpszDriver, lpszDevice, lpszOutput: PKOLChar; lpdvmInit: PDeviceMode): HDC; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'CreateDCW'{$ELSE}'CreateDCA'{$ENDIF};
function CreateEnhMetaFile(DC: HDC; FileName: PKOLChar; Rect: PRect; Desc: PKOLChar): HDC; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'CreateEnhMetaFileW'{$ELSE}'CreateEnhMetaFileA'{$ENDIF};
function CreateFont(nHeight, nWidth, nEscapement, nOrientaion, fnWeight: Integer; fdwItalic, fdwUnderline, fdwStrikeOut, fdwCharSet, fdwOutputPrecision, fdwClipPrecision, fdwQuality, fdwPitchAndFamily: DWORD; lpszFace: PKOLChar): HFONT; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'CreateFontW'{$ELSE}'CreateFontA'{$ENDIF};
function CreateFontIndirect(const p1: TLogFont): HFONT; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'CreateFontIndirectW'{$ELSE}'CreateFontIndirectA'{$ENDIF};
function CreateFontIndirectEx(const p1: PEnumLogFontExDV): HFONT; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'CreateFontIndirectExW'{$ELSE}'CreateFontIndirectExA'{$ENDIF};
function CreateIC(lpszDriver, lpszDevice, lpszOutput: PKOLChar; lpdvmInit: PDeviceMode): HDC; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'CreateICW'{$ELSE}'CreateICA'{$ENDIF};
function CreateMetaFile(p1: PKOLChar): HDC; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'CreateMetaFileW'{$ELSE}'CreateMetaFileA'{$ENDIF};
function CreateScalableFontResource(p1: DWORD; p2, p3, p4: PKOLChar): BOOL; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'CreateScalableFontResourceW'{$ELSE}'CreateScalableFontResourceA'{$ENDIF};
function DeviceCapabilities(pDriverName, pDeviceName, pPort: PKOLChar; iIndex: Integer; pOutput: PKOLChar; DevMode: PDeviceMode): Integer; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'DeviceCapabilitiesW'{$ELSE}'DeviceCapabilitiesA'{$ENDIF};
function EnumFontFamilies(DC: HDC; p2: PKOLChar; p3: TFNFontEnumProc; p4: LPARAM): BOOL; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'EnumFontFamiliesW'{$ELSE}'EnumFontFamiliesA'{$ENDIF};
function EnumFontFamiliesEx(DC: HDC; var p2: TLogFont; p3: TFNFontEnumProc; p4: LPARAM; p5: DWORD): BOOL; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'EnumFontFamiliesExW'{$ELSE}'EnumFontFamiliesExA'{$ENDIF};
function EnumFonts(DC: HDC; lpszFace: PKOLChar; fntenmprc: TFNFontEnumProc; lpszData: PKOLChar): Integer; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'EnumFontsW'{$ELSE}'EnumFontsA'{$ENDIF};
function EnumICMProfiles(DC: HDC; ICMProc: TFNICMEnumProc; p3: LPARAM): Integer; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'EnumICMProfilesW'{$ELSE}'EnumICMProfilesA'{$ENDIF};
function ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect; Str: PKOLChar; Count: Longint; Dx: PInteger): BOOL; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'ExtTextOutW'{$ELSE}'ExtTextOutA'{$ENDIF};
function GetCharABCWidths(DC: HDC; FirstChar, LastChar: UINT; const ABCStructs): BOOL; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'GetCharABCWidthsW'{$ELSE}'GetCharABCWidthsA'{$ENDIF};
function GetCharABCWidthsFloat(DC: HDC; FirstChar, LastChar: UINT; const ABCFloatSturcts): BOOL; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'GetCharABCWidthsFloatW'{$ELSE}'GetCharABCWidthsFloatA'{$ENDIF};
function GetCharWidth32(DC: HDC; FirstChar, LastChar: UINT; const Widths): BOOL; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'GetCharWidth32W'{$ELSE}'GetCharWidth32A'{$ENDIF};
function GetCharWidth(DC: HDC; FirstChar, LastChar: UINT; const Widths): BOOL; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'GetCharWidthW'{$ELSE}'GetCharWidthA'{$ENDIF};
function GetCharWidthFloat(DC: HDC; FirstChar, LastChar: UINT; const Widths): BOOL; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'GetCharWidthFloatW'{$ELSE}'GetCharWidthFloatA'{$ENDIF};
function GetCharacterPlacement(DC: HDC; p2: PKOLChar; p3, p4: BOOL; var p5: TGCPResults; p6: DWORD): DWORD; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'GetCharacterPlacementW'{$ELSE}'GetCharacterPlacementA'{$ENDIF};
function GetEnhMetaFile(p1: PKOLChar): HENHMETAFILE; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'GetEnhMetaFileW'{$ELSE}'GetEnhMetaFileA'{$ENDIF};
function GetEnhMetaFileDescription(p1: HENHMETAFILE; p2: UINT; p3: PKOLChar): UINT; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'GetEnhMetaFileDescriptionW'{$ELSE}'GetEnhMetaFileDescriptionA'{$ENDIF};
function GetGlyphIndices(DC: HDC; p2: PKOLChar; p3: Integer; p4: PWORD; p5: DWORD): DWORD; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'GetGlyphIndicesW'{$ELSE}'GetGlyphIndicesA'{$ENDIF};
function GetGlyphOutline(DC: HDC; uChar, uFormat: UINT; const lpgm: TGlyphMetrics; cbBuffer: DWORD; lpvBuffer: Pointer; const lpmat2: TMat2): DWORD; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'GetGlyphOutlineW'{$ELSE}'GetGlyphOutlineA'{$ENDIF};
function GetICMProfile(DC: HDC; var Size: DWORD; Name: PKOLChar): BOOL; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'GetICMProfileW'{$ELSE}'GetICMProfileA'{$ENDIF};
function GetLogColorSpace(p1: HCOLORSPACE; var ColorSpace: TLogColorSpace; Size: DWORD): BOOL; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'GetLogColorSpaceW'{$ELSE}'GetLogColorSpaceA'{$ENDIF};
function GetMetaFile(p1: PKOLChar): HMETAFILE; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'GetMetaFileW'{$ELSE}'GetMetaFileA'{$ENDIF};
function GetObject(p1: HGDIOBJ; p2: Integer; p3: Pointer): Integer; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'GetObjectW'{$ELSE}'GetObjectA'{$ENDIF};
function GetOutlineTextMetrics(DC: HDC; p2: UINT; OTMetricStructs: Pointer): UINT; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'GetOutlineTextMetricsW'{$ELSE}'GetOutlineTextMetricsA'{$ENDIF};
function GetTextExtentExPoint(DC: HDC; p2: PKOLChar; p3, p4: Integer; p5, p6: PInteger; var p7: TSize): BOOL; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'GetTextExtentExPointW'{$ELSE}'GetTextExtentExPointA'{$ENDIF};
function GetTextExtentPoint32(DC: HDC; Str: PKOLChar; Count: Integer; var Size: TSize): BOOL; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'GetTextExtentPoint32W'{$ELSE}'GetTextExtentPoint32A'{$ENDIF};
function GetTextExtentPoint32W(DC: HDC; Str: PWideChar; Count: Integer; var Size: TSize): BOOL; stdcall; external gdi32 name 'GetTextExtentPoint32W';
function GetTextExtentPoint(DC: HDC; Str: PKOLChar; Count: Integer; var Size: TSize): BOOL; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'GetTextExtentPointW'{$ELSE}'GetTextExtentPointA'{$ENDIF};
function GetTextFace(DC: HDC; Count: Integer; Buffer: PKOLChar): Integer; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'GetTextFaceW'{$ELSE}'GetTextFaceA'{$ENDIF};
function GetTextMetrics(DC: HDC; var TM: TTextMetric): BOOL; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'GetTextMetricsW'{$ELSE}'GetTextMetricsA'{$ENDIF};
function PolyTextOut(DC: HDC; const PolyTextArray; Strings: Integer): BOOL; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'PolyTextOutW'{$ELSE}'PolyTextOutA'{$ENDIF};
function RemoveFontResource(FileName: PKOLChar): BOOL; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'RemoveFontResourceW'{$ELSE}'RemoveFontResourceA'{$ENDIF};
function RemoveFontResourceEx(p1: PKOLChar; p2: DWORD; p3: PDesignVector): BOOL; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'RemoveFontResourceExW'{$ELSE}'RemoveFontResourceExA'{$ENDIF};
function ResetDC(DC: HDC; const InitData: TDeviceMode): HDC; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'ResetDCW'{$ELSE}'ResetDCA'{$ENDIF};
function SetICMProfile(DC: HDC; Name: PKOLChar): BOOL; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'SetICMProfileW'{$ELSE}'SetICMProfileA'{$ENDIF};
function StartDoc(DC: HDC; const p2: TDocInfo): Integer; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'StartDocW'{$ELSE}'StartDocA'{$ENDIF};
function TextOut(DC: HDC; X, Y: Integer; Str: PKOLChar; Count: Integer): BOOL; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'TextOutW'{$ELSE}'TextOutA'{$ENDIF};
function UpdateICMRegKey(p1: DWORD; p2, p3: PKOLChar; p4: UINT): BOOL; stdcall; external gdi32 name {$IFDEF UNICODE_CTRLS}'UpdateICMRegKeyW'{$ELSE}'UpdateICMRegKeyA'{$ENDIF};
function wglUseFontBitmaps(DC: HDC; p2, p3, p4: DWORD): BOOL; stdcall; external opengl32 name {$IFDEF UNICODE_CTRLS}'wglUseFontBitmapsW'{$ELSE}'wglUseFontBitmapsA'{$ENDIF};
function wglUseFontOutlines(p1: HDC; p2, p3, p4: DWORD; p5, p6: Single; p7: Integer; p8: PGlyphMetricsFloat): BOOL; stdcall; external opengl32 name {$IFDEF UNICODE_CTRLS}'wglUseFontOutlinesW'{$ELSE}'wglUseFontOutlinesA'{$ENDIF};
function AnsiToOem(const lpszSrc: LPCSTR; lpszDst: LPSTR): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'CharToOemW'{$ELSE}'CharToOemA'{$ENDIF};
function AnsiToOemBuff(lpszSrc: LPCSTR; lpszDst: LPSTR; cchDstLength: DWORD): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'CharToOemBuffA'{$ELSE}'CharToOemBuffA'{$ENDIF};
function AnsiUpper(lpsz: LPSTR): LPSTR; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'CharUpperW'{$ELSE}'CharUpperA'{$ENDIF};
function AnsiUpperBuff(lpsz: LPSTR; cchLength: DWORD): DWORD; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'CharUpperBuffW'{$ELSE}'CharUpperBuffA'{$ENDIF};
function AnsiLower(lpsz: LPSTR): LPSTR; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'CharLowerW'{$ELSE}'CharLowerA'{$ENDIF};
function AnsiLowerBuff(lpsz: LPSTR; cchLength: DWORD): DWORD; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'CharLowerBuffW'{$ELSE}'CharLowerBuffA'{$ENDIF};
function AnsiNext(const lpsz: LPCSTR): LPSTR; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'CharNextW'{$ELSE}'CharNextA'{$ENDIF};
function AnsiPrev(const lpszStart: LPCSTR; const lpszCurrent: LPCSTR): LPSTR; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'CharPrevW'{$ELSE}'CharPrevA'{$ENDIF};
function AppendMenu(hMenu: HMENU; uFlags, uIDNewItem: UINT; lpNewItem: PKOLChar): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'AppendMenuW'{$ELSE}'AppendMenuA'{$ENDIF};
function CallMsgFilter(var lpMsg: TMsg; nCode: Integer): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'CallMsgFilterW'{$ELSE}'CallMsgFilterA'{$ENDIF};
function CallWindowProc(lpPrevWndFunc: TFNWndProc; hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'CallWindowProcW'{$ELSE}'CallWindowProcA'{$ENDIF};
function ChangeDisplaySettings(const lpDevMode: PDeviceMode; dwFlags: DWORD): Longint; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'ChangeDisplaySettingsW'{$ELSE}'ChangeDisplaySettingsA'{$ENDIF};
function ChangeDisplaySettingsEx(lpszDeviceName: PKOLChar; var lpDevMode: TDeviceMode; wnd: HWND; dwFlags: DWORD; lParam: Pointer): Longint; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'ChangeDisplaySettingsExW'{$ELSE}'ChangeDisplaySettingsExA'{$ENDIF};
function ChangeMenu(hMenu: HMENU; cmd: UINT; lpszNewItem: PKOLChar; cmdInsert: UINT; flags: UINT): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'ChangeMenuW'{$ELSE}'ChangeMenuA'{$ENDIF};
function CharLower(lpsz: PKOLChar): PKOLChar; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'CharLowerW'{$ELSE}'CharLowerA'{$ENDIF};
function CharLowerBuff(lpsz: PKOLChar; cchLength: DWORD): DWORD; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'CharLowerBuffW'{$ELSE}'CharLowerBuffA'{$ENDIF};
function CharNext(lpsz: PKOLChar): PKOLChar; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'CharNextW'{$ELSE}'CharNextA'{$ENDIF};
function CharNextEx(CodePage: Word; lpCurrentChar: LPCSTR; dwFlags: DWORD): LPSTR; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'CharNextExW'{$ELSE}'CharNextExA'{$ENDIF};
function CharPrev(lpszStart: PKOLChar; lpszCurrent: PKOLChar): PKOLChar; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'CharPrevW'{$ELSE}'CharPrevA'{$ENDIF};
function CharPrevEx(CodePage: Word; lpStart, lpCurrentChar: LPCSTR; dwFlags: DWORD): LPSTR; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'CharPrevExW'{$ELSE}'CharPrevExA'{$ENDIF};
function CharToOem(lpszSrc: PKOLChar; lpszDst: PKOLChar): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'CharToOemW'{$ELSE}'CharToOemA'{$ENDIF};
function CharToOemBuff(lpszSrc: PKOLChar; lpszDst: PKOLChar; cchDstLength: DWORD): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'CharToOemBuffW'{$ELSE}'CharToOemBuffA'{$ENDIF};
function CharUpper(lpsz: PKOLChar): PKOLChar; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'CharUpperW'{$ELSE}'CharUpperA'{$ENDIF};
function CharUpperBuff(lpsz: PKOLChar; cchLength: DWORD): DWORD; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'CharUpperBuffW'{$ELSE}'CharUpperBuffA'{$ENDIF};
function CopyAcceleratorTable(hAccelSrc: HACCEL; var lpAccelDst; cAccelEntries: Integer): Integer; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'CopyAcceleratorTableW'{$ELSE}'CopyAcceleratorTableA'{$ENDIF};
function CreateAcceleratorTable(var Accel; Count: Integer): HACCEL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'CreateAcceleratorTableW'{$ELSE}'CreateAcceleratorTableA'{$ENDIF};
function CreateDesktop(lpszDesktop, lpszDevice: PKOLChar; pDevmode: PDeviceMode; dwFlags: DWORD; dwDesiredAccess: DWORD; lpsa: PSecurityAttributes): HDESK; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'CreateDesktopW'{$ELSE}'CreateDesktopA'{$ENDIF};
function CreateDialogIndirectParam(hInstance: HINST; const lpTemplate: TDlgTemplate; hWndParent: HWND; lpDialogFunc: TFNDlgProc; dwInitParam: LPARAM): HWND; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'CreateDialogIndirectParamW'{$ELSE}'CreateDialogIndirectParamA'{$ENDIF};
function CreateDialogParam(hInstance: HINST; lpTemplateName: PKOLChar; hWndParent: HWND; lpDialogFunc: TFNDlgProc; dwInitParam: LPARAM): HWND; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'CreateDialogParamW'{$ELSE}'CreateDialogParamA'{$ENDIF};
function CreateMDIWindow(lpClassName, lpWindowName: PKOLChar; dwStyle: DWORD; X, Y, nWidth, nHeight: Integer; hWndParent: HWND; hInstance: HINST; lParam: LPARAM): HWND; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'CreateMDIWindowW'{$ELSE}'CreateMDIWindowA'{$ENDIF};
function CreateWindowEx(dwExStyle: DWORD; lpClassName: PKOLChar; lpWindowName: PKOLChar; dwStyle: DWORD; X, Y, nWidth, nHeight: Integer; hWndParent: HWND; hMenu: HMENU; hInstance: HINST; lpParam: Pointer): HWND; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'CreateWindowExW'{$ELSE}'CreateWindowExA'{$ENDIF};
function CreateWindowStation(lpwinsta: PKOLChar; dwReserved, dwDesiredAccess: DWORD; lpsa: PSecurityAttributes): HWINSTA; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'CreateWindowStationW'{$ELSE}'CreateWindowStationA'{$ENDIF};
function DefDlgProc(hDlg: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'DefDlgProcW'{$ELSE}'DefDlgProcA'{$ENDIF};
function DefFrameProc(hWnd, hWndMDIClient: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'DefFrameProcW'{$ELSE}'DefFrameProcA'{$ENDIF};
function DefMDIChildProc(hWnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'DefMDIChildProcW'{$ELSE}'DefMDIChildProcA'{$ENDIF};
function DefWindowProc(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'DefWindowProcW'{$ELSE}'DefWindowProcA'{$ENDIF};
function DialogBoxIndirectParam(hInstance: HINST; const lpDialogTemplate: TDlgTemplate; hWndParent: HWND; lpDialogFunc: TFNDlgProc; dwInitParam: LPARAM): Integer; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'DialogBoxIndirectParamW'{$ELSE}'DialogBoxIndirectParamA'{$ENDIF};
function DialogBoxParam(hInstance: HINST; lpTemplateName: PKOLChar; hWndParent: HWND; lpDialogFunc: TFNDlgProc; dwInitParam: LPARAM): Integer; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'DialogBoxParamW'{$ELSE}'DialogBoxParamA'{$ENDIF};
function DispatchMessage(const lpMsg: TMsg): LRESULT; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'DispatchMessageW'{$ELSE}'DispatchMessageA'{$ENDIF};
function DlgDirList(hDlg: HWND; lpPathSpec: PKOLChar; nIDListBox, nIDStaticPath: Integer; uFileType: UINT): Integer; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'DlgDirListW'{$ELSE}'DlgDirListA'{$ENDIF};
function DlgDirListComboBox(hDlg: HWND; lpPathSpec: PKOLChar; nIDComboBox, nIDStaticPath: Integer; uFiletype: UINT): Integer; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'DlgDirListComboBoxW'{$ELSE}'DlgDirListComboBoxA'{$ENDIF};
function DlgDirSelectComboBoxEx(hDlg: HWND; lpString: PKOLChar; nCount, nIDComboBox: Integer): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'DlgDirSelectComboBoxExW'{$ELSE}'DlgDirSelectComboBoxExA'{$ENDIF};
function DlgDirSelectEx(hDlg: HWND; lpString: PKOLChar; nCount, nIDListBox: Integer): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'DlgDirSelectExW'{$ELSE}'DlgDirSelectExA'{$ENDIF};
function DrawState(DC: HDC; Brush: HBRUSH; CBFunc: TFNDrawStateProc; lData: LPARAM; wData: WPARAM; x, y, cx, cy: Integer; Flags: UINT): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'DrawStateW'{$ELSE}'DrawStateA'{$ENDIF};
function DrawText(hDC: HDC; lpString: PKOLChar; nCount: Integer; var lpRect: TRect; uFormat: UINT): Integer; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'DrawTextW'{$ELSE}'DrawTextA'{$ENDIF};
function DrawTextEx(DC: HDC; lpchText: PKOLChar; cchText: Integer; var p4: TRect; dwDTFormat: UINT; DTParams: PDrawTextParams): Integer; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'DrawTextExW'{$ELSE}'DrawTextExA'{$ENDIF};
function EnumDesktops(hwinsta: HWINSTA; lpEnumFunc: TFNDeskTopEnumProc; lParam: LPARAM): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'EnumDesktopsW'{$ELSE}'EnumDesktopsA'{$ENDIF};
function EnumDisplaySettings(lpszDeviceName: PKOLChar; iModeNum: DWORD; var lpDevMode: TDeviceMode): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'EnumDisplaySettingsW'{$ELSE}'EnumDisplaySettingsA'{$ENDIF};
function EnumDisplayDevices(Unused: Pointer; iDevNum: DWORD; var lpDisplayDevice: TDisplayDevice; dwFlags: DWORD): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'EnumDisplayDevicesW'{$ELSE}'EnumDisplayDevicesA'{$ENDIF};
function EnumProps(hWnd: HWND; lpEnumFunc: TFNPropEnumProc): Integer; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'EnumPropsW'{$ELSE}'EnumPropsA'{$ENDIF};
function EnumPropsEx(hWnd: HWND; lpEnumFunc: TFNPropEnumProcEx; lParam: LPARAM): Integer; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'EnumPropsExW'{$ELSE}'EnumPropsExA'{$ENDIF};
function EnumWindowStations(lpEnumFunc: TFNWinStaEnumProc; lParam: LPARAM): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'EnumWindowStationsW'{$ELSE}'EnumWindowStationsA'{$ENDIF};
function FindWindow(lpClassName, lpWindowName: PKOLChar): HWND; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'FindWindowW'{$ELSE}'FindWindowA'{$ENDIF};
function FindWindowEx(Parent, Child: HWND; ClassName, WindowName: PKOLChar): HWND; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'FindWindowExW'{$ELSE}'FindWindowExA'{$ENDIF};
function GetAltTabInfo(hwnd: HWND; iItem: Integer; var pati: TAltTabInfo; pszItemText: PKOLChar; cchItemText: UINT): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'GetAltTabInfoW'{$ELSE}'GetAltTabInfoA'{$ENDIF};
function GetClassInfo(hInstance: HINST; lpClassName: PKOLChar; var lpWndClass: TWndClass): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'GetClassInfoW'{$ELSE}'GetClassInfoA'{$ENDIF};
function GetClassInfoEx(Instance: HINST; Classname: PKOLChar; var WndClass: TWndClassEx): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'GetClassInfoExW'{$ELSE}'GetClassInfoExA'{$ENDIF};
function GetClassName(hWnd: HWND; lpClassName: PKOLChar; nMaxCount: Integer): Integer; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'GetClassNameW'{$ELSE}'GetClassNameA'{$ENDIF};
function GetClipboardFormatName(format: UINT; lpszFormatName: PKOLChar; cchMaxCount: Integer): Integer; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'GetClipboardFormatNameW'{$ELSE}'GetClipboardFormatNameA'{$ENDIF};
function GetDlgItemText(hDlg: HWND; nIDDlgItem: Integer; lpString: PKOLChar; nMaxCount: Integer): UINT; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'GetDlgItemTextW'{$ELSE}'GetDlgItemTextA'{$ENDIF};
function GetKeyNameText(lParam: Longint; lpString: PKOLChar; nSize: Integer): Integer; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'GetKeyNameTextW'{$ELSE}'GetKeyNameTextA'{$ENDIF};
function GetKeyboardLayoutName(pwszKLID: PKOLChar): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'GetKeyboardLayoutNameW'{$ELSE}'GetKeyboardLayoutNameA'{$ENDIF};
function GetMenuItemInfo(p1: HMENU; p2: UINT; p3: BOOL; var p4: TMenuItemInfo): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'GetMenuItemInfoW'{$ELSE}'GetMenuItemInfoA'{$ENDIF};
function GetMenuString(hMenu: HMENU; uIDItem: UINT; lpString: PKOLChar; nMaxCount: Integer; uFlag: UINT): Integer; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'GetMenuStringW'{$ELSE}'GetMenuStringA'{$ENDIF};
function GetMessage(var lpMsg: TMsg; hWnd: HWND; wMsgFilterMin, wMsgFilterMax: UINT): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'GetMessageW'{$ELSE}'GetMessageA'{$ENDIF};
function GetProp(hWnd: HWND; lpString: PKOLChar): THandle; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'GetPropW'{$ELSE}'GetPropA'{$ENDIF};
function GetTabbedTextExtent(hDC: HDC; lpString: PKOLChar; nCount, nTabPositions: Integer; var lpnTabStopPositions): DWORD; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'GetTabbedTextExtentW'{$ELSE}'GetTabbedTextExtentA'{$ENDIF};
function GetUserObjectInformation(hObj: THandle; nIndex: Integer; pvInfo: Pointer; nLength: DWORD; var lpnLengthNeeded: DWORD): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'GetUserObjectInformationW'{$ELSE}'GetUserObjectInformationA'{$ENDIF};
function GetWindowModuleFileName(hwnd: HWND; pszFileName: PKOLChar; cchFileNameMax: UINT): UINT; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'GetWindowModuleFileNameW'{$ELSE}'GetWindowModuleFileNameA'{$ENDIF};
function GetWindowText(hWnd: HWND; lpString: PKOLChar; nMaxCount: Integer): Integer; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'GetWindowTextW'{$ELSE}'GetWindowTextA'{$ENDIF};
function GetWindowTextLength(hWnd: HWND): Integer; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'GetWindowTextLengthW'{$ELSE}'GetWindowTextLengthA'{$ENDIF};
function GrayString(hDC: HDC; hBrush: HBRUSH; lpOutputFunc: TFNGrayStringProc; lpData: LPARAM; nCount, X, Y, nWidth, nHeight: Integer): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'GrayStringW'{$ELSE}'GrayStringA'{$ENDIF};
function InsertMenu(hMenu: HMENU; uPosition, uFlags, uIDNewItem: UINT; lpNewItem: PKOLChar): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'InsertMenuW'{$ELSE}'InsertMenuA'{$ENDIF};
function InsertMenuItem(p1: HMENU; p2: UINT; p3: BOOL; const p4: TMenuItemInfo): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'InsertMenuItemW'{$ELSE}'InsertMenuItemA'{$ENDIF};
function IsCharAlpha(ch: KOLChar): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'IsCharAlphaW'{$ELSE}'IsCharAlphaA'{$ENDIF};
function IsCharAlphaNumeric(ch: KOLChar): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'IsCharAlphaNumericW'{$ELSE}'IsCharAlphaNumericA'{$ENDIF};
function IsCharLower(ch: KOLChar): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'IsCharLowerW'{$ELSE}'IsCharLowerA'{$ENDIF};
function IsCharUpper(ch: KOLChar): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'IsCharUpperW'{$ELSE}'IsCharUpperA'{$ENDIF};
function IsDialogMessage(hDlg: HWND; var lpMsg: TMsg): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'IsDialogMessageW'{$ELSE}'IsDialogMessageA'{$ENDIF};
function LoadAccelerators(hInstance: HINST; lpTableName: PKOLChar): HACCEL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'LoadAcceleratorsW'{$ELSE}'LoadAcceleratorsA'{$ENDIF};
function LoadBitmap(hInstance: HINST; lpBitmapName: PKOLChar): HBITMAP; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'LoadBitmapW'{$ELSE}'LoadBitmapA'{$ENDIF};
function LoadCursor(hInstance: HINST; lpCursorName: PKOLChar): HCURSOR; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'LoadCursorW'{$ELSE}'LoadCursorA'{$ENDIF};
function LoadCursorFromFile(lpFileName: PKOLChar): HCURSOR; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'LoadCursorFromFileW'{$ELSE}'LoadCursorFromFileA'{$ENDIF};
function LoadIcon(hInstance: HINST; lpIconName: PKOLChar): HICON; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'LoadIconW'{$ELSE}'LoadIconA'{$ENDIF};
function LoadImage(hInst: HINST; ImageName: PKOLChar; ImageType: UINT; X, Y: Integer; Flags: UINT): THandle; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'LoadImageW'{$ELSE}'LoadImageA'{$ENDIF};
function LoadKeyboardLayout(pwszKLID: PKOLChar; Flags: UINT): HKL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'LoadKeyboardLayoutW'{$ELSE}'LoadKeyboardLayoutA'{$ENDIF};
function LoadMenu(hInstance: HINST; lpMenuName: PKOLChar): HMENU; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'LoadMenuW'{$ELSE}'LoadMenuA'{$ENDIF};
function LoadMenuIndirect(lpMenuTemplate: Pointer): HMENU; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'LoadMenuIndirectW'{$ELSE}'LoadMenuIndirectA'{$ENDIF};
function LoadString(hInstance: HINST; uID: UINT; lpBuffer: PKOLChar; nBufferMax: Integer): Integer; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'LoadStringW'{$ELSE}'LoadStringA'{$ENDIF};
function MapVirtualKey(uCode, uMapType: UINT): UINT; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'MapVirtualKeyW'{$ELSE}'MapVirtualKeyA'{$ENDIF};
function MapVirtualKeyEx(uCode, uMapType: UINT; dwhkl: HKL): UINT; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'MapVirtualKeyExW'{$ELSE}'MapVirtualKeyExA'{$ENDIF};
function MessageBox(hWnd: HWND; lpText, lpCaption: PKOLChar; uType: UINT): Integer; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'MessageBoxW'{$ELSE}'MessageBoxA'{$ENDIF};
function MessageBoxEx(hWnd: HWND; lpText, lpCaption: PKOLChar; uType: UINT; wLanguageId: Word): Integer; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'MessageBoxExW'{$ELSE}'MessageBoxExA'{$ENDIF};
function MessageBoxIndirect(const MsgBoxParams: TMsgBoxParams): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'MessageBoxIndirectW'{$ELSE}'MessageBoxIndirectA'{$ENDIF};
function ModifyMenu(hMnu: HMENU; uPosition, uFlags, uIDNewItem: UINT; lpNewItem: PKOLChar): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'ModifyMenuW'{$ELSE}'ModifyMenuA'{$ENDIF};
function OemToAnsi(const lpszSrc: LPCSTR; lpszDst: LPSTR): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'OemToCharW'{$ELSE}'OemToCharA'{$ENDIF};
function OemToAnsiBuff(lpszSrc: LPCSTR; lpszDst: LPSTR; cchDstLength: DWORD): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'OemToCharBuffW'{$ELSE}'OemToCharBuffA'{$ENDIF};
function OemToChar(lpszSrc: PKOLChar; lpszDst: PKOLChar): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'OemToCharW'{$ELSE}'OemToCharA'{$ENDIF};
function OemToCharBuff(lpszSrc: PKOLChar; lpszDst: PKOLChar; cchDstLength: DWORD): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'OemToCharBuffW'{$ELSE}'OemToCharBuffA'{$ENDIF};
function OpenDesktop(lpszDesktop: PKOLChar; dwFlags: DWORD; fInherit: BOOL; dwDesiredAccess: DWORD): HDESK; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'OpenDesktopW'{$ELSE}'OpenDesktopA'{$ENDIF};
function OpenWindowStation(lpszWinSta: PKOLChar; fInherit: BOOL; dwDesiredAccess: DWORD): HWINSTA; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'OpenWindowStationW'{$ELSE}'OpenWindowStationA'{$ENDIF};
function PeekMessage(var lpMsg: TMsg; hWnd: HWND; wMsgFilterMin, wMsgFilterMax, wRemoveMsg: UINT): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'PeekMessageW'{$ELSE}'PeekMessageA'{$ENDIF};
function PostMessage(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'PostMessageW'{$ELSE}'PostMessageA'{$ENDIF};
function PostThreadMessage(idThread: DWORD; Msg: UINT; wParam: WPARAM; lParam: LPARAM): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'PostThreadMessageW'{$ELSE}'PostThreadMessageA'{$ENDIF};
function RealGetWindowClass(hwnd: HWND; pszType: PKOLChar; cchType: UINT): UINT; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'RealGetWindowClassW'{$ELSE}'RealGetWindowClassA'{$ENDIF};
function RegisterClass(const lpWndClass: TWndClass): ATOM; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'RegisterClassW'{$ELSE}'RegisterClassA'{$ENDIF};
function RegisterClassEx(const WndClass: TWndClassEx): ATOM; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'RegisterClassExW'{$ELSE}'RegisterClassExA'{$ENDIF};
function RegisterClipboardFormat(lpszFormat: PKOLChar): UINT; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'RegisterClipboardFormatW'{$ELSE}'RegisterClipboardFormatA'{$ENDIF};
function RegisterDeviceNotification(hRecipient: THandle; NotificationFilter: Pointer; Flags: DWORD): HDEVNOTIFY; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'RegisterDeviceNotificationW'{$ELSE}'RegisterDeviceNotificationA'{$ENDIF};
function RegisterWindowMessage(lpString: PKOLChar): UINT; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'RegisterWindowMessageW'{$ELSE}'RegisterWindowMessageA'{$ENDIF};
function RemoveProp(hWnd: HWND; lpString: PKOLChar): THandle; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'RemovePropW'{$ELSE}'RemovePropA'{$ENDIF};
function SendDlgItemMessage(hDlg: HWND; nIDDlgItem: Integer; Msg: UINT; wParam: WPARAM; lParam: LPARAM): Longint; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'SendDlgItemMessageW'{$ELSE}'SendDlgItemMessageA'{$ENDIF};
function SendMessage(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'SendMessageW'{$ELSE}'SendMessageA'{$ENDIF};
function SendMessageCallback(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM; lpResultCallBack: TFNSendAsyncProc; dwData: DWORD): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'SendMessageCallbackW'{$ELSE}'SendMessageCallbackA'{$ENDIF};
function SendMessageTimeout(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM; fuFlags, uTimeout: UINT; var lpdwResult: DWORD): LRESULT; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'SendMessageTimeoutW'{$ELSE}'SendMessageTimeoutA'{$ENDIF};
function SendNotifyMessage(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'SendNotifyMessageW'{$ELSE}'SendNotifyMessageA'{$ENDIF};
function SetDlgItemText(hDlg: HWND; nIDDlgItem: Integer; lpString: PKOLChar): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'SetDlgItemTextW'{$ELSE}'SetDlgItemTextA'{$ENDIF};
function SetMenuItemInfo(p1: HMENU; p2: UINT; p3: BOOL; const p4: TMenuItemInfo): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'SetMenuItemInfoW'{$ELSE}'SetMenuItemInfoA'{$ENDIF};
function SetProp(hWnd: HWND; lpString: PKOLChar; hData: THandle): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'SetPropW'{$ELSE}'SetPropA'{$ENDIF};
function SetUserObjectInformation(hObj: THandle; nIndex: Integer; pvInfo: Pointer; nLength: DWORD): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'SetUserObjectInformationW'{$ELSE}'SetUserObjectInformationA'{$ENDIF};
function SetWindowText(hWnd: HWND; lpString: PKOLChar): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'SetWindowTextW'{$ELSE}'SetWindowTextA'{$ENDIF};
function SetWindowsHook(nFilterType: Integer; pfnFilterProc: TFNHookProc): HHOOK; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'SetWindowsHookW'{$ELSE}'SetWindowsHookA'{$ENDIF};
function SetWindowsHookEx(idHook: Integer; lpfn: TFNHookProc; hmod: HINST; dwThreadId: DWORD): HHOOK; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'SetWindowsHookExW'{$ELSE}'SetWindowsHookExA'{$ENDIF};
function SystemParametersInfo(uiAction, uiParam: UINT; pvParam: Pointer; fWinIni: UINT): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'SystemParametersInfoW'{$ELSE}'SystemParametersInfoA'{$ENDIF};
function TabbedTextOut(hDC: HDC; X, Y: Integer; lpString: PKOLChar; nCount, nTabPositions: Integer; var lpnTabStopPositions; nTabOrigin: Integer): Longint; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'TabbedTextOutW'{$ELSE}'TabbedTextOutA'{$ENDIF};
function TranslateAccelerator(hWnd: HWND; hAccTable: HACCEL; var lpMsg: TMsg): Integer; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'TranslateAcceleratorW'{$ELSE}'TranslateAcceleratorA'{$ENDIF};
function UnregisterClass(lpClassName: PKOLChar; hInstance: HINST): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'UnregisterClassW'{$ELSE}'UnregisterClassA'{$ENDIF};
function VkKeyScan(ch: KOLChar): SHORT; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'VkKeyScanW'{$ELSE}'VkKeyScanA'{$ENDIF};
function VkKeyScanEx(ch: KOLChar; dwhkl: HKL): SHORT; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'VkKeyScanExW'{$ELSE}'VkKeyScanExA'{$ENDIF};
function WinHelp(hWndMain: HWND; lpszHelp: PKOLChar; uCommand: UINT; dwData: DWORD): BOOL; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'WinHelpW'{$ELSE}'WinHelpA'{$ENDIF};
function wsprintf(Output: PKOLChar; Format: PKOLChar): Integer; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'wsprintfW'{$ELSE}'wsprintfA'{$ENDIF};
function wvsprintf(Output: PKOLChar; Format: PKOLChar; arglist: va_list): Integer; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'wvsprintfW'{$ELSE}'wvsprintfA'{$ENDIF};
function SHGetFileInfo(pszPath: PKOLChar; dwFileAttributes: DWORD; var psfi: TSHFileInfo; cbFileInfo, uFlags: UINT): PtrUInt; stdcall; external 'shell32.dll' name {$IFDEF UNICODE_CTRLS}'SHGetFileInfoW'{$ELSE}'SHGetFileInfoA'{$ENDIF};
function SHBrowseForFolder(var lpbi: TBrowseInfo): PItemIDList; stdcall; external 'shell32.dll' name {$IFDEF UNICODE_CTRLS}'SHBrowseForFolderW'{$ELSE}'SHBrowseForFolderA'{$ENDIF};
function SHBrowseForFolderA(var lpbi: TBrowseInfoA): PItemIDList; stdcall; external 'shell32.dll' name 'SHBrowseForFolderA';
function SHGetPathFromIDList(pidl: PItemIDList; pszPath: PKOLChar): BOOL; stdcall; external 'shell32.dll' name {$IFDEF UNICODE_CTRLS}'SHGetPathFromIDListW'{$ELSE}'SHGetPathFromIDListA'{$ENDIF};
function SHGetPathFromIDListA(pidl: PItemIDList; pszPath: PAnsiChar): BOOL; stdcall; external 'shell32.dll' name 'SHGetPathFromIDListA';
function SHFileOperation(const lpFileOp: TSHFileOpStruct): Integer; stdcall; external 'shell32.dll' name {$IFDEF UNICODE_CTRLS}'SHFileOperationW'{$ELSE}'SHFileOperationA'{$ENDIF};
function Shell_NotifyIcon(dwMessage: DWORD; lpData: PNotifyIconData): BOOL; stdcall; external 'shell32.dll' name {$IFDEF UNICODE_CTRLS}'Shell_NotifyIconW'{$ELSE}'Shell_NotifyIconA'{$ENDIF};
function ExtractIcon(hInst: HINST; lpszExeFileName: PKOLChar; nIconIndex: UINT): HICON; stdcall; external 'shell32.dll' name {$IFDEF UNICODE_CTRLS}'ExtractIconW'{$ELSE}'ExtractIconA'{$ENDIF};
function ExtractAssociatedIcon(hInst: HINST; lpIconPath: PKOLChar; var lpiIcon: Word): HICON; stdcall; external 'shell32.dll' name {$IFDEF UNICODE_CTRLS}'ExtractAssociatedIconW'{$ELSE}'ExtractAssociatedIconA'{$ENDIF};
function DragQueryFile(Drop: HDROP; FileIndex: UINT; FileName: PKOLChar; cb: UINT): UINT; stdcall; external 'shell32.dll' name {$IFDEF UNICODE_CTRLS}'DragQueryFileW'{$ELSE}'DragQueryFileA'{$ENDIF};
function GetOpenFileName(var OpenFile: TOpenFilename): BOOL; stdcall; external 'comdlg32.dll' name {$IFDEF UNICODE_CTRLS}'GetOpenFileNameW'{$ELSE}'GetOpenFileNameA'{$ENDIF};
function GetSaveFileName(var OpenFile: TOpenFilename): BOOL; stdcall; external 'comdlg32.dll' name {$IFDEF UNICODE_CTRLS}'GetSaveFileNameW'{$ELSE}'GetSaveFileNameA'{$ENDIF};
function ChooseFont(var ChooseFont: TChooseFont): BOOL; stdcall; external 'comdlg32.dll' name {$IFDEF UNICODE_CTRLS}'ChooseFontW'{$ELSE}'ChooseFontA'{$ENDIF};
function ChooseColor(var CC: TChooseColor): BOOL; stdcall; external 'comdlg32.dll' name {$IFDEF UNICODE_CTRLS}'ChooseColorW'{$ELSE}'ChooseColorA'{$ENDIF};
function ImageList_LoadImage(Instance: HINST; Bmp: PKOLChar; CX, Grow: Integer; Mask: TColorRef; pType, Flags: Cardinal): HImageList; stdcall; external cctrl name {$IFDEF UNICODE_CTRLS}'ImageList_LoadImageW'{$ELSE}'ImageList_LoadImageA'{$ENDIF};

// NT 4.0 bug workaround - NT 4.0 doesn't test bInitialOwner for zero/nonzero, it tests for 1
function KOLCreateMutex(lpMutexAttributes: PSecurityAttributes; bInitialOwner: Integer; lpName: PKOLChar): THandle; stdcall; external kernel32 name {$IFDEF UNICODE_CTRLS}'CreateMutexW'{$ELSE}'CreateMutexA'{$ENDIF};
function CreateMutex(lpMutexAttributes: PSecurityAttributes; bInitialOwner: BOOL; lpName: PKOLChar): THandle;

function GetWindowLong(hWnd: HWND; nIndex: Integer): Longint; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'GetWindowLongW'{$ELSE}'GetWindowLongA'{$ENDIF};
function SetWindowLong(hWnd: HWND; nIndex: Integer; dwNewLong: Longint): Longint; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'SetWindowLongW'{$ELSE}'SetWindowLongA'{$ENDIF};
function GetClassLong(hWnd: HWND; nIndex: Integer): DWORD; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'GetClassLongW'{$ELSE}'GetClassLongA'{$ENDIF};
function SetClassLong(hWnd: HWND; nIndex: Integer; dwNewLong: Longint): DWORD; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'SetClassLongW'{$ELSE}'SetClassLongA'{$ENDIF};

{$IFDEF WIN64}
function GetWindowLongPtr(hWnd: HWND; nIndex: Integer): LONG_PTR; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'GetWindowLongPtrW'{$ELSE}'GetWindowLongPtrA'{$ENDIF};
function SetWindowLongPtr(hWnd: HWND; nIndex: Integer; dwNewLong: LONG_PTR): LONG_PTR; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'SetWindowLongPtrW'{$ELSE}'SetWindowLongPtrA'{$ENDIF};
function GetClassLongPtr(hWnd: HWND; nIndex: Integer): ULONG_PTR; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'GetClassLongPtrW'{$ELSE}'GetClassLongPtrA'{$ENDIF};
function SetClassLongPtr(hWnd: HWND; nIndex: Integer; dwNewLong: LONG_PTR): ULONG_PTR; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'SetClassLongPtrW'{$ELSE}'SetClassLongPtrA'{$ENDIF};
{$ELSE}
function GetWindowLongPtr(hWnd: HWND; nIndex: Integer): LONG_PTR; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'GetWindowLongW'{$ELSE}'GetWindowLongA'{$ENDIF};
function SetWindowLongPtr(hWnd: HWND; nIndex: Integer; dwNewLong: LONG_PTR): LONG_PTR; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'SetWindowLongW'{$ELSE}'SetWindowLongA'{$ENDIF};
function GetClassLongPtr(hWnd: HWND; nIndex: Integer): ULONG_PTR; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'GetClassLongW'{$ELSE}'GetClassLongA'{$ENDIF};
function SetClassLongPtr(hWnd: HWND; nIndex: Integer; dwNewLong: LONG_PTR): ULONG_PTR; stdcall; external user32 name {$IFDEF UNICODE_CTRLS}'SetClassLongW'{$ELSE}'SetClassLongA'{$ENDIF};
{$ENDIF}

/// externals no ansi/unicode ///
function ImageList_Create(CX, CY: Integer; Flags: UINT; Initial, Grow: Integer): HImageList; stdcall; external cctrl name 'ImageList_Create';
function ImageList_Destroy(ImageList: HImageList): Bool; stdcall; external cctrl name 'ImageList_Destroy';
function ImageList_GetImageCount(ImageList: HImageList): Integer; stdcall; external cctrl name 'ImageList_GetImageCount';
function ImageList_SetImageCount(ImageList: HImageList; Count: Integer): Integer; stdcall; external cctrl name 'ImageList_SetImageCount';
function ImageList_Add(ImageList: HImageList; Image, Mask: HBitmap): Integer; stdcall; external cctrl name 'ImageList_Add';
function ImageList_ReplaceIcon(ImageList: HImageList; Index: Integer; Icon: HIcon): Integer; stdcall; external cctrl name 'ImageList_ReplaceIcon';
function ImageList_SetBkColor(ImageList: HImageList; ClrBk: TColorRef): TColorRef; stdcall; external cctrl name 'ImageList_SetBkColor';
function ImageList_GetBkColor(ImageList: HImageList): TColorRef; stdcall; external cctrl name 'ImageList_GetBkColor';
function ImageList_SetOverlayImage(ImageList: HImageList; Image: Integer; Overlay: Integer): Bool; stdcall; external cctrl name 'ImageList_SetOverlayImage';
function ImageList_Draw(ImageList: HImageList; Index: Integer; Dest: HDC; X, Y: Integer; Style: UINT): Bool; stdcall; external cctrl name 'ImageList_Draw';
function ImageList_Replace(ImageList: HImageList; Index: Integer; Image, Mask: HBitmap): Bool; stdcall; external cctrl name 'ImageList_Replace';
function ImageList_AddMasked(ImageList: HImageList; Image: HBitmap; Mask: TColorRef): Integer; stdcall; external cctrl name 'ImageList_AddMasked';
function ImageList_DrawEx(ImageList: HImageList; Index: Integer; Dest: HDC; X, Y, DX, DY: Integer; Bk, Fg: TColorRef; Style: Cardinal): Bool; stdcall; external cctrl name 'ImageList_DrawEx';
function ImageList_Remove(ImageList: HImageList; Index: Integer): Bool; stdcall; external cctrl name 'ImageList_Remove';
function ImageList_GetIcon(ImageList: HImageList; Index: Integer; Flags: Cardinal): HIcon; stdcall; external cctrl name 'ImageList_GetIcon';
function ImageList_BeginDrag(ImageList: HImageList; Track: Integer; XHotSpot, YHotSpot: Integer): Bool; stdcall; external cctrl name 'ImageList_BeginDrag';
function ImageList_EndDrag: Bool; stdcall; external cctrl name 'ImageList_EndDrag';
function ImageList_DragEnter(LockWnd: HWnd; X, Y: Integer): Bool; stdcall; external cctrl name 'ImageList_DragEnter';
function ImageList_DragLeave(LockWnd: HWnd): Bool; stdcall; external cctrl name 'ImageList_DragLeave';
function ImageList_DragMove(X, Y: Integer): Bool; stdcall; external cctrl name 'ImageList_DragMove';
function ImageList_SetDragCursorImage(ImageList: HImageList; Drag: Integer; XHotSpot, YHotSpot: Integer): Bool; stdcall; external cctrl name 'ImageList_SetDragCursorImage';
function ImageList_DragShowNolock(Show: Bool): Bool; stdcall; external cctrl name 'ImageList_DragShowNolock';
function ImageList_GetDragImage(Point, HotSpot: PPoint): HImageList; stdcall; external cctrl name 'ImageList_GetDragImage';
function ImageList_GetIconSize(ImageList: HImageList; var CX, CY: Integer): Bool; stdcall; external cctrl name 'ImageList_GetIconSize';
function ImageList_SetIconSize(ImageList: HImageList; CX, CY: Integer): Bool; stdcall; external cctrl name 'ImageList_SetIconSize';
function ImageList_GetImageInfo(ImageList: HImageList; Index: Integer; var ImageInfo: TImageInfo): Bool; stdcall; external cctrl name 'ImageList_GetImageInfo';
function ImageList_Merge(ImageList1: HImageList; Index1: Integer; ImageList2: HImageList; Index2: Integer; DX, DY: Integer): HImageList; stdcall; external cctrl name 'ImageList_Merge';
function SysAllocStringLen(psz: PWideChar; len: Integer): PWideChar; stdcall; external 'oleaut32.dll' name 'SysAllocStringLen';
procedure SysFreeString(psz: PWideChar); stdcall; external 'oleaut32.dll' name 'SysFreeString';
function OleInitialize(pwReserved: Pointer): HResult; stdcall; external 'ole32.dll' name 'OleInitialize';
procedure OleUninitialize; stdcall; external 'ole32.dll' name 'OleUninitialize';
procedure CoTaskMemFree(pv: Pointer); stdcall; external 'ole32.dll' name 'CoTaskMemFree';
function RevokeDragDrop(wnd: HWnd): HResult; stdcall; external 'ole32.dll' name 'RevokeDragDrop';
function timeSetEvent(uDelay, uResolution: UINT; lpFunction: TFNTimeCallBack; dwUser: DWORD_PTR; uFlags: UINT): THandle; stdcall; external 'winmm.dll' name 'timeSetEvent';
function timeKillEvent(uTimerID: UINT): THandle; stdcall; external 'winmm.dll' name 'timeKillEvent';
function timeBeginPeriod(uPeriod: UINT): UINT; stdcall; external 'winmm.dll' name 'timeBeginPeriod';
function timeEndPeriod(uPeriod: UINT): UINT; stdcall; external 'winmm.dll' name 'timeEndPeriod';
function DragQueryPoint(Drop: HDROP; var Point: TPoint): BOOL; stdcall; external 'shell32.dll' name 'DragQueryPoint';
procedure DragFinish(Drop: HDROP); stdcall; external 'shell32.dll' name 'DragFinish';
procedure DragAcceptFiles(Wnd: HWND; Accept: BOOL); stdcall; external 'shell32.dll' name 'DragAcceptFiles';
function CreateMappedBitmap(Instance: HINST; Bitmap: PtrInt; Flags: UINT; ColorMap: PColorMap; NumMaps: Integer): HBitmap; stdcall; external cctrl name 'CreateMappedBitmap';
{* Creates mapped bitmap replacing colors correspondently to the
   ColorMap (each pare of colors defines color replaced and a color
   used for replace it in the bitmap). See also CreateMappedBitmapEx. }
procedure InitCommonControls; stdcall; external cctrl name 'InitCommonControls';
{* ComCtrl32 controls initialization. }
function BroadcastSystemMessage(Flags: DWORD; Recipients: PDWORD; uiMessage: UINT; wParam: WPARAM; lParam: LPARAM): Longint; stdcall; external user32 name 'BroadcastSystemMessage';
{* Redefine here incorrectly declared BroadcastSystemMessage API function.
   It should not refer to BroadcastSystemMessageA, which is not present in
   earlier versions of Windows95, but to BroadcastSystemMessage, which is
   present in all Windows95/98/Me and NT/2K/XP. *}

//API TEMPLATE {$IFDEF UNICODE_CTRLS}{$ELSE}{$ENDIF}

