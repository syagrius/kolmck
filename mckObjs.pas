{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        KKKKK    KKKKK    OOOOOOOOO    LLLLL
        KKKKK    KKKKK  OOOOOOOOOOOOO  LLLLL
        KKKKK    KKKKK  OOOOO   OOOOO  LLLLL
        KKKKK  KKKKK    OOOOO   OOOOO  LLLLL
        KKKKKKKKKK      OOOOO   OOOOO  LLLLL
        KKKKK  KKKKK    OOOOO   OOOOO  LLLLL
        KKKKK    KKKKK  OOOOO   OOOOO  LLLLL
        KKKKK    KKKKK  OOOOOOOOOOOOO  LLLLLLLLLLLLL     kkkkk
        KKKKK    KKKKK    OOOOOOOOO    LLLLLLLLLLLLL    kkkkk
                                                       kkkkk
    mmmmm  mmmmm   mmmmmm          cccccccccccc       kkkkk   kkkkk
   mmmmmmmm   mmmmm     mmmmm   cccccc       ccccc   kkkkk kkkkk
  mmmmmmmm   mmmmm     mmmmm   cccccc               kkkkkkkk
 mmmmm      mmmmm     mmmmm   cccccc      ccccc    kkkkk  kkkkk
mmmmm      mmmmm     mmmmm     cccccccccccc       kkkkk     kkkkk

  Key Objects Library (C) 2000 by Kladov Vladimir.
  KOL Mirror Classes Kit (C) 2000 by Kladov Vladimir.
}
unit mckObjs;

interface

{$I KOLDEF.INC}

uses
  KOL, Classes, Forms, Controls, Dialogs, Windows, Messages, extctrls, stdctrls,
  comctrls, SysUtils, Graphics, mirror, ShellAPI, buttons, mckFileFilterEditor,
//////////////////////////////////////////
     {$IFDEF _D6orHigher}               //
     DesignIntf, DesignEditors,         //
     {$ELSE}                            //
//////////////////////////////////////////
     DsgnIntf,
//////////////////////////////////////////
     {$ENDIF}                           //
//////////////////////////////////////////
     imglist, TypInfo, menus;

type
  //============================================================================
  //---- MIRROR FOR A TIMER ----
  //---- «≈– ¿ÀŒ ƒÀﬂ “¿…Ã≈–¿ ----
  TKOLTimer = class(TKOLObj)
  private
    FEnabled: Boolean;
    FInterval: Integer;
    FOnTimer: TOnEvent;
    FPeriodic: Boolean;
    FMultimedia: Boolean;
    FResolution: Integer;
    procedure SetEnabled(const Value: Boolean);
    procedure SetInterval(const Value: Integer);
    procedure SetOnTimer(const Value: TOnEvent);
    procedure SetMultimedia(const Value: Boolean);
    procedure SetPeriodic(const Value: Boolean);
    procedure SetResolution(const Value: Integer);
  protected
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: string); override;
    procedure AssignEvents(SL: TStringList; const AName: string); override;
    procedure SetupLast(SL: TStringList; const AName, AParent, Prefix: string); override;
  public
    function TypeName: string; override;
    constructor Create(AOwner: TComponent); override;
  published
    property Interval: Integer read FInterval write SetInterval;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property OnTimer: TOnEvent read FOnTimer write SetOnTimer;
    property Multimedia: Boolean read FMultimedia write SetMultimedia;
    property Resolution: Integer read FResolution write SetResolution;
    property Periodic: Boolean read FPeriodic write SetPeriodic;
  end;

  //============================================================================
  //---- MIRROR FOR A THREAD ----
  //---- «≈– ¿ÀŒ ƒÀﬂ Õ»“» ----
  TPriorityClass = (pcNormal, pcIdle, pcHigh, pcRealTime);

  TThreadPriority = (tpNormal, tpBelowNormal, tpLowest, tpIdle, tpAboveNormal, tpHighest, tpCritical);

  TKOLThread = class(TKOLObj)
  private
    FPriorityClass: TPriorityClass;
    FThreadPriority: TThreadPriority;
    FOnExecute: TOnThreadExecute;
    FOnSuspend: TObjectMethod;
    FOnResume: TOnEvent;
    FstartSuspended: Boolean;
    F_AutoFree: Boolean;
    FPriorityBoost: Boolean;
    procedure SetPriorityClass(const Value: TPriorityClass);
    procedure SetThreadPriority(const Value: TThreadPriority);
    procedure SetOnExecute(const Value: TOnThreadExecute);
    procedure SetOnSuspend(const Value: TObjectMethod);
    procedure SetOnResume(const Value: TOnEvent);
    procedure SetstartSuspended(const Value: Boolean);
    procedure SetAutoFree(const Value: Boolean);
    procedure SetPriorityBoost(const Value: Boolean);
  protected
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: string); override;
    function NotAutoFree: Boolean; override;
    function BestEventName: string; override;
  public
    procedure AssignEvents(SL: TStringList; const AName: string); override;
    constructor Create(AOwner: TComponent); override;
  published
    property PriorityClass: TPriorityClass read FPriorityClass write SetPriorityClass;
    property ThreadPriority: TThreadPriority read FThreadPriority write SetThreadPriority;
    property OnExecute: TOnThreadExecute read FOnExecute write SetOnExecute;
    property OnSuspend: TObjectMethod read FOnSuspend write SetOnSuspend;
    property OnResume: TOnEvent read FOnResume write SetOnResume;
    property startSuspended: Boolean read FstartSuspended write SetstartSuspended;
    property AutoFree: Boolean read F_AutoFree write SetAutoFree;
    property PriorityBoost: Boolean read FPriorityBoost write SetPriorityBoost;
  end;

  //============================================================================
  //---- MIRROR FOR AN IMAGELIST ----
  //---- «≈– ¿ÀŒ ƒÀﬂ —œ»— ¿ –»—”Õ Œ¬ ----
  TKOLImageList = class(TKOLObj)
  private
    FImgWidth: Integer;
    FImgHeight: Integer;
    FCount: Integer;
    FBitmap: TBitmap;
    FSystemImageList: Boolean;
    FTransparentColor: TColor;
    FColors: TImageListColors;
    FMasked: Boolean;
    FBkColor: TColor;
    FAllowCompression: Boolean;
    FForce32bit: Boolean;
    procedure SetImgHeight(Value: Integer);
    procedure SetImgWidth(Value: Integer);
    procedure SetCount(const Value: Integer);
    procedure SetBitmap(const Value: TBitmap);
    procedure SetSystemImageList(const Value: Boolean);
    function GetBitmap: TBitmap;
    procedure SetTransparentColor(const Value: TColor);
    function GetTransparentColor: TColor;
    procedure SetColors(const Value: TImageListColors);
    procedure SetMasked(const Value: Boolean);
    procedure SetBkColor(const Value: TColor);
    function GetImageListHandle: THandle;
    procedure AssignBitmapToKOLImgList;
    procedure SetAllowCompression(const Value: Boolean);
    procedure SetForce32bit(const Value: Boolean);
  protected
    FKOLImgList: PImageList;
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(Value: TPersistent); override;
    property Handle: THandle read GetImageListHandle;
  published
    property ImgWidth: Integer read FImgWidth write SetImgWidth;
    property ImgHeight: Integer read FImgHeight write SetImgHeight;
    property Count: Integer read FCount write SetCount;
    property bitmap: TBitmap read GetBitmap write SetBitmap;
    property TransparentColor: TColor read GetTransparentColor write SetTransparentColor;
    property systemimagelist: Boolean read FSystemImageList write SetSystemImageList;
    property Colors: TImageListColors read FColors write SetColors;
    property Masked: Boolean read FMasked write SetMasked;
    property BkColor: TColor read FBkColor write SetBkColor;
    property AllowCompression: Boolean read FAllowCompression write SetAllowCompression default TRUE;
    property Force32bit: Boolean read FForce32bit write SetForce32bit;
  end;

  TKOLImageListEditor = class(TComponentEditor)
  private
  protected
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  //----------------------------------------------------------------------------
  //---- MIRROR FOR OPENSAVE FILE DIALOG ----
  //---- «≈– ¿ÀŒ ƒÀﬂ ƒ»¿ÀŒ√¿ ¬€¡Œ–¿ ‘¿…À¿ ----
  TKOLOpenSaveDialog = class(TKOLObj)
  private
    FOptions: TOpenSaveOptions;
    FInitialDir: string;
    FFilter: string;
    FFilterIndex: Integer;
    FTitle: string;
    FDefExtension: string;
    FOpenDialog: Boolean;
    FTemplateName: string;
    FNoPlaceBar: Boolean;
    procedure SetOptions(const Value: TOpenSaveOptions);
    procedure SetInitialDir(const Value: string);
    procedure SetFilter(const Value: string);
    procedure SetFilterIndex(const Value: Integer);
    procedure SetTitle(const Value: string);
    procedure SetDefExtension(const Value: string);
    procedure SetOpenDialog(const Value: Boolean);
    procedure SetTemplateName(const Value: string);
    procedure SetNoPlaceBar(const Value: Boolean);
  protected
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: string); override;
    procedure SetupLast(SL: TStringList; const AName, AParent, Prefix: string); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Options: TOpenSaveOptions read FOptions write SetOptions;
    property NoPlaceBar: Boolean read FNoPlaceBar write SetNoPlaceBar;
    property Title: string read FTitle write SetTitle;
    property TemplateName: string read FTemplateName write SetTemplateName;
    property InitialDir: string read FInitialDir write SetInitialDir;
    property Filter: string read FFilter write SetFilter;
    property FilterIndex: Integer read FFilterIndex write SetFilterIndex;
    property DefExtension: string read FDefExtension write SetDefExtension;
    property OpenDialog: Boolean read FOpenDialog write SetOpenDialog;
    property Localizy;
  end;

  TKOLFileFilter = class(TStringProperty)
  private
  protected
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  //----------------------------------------------------------------------------
  //---- MIRROR FOR OPENDIR DIALOG ----
  //---- «≈– ¿ÀŒ ƒÀﬂ ƒ»¿ÀŒ√¿ ¬€¡Œ–¿ ƒ»–≈ “Œ–»ﬂ ----
  TKOLOpenDirDialog = class(TKOLObj)
  private
    FTitle: string;
    FOptions: TOpenDirOptions;
    FInitialPath: string;
    FCenterOnScreen: Boolean;
    FOnSelChanged: TOnODSelChange;
    FAltDialog: Boolean;
    procedure SetTitle(const Value: string);
    procedure SetOptions(const Value: TOpenDirOptions);
    procedure SetInitialPath(const Value: string);
    procedure SetCenterOnScreen(const Value: Boolean);
    procedure SetOnSelChanged(const Value: TOnODSelChange);
    procedure SetAltDialog(const Value: Boolean);
  protected
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: string); override;
    procedure SetupLast(SL: TStringList; const AName, AParent, Prefix: string); override;
  public
    procedure AssignEvents(SL: TStringList; const AName: string); override;
    constructor Create(AOwner: TComponent); override;
    function TypeName: string; override;
    function AdditionalUnits: string; override;
  published
    property Title: string read FTitle write SetTitle;
    property Options: TOpenDirOptions read FOptions write SetOptions;
    property InitialPath: string read FInitialPath write SetInitialPath;
    property CenterOnScreen: Boolean read FCenterOnScreen write SetCenterOnScreen;
    property OnSelChanged: TOnODSelChange read FOnSelChanged write SetOnSelChanged;
    property Localizy;
    property AltDialog: Boolean read FAltDialog write SetAltDialog;
  end;


  //----------------------------------------------------------------------------
  //---- MIRROR FOR COLOR CHOOSING DIALOG ----
  //---- «≈– ¿ÀŒ ƒÀﬂ ƒ»¿ÀŒ√¿ ¬€¡Œ–¿ ÷¬≈“¿ ----
  TKOLColorDialog = class(TKOLObj)
  private
    FColorCustomOption: TColorCustomOption;
    FCustomColors: array[1..16] of TColor;
    procedure SetColorCustomOption(const Value: TColorCustomOption);
    function GetCustomColor(const Index: Integer): TColor;
    procedure SetCustomColor(const Index: Integer; const Value: TColor);
  protected
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: string); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ColorCustomOption: TColorCustomOption read FColorCustomOption write SetColorCustomOption;
    property CustomColor1: TColor index 1 read GetCustomColor write SetCustomColor;
    property CustomColor2: TColor index 2 read GetCustomColor write SetCustomColor;
    property CustomColor3: TColor index 3 read GetCustomColor write SetCustomColor;
    property CustomColor4: TColor index 4 read GetCustomColor write SetCustomColor;
    property CustomColor5: TColor index 5 read GetCustomColor write SetCustomColor;
    property CustomColor6: TColor index 6 read GetCustomColor write SetCustomColor;
    property CustomColor7: TColor index 7 read GetCustomColor write SetCustomColor;
    property CustomColor8: TColor index 8 read GetCustomColor write SetCustomColor;
    property CustomColor9: TColor index 9 read GetCustomColor write SetCustomColor;
    property CustomColor10: TColor index 10 read GetCustomColor write SetCustomColor;
    property CustomColor11: TColor index 11 read GetCustomColor write SetCustomColor;
    property CustomColor12: TColor index 12 read GetCustomColor write SetCustomColor;
    property CustomColor13: TColor index 13 read GetCustomColor write SetCustomColor;
    property CustomColor14: TColor index 14 read GetCustomColor write SetCustomColor;
    property CustomColor15: TColor index 15 read GetCustomColor write SetCustomColor;
    property CustomColor16: TColor index 16 read GetCustomColor write SetCustomColor;
  end;

  //----------------------------------------------------------------------------
  //---- MIRROR FOR FONT CHOOSING DIALOG ----
  //---- «≈– ¿ÀŒ ƒÀﬂ ƒ»¿ÀŒ√¿ ¬€¡Œ–¿ ÷¬≈“¿ ----
  TKOLFontDialog = class(TKOLObj)
  private
    FMinFontSize: Integer;
    FMaxFontSize: Integer;
    FDevice: KOL.TFontDialogDevice;
    FFont: TKOLFont;
    FOnHelp: TOnEvent;
    FOnApply: TOnEvent;
    FOptions: KOL.TFontDialogOptions;
    procedure SetMinFontSize(const Value: Integer);
    procedure SetMaxFontSize(const Value: Integer);
    procedure SetDevice(const Value: KOL.TFontDialogDevice);
    procedure SetInitFont(const Value: TKOLFont);
    procedure SetOnApply(const Value: TOnEvent);
    procedure SetOnHelp(const Value: TOnEvent);
    procedure SetOptions(const Value: KOL.TFontDialogOptions);
  protected
    procedure AssignEvents(SL: TStringList; const AName: string); override;
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Device: KOL.TFontDialogDevice read FDevice write SetDevice;
    property MinFontSize: Integer read FMinFontSize write SetMinFontSize;
    property MaxFontSize: Integer read FmaxFontSize write SetMaxFontSize;
    property Options: KOL.TFontDialogOptions read FOptions write SetOptions; // default [KOL.fdEffects, KOL.fdInitFont];
    property Font: TKOLFont read FFont write SetInitFont;
    property OnApply: TOnEvent read FOnApply write SetOnApply;
    property OnHelp: TOnEvent read FOnHelp write SetOnHelp;
  end;

  //----------------------------------------------------------------------------
  //---- MIRROR FOR TRAY ICON ----
  //---- «≈– ¿ÀŒ ƒÀﬂ » ŒÕ » ¬ “–≈≈ ----
  TKOLTrayIcon = class(TKOLObj)
  private
    FIcon: TIcon;
    FActive: Boolean;
    FTooltip: string;
    FAutoRecreate: Boolean;
    FOnMouse: TOnTrayIconMouse;
    FNoAutoDeactivate: Boolean;
    procedure SetIcon(const Value: TIcon);
    procedure SetActive(const Value: Boolean);
    procedure SetTooltip(const Value: string);
    procedure SetAutoRecreate(const Value: Boolean);
    procedure SetOnMouse(const Value: TOnTrayIconMouse);
    procedure SetNoAutoDeactivate(const Value: Boolean);
  protected
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: string); override;
    procedure SetupLast(SL: TStringList; const AName, AParent, Prefix: string); override;
    procedure AssignEvents(SL: TStringList; const AName: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Icon: TIcon read FIcon write SetIcon;
    property Active: Boolean read FActive write SetActive;
    property NoAutoDeactivate: Boolean read FNoAutoDeactivate write SetNoAutoDeactivate;
    property Tooltip: string read FTooltip write SetTooltip;
    property AutoRecreate: Boolean read FAutoRecreate write SetAutoRecreate;
    property OnMouse: TOnTrayIconMouse read FOnMouse write SetOnMouse;
    property Localizy;
  end;

type
  KOLTPixelFormat = KOL.TPixelFormat;

function CountSystemColorsUsedInBitmap(Bmp: KOL.PBitmap; ColorList: KOL.PList): KOLTPixelFormat;
//function SaveBitmap( Bitmap: TBitmap; const Path: String ): Boolean;

procedure GenerateBitmapResource(Bitmap: TBitmap; const RsrcName, FileName:
  string; var Updated: Boolean; AllowCompression: Boolean);

procedure GenerateIconResource(Icon: TIcon; const RsrcName, FileName: KOLString; var Updated: Boolean);

procedure RemoveSelection(FD: IFormDesigner);

function String2Pascal(S: string; const Concatenator: string): string;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('KOL', [TKOLTimer, TKOLThread, TKOLImageList, TKOLMainMenu,
    TKOLPopupMenu, TKOLOpenSaveDialog, TKOLOpenDirDialog, TKOLColorDialog, TKOLFontDialog, TKOLTrayIcon]);
  RegisterComponentEditor(TKOLImageList, TKOLImageListEditor);
  RegisterPropertyEditor(TypeInfo(string), TKOLOpenSaveDialog, 'Filter', TKOLFileFilter);
  RegisterPropertyEditor(TypeInfo(TOnODSelChange), TKOLOpenDirDialog, 'OnSelChanged', TKOLOnEventPropEditor);
  RegisterPropertyEditor(TypeInfo(TOnTrayIconMouse), nil, '', TKOLOnEventPropEditor);
end;

function String2PascalStr1(const S: string; const Concatenator: string): string;
var
  I, Strt: Integer;

  function String2DoubleQuotas(const S: string): string;
  var
    I, J: Integer;
  begin
    //if IndexOfChar( S, '''' ) <= 0 then
    if pos('''', S) <= 0 then
      Result := S
    else begin
      J := 0;
      for I := 1 to Length(S) do
        if S[I] = '''' then
          Inc(J);
      SetLength(Result, Length(S) + J);
      J := 1;
      for I := 1 to Length(S) do begin
        Result[J] := S[I];
        Inc(J);
        if S[I] = '''' then begin
          Result[J] := '''';
          Inc(J);
        end;
      end;
    end;
  end;

begin
  Result := '';
  if S = '' then begin
    Result := '''''';
    exit;
  end;
  Strt := 1;
  for I := 1 to Length(S) + 1 do begin
    if (I > Length(S)) or (S[I] < ' ') then begin
      if (I > Strt) and (I > 1) then begin
        if Result <> '' then
          Result := Result + Concatenator;
        Result := Result + '''' + String2DoubleQuotas(Copy(S, Strt, I - Strt)) + '''';
      end;
      if I > Length(S) then
        break;
      if Result <> '' then
        Result := Result + Concatenator
      else
        Result := Result + '''''' + Concatenator;
      // Result := Result + '''''';
      //if   IndexOfChar(Concatenator, ',') > 0 then
      if pos(',', Concatenator) > 0 then
        Result := Result + IntToStr(Integer(S[I]))
      else
        Result := Result + '#' + IntToStr(Integer(S[I]));
      Strt := I + 1;
    end;
  end;
end;

function String2Pascal(S: string; const Concatenator: string): string;
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'String2Pascal', 0

@@e_signature:
  end;
  if Length(S) > 0 then begin
    Result := '';
    while S <> '' do begin
      if Result <> '' then
        Result := Result + Concatenator;
      Result := Result + String2PascalStr1(Copy(S, 1, 255), Concatenator);
      S := Copy(S, 256, MaxInt);
    end;
  end
  else
    Result := '''''';
end;

procedure RemoveSelection(FD: IFormDesigner);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'RemoveSelection', 0

@@e_signature:
  end;
  try
    FD.NoSelection;
  except
    Rpt('*/\* EXCEPTION - Could not remove current selection', WHITE);
  end;
end;

function ColorsAreSystem16(ColorList: PList): Boolean;
const
  SysColors: array[0..15] of TColor = (0, $800000, $8000, $808000, $80, $800080,
    $8080, $808080, $C0C0C0, $FF0000, $FF00, $FFFF00, $FF, $FF00FF, $FFFF, $FFFFFF);
var
  I, J: Integer;
  C: TColor;
  Found: Boolean;
begin
  Result := TRUE;
  for I := 0 to ColorList.Count - 1 do begin
    C := TColor(ColorList.Items[I]);
    Found := FALSE;
    for J := 0 to 15 do
      if SysColors[J] = C then begin
        Found := TRUE;
        break;
      end;
    if not Found then begin
      Rpt('***** Color ' + IntToHex(C, 8) + ' not found in system 16 colors', WHITE);
      Result := FALSE;
      Exit;
    end;
  end;
end;

function ColorsAreSystem256(ColorList: PList): Boolean;
const
  SysColors8bit: array[0..255] of DWORD = ($000000, $C0DCC0, $800000, $808000,
    $008000, $008080, $000080, $800080, $808080, $00FF00, $0000FF, $00FFFF,
    $C0DCC0, $000040, $400040, $000000, $A0A0A4, $C0C0C0, $C0DCC0, $FFFBF0,
    $FFFBF0, $FFFBF0, $FFFBF0, $FFFFFF, $FF0000, $FFFF00, $FF00FF, $FFFFFF,
    $A6CAF0, $402000, $004040, $202040, $202040, $606040, $404040, $E08080,
    $E00080, $C0DCC0, $A0A0A4, $800000, $C02000, $404000, $A04000, $E04000,
    $406000, $A06000, $E06000, $40A000, $202040, $404040, $404040, $E06040,
    $A6CAF0, $C0DCC0, $40E000, $800000, $004000, $604000, $C04000, $006000,
    $606000, $C06000, $00A000, $60A000, $A0A000, $E0A000, $40C000, $A0C000,
    $E0C000, $A0E000, $00E040, $600040, $C00040, $0000FF, $604040, $C04040,
    $006040, $606040, $C06040, $00A040, $C0A000, $00C000, $60C000, $C0C000,
    $60E000, $C0E000, $0000FF, $A00040, $E00040, $404040, $A04040, $E04040,
    $406040, $A06040, $E06040, $40A040, $60A040, $C0A040, $00C040, $60C040,
    $C0C040, $40E040, $A0E040, $E0E040, $400080, $A00080, $E00080, $404080,
    $A04080, $E04080, $406080, $A06080, $A0A040, $E0A040, $40C040, $A0C040,
    $E0C040, $60E040, $C0E040, $000080, $600080, $C00080, $004080, $604080,
    $C04080, $006080, $606080, $C06080, $00A080, $A0C080, $E0C080, $40E080,
    $C0E080, $FF00FF, $A04080, $C00080, $404080, $C04080, $006080, $604080,
    $C06080, $40A080, $A0A0A4, $E0A080, $40C080, $C0C080, $00E080, $A0E080,
    $E000C0, $00A080, $A00080, $000080, $600080, $E00080, $406080, $A06080,
    $E04080, $60A080, $C0A080, $00C080, $40C080, $A0C080, $E0C080, $40E080,
    $A0E080, $E0E080, $400080, $A000C0, $004080, $6040C0, $C040C0, $0060C0,
    $606080, $C060C0, $00A0C0, $60A0C0, $60C080, $C0C080, $00E080, $60C080,
    $C0E080, $0000C0, $6000C0, $C000C0, $4040C0, $A040C0, $E040C0, $4060C0,
    $A060C0, $E06080, $40A0C0, $A0A0C0, $C0A0C0, $00A0C0, $60A0C0, $C0A0C0,
    $00C0C0, $60C0C0, $C0C0C0, $00FFFF, $60E080, $C0DCC0, $4000C0, $A000C0,
    $4040C0, $A040C0, $FF00FF, $4060C0, $E0A0C0, $40A0C0, $A0A0C0, $E0A0C0,
    $40C0C0, $A0C0C0, $E0A0C0, $40C0C0, $C0DCC0, $FFFBF0, $6000C0, $0040C0,
    $6040C0, $C040C0, $0060C0, $6060C0, $A060C0, $E060C0, $40A0C0, $A6CAF0,
    $E0A0C0, $40C0C0, $A6CAF0, $FFFBF0, $60C0C0, $FFFFFF, $60E080, $6060C0,
    $A6CAF0, $606040, $808080, $C0C0C0, $C060C0, $00A0C0, $60A0C0, $A6CAF0,
    $00FFFF, $60C0C0, $A6CAF0, $00FFFF, $A6CAF0, $E06080, $E0E080, $E060C0,
    $A00040, $808080, $A0A0A4, $C0C0C0);
var
  I, J: Integer;
  C: DWORD;
begin
  Result := FALSE;
  for I := 0 to ColorList.Count - 1 do begin
    C := DWORD(ColorList.Items[I]);
    for J := 0 to 255 do begin
      if SysColors8bit[J] = C then begin
        C := 0;
        break;
      end;
    end;
    if C <> 0 then begin
      //Rpt( '~~~ color not found: ' + Int2Hex( C, 6 ), WHITE );
      Exit;
    end;
  end;
  Result := TRUE;
end;

function ColorsAre64K(ColorList: PList): Boolean;
var
  I: Integer;
  C: DWORD;
begin
  Result := FALSE;
  for I := 0 to ColorList.Count - 1 do begin
    C := DWORD(ColorList.Items[I]);
    if (C and $E0C0E0) <> C then begin
      //Rpt( '~~~ color not found: ' + Int2Hex( C, 6 ), WHITE );
      Exit;
    end;
  end;
  Result := TRUE;
end;

function CountSystemColorsUsedInBitmap(Bmp: KOL.PBitmap; ColorList: KOL.PList): KOL.TPixelFormat;
var
  Y, X: Integer;
  L: PDWORD;
  C: TColor;
  R, G, B: Byte;
  not_use_16bpp: Boolean;
begin
  Rpt('CountSystemColorsUsedInBitmap()', YELLOW);
  ColorList.Clear;
  ColorList.Capacity := 65537;
  try
    not_use_16bpp := FALSE;
    for Y := 0 to Bmp.Height - 1 do begin
      L := Bmp.ScanLine[Y];
      for X := 0 to Bmp.Width - 1 do begin
        C := L^ and $FFFFFF;
        if ((C and $E0C0E0) <> C) and not not_use_16bpp then begin
          R := C and $FF;
          G := (C and $FF00) shr 8;
          B := C shr 16;
          if ((R and $E0) <> R) and (R <> $FF) or ((G and $C0) <> G) and (G <>
            $FF) or ((B and $E0) <> B) and (B <> $FF) then begin
            //Result := KOL.pf24bit;
            //Rpt( '~~~ color not found: ' + Int2Hex( C, 6 ), WHITE );
            //Exit;
            not_use_16bpp := TRUE;
          end;
        end;
        if ColorList.IndexOf(Pointer(C)) < 0 then begin
          ColorList.Add(Pointer(C));
          if ColorList.Count > 65536 then begin
            //Result := KOL.pf24bit;
            //Rpt( '~~~~~ pf24bit (break) ~~~~~ (' + IntToStr( ColorList.Count ) +
            //  ')', WHITE );
            //Exit;
            not_use_16bpp := TRUE;
            break;
          end;
          if not_use_16bpp and (ColorList.Count > 256) then

        end;
        Inc(L);
      end;
    end;
    if (ColorList.Count <= 2) {and
       ((ColorList.Count = 0) or
        (ColorList.Count > 0) and (DWORD(ColorList.Items[ 0 ]) and $FFFFFF = $FFFFFF) and
        ((ColorList.Count < 2) or
         (ColorList.Count = 2) and (DWORD( ColorList.Items[ 1 ] ) and $FFFFFF = 0) ))}
           then begin
      Result := KOL.pf1bit;
      Rpt('~~~~~ pf1bit ~~~~~', WHITE);
    end
    else if (ColorList.Count <= 16) {and ColorsAreSystem16( ColorList )} then begin
      Result := KOL.pf4bit;
      Rpt('~~~~~ pf4bit ~~~~~', WHITE);
    end
    else if (ColorList.Count <= 256) {and ColorsAreSystem256( ColorList )} then begin
      Result := KOL.pf8bit;
      Rpt('~~~~~ pf8bit ~~~~~', WHITE);
    end
    else if (ColorList.Count <= 65536) and not not_use_16bpp and ColorsAre64K(ColorList) then begin
      Result := KOL.pf16bit;
      Rpt('~~~~~ pf16bit ~~~~~', WHITE);
    end
    else begin
      Result := KOL.pf24bit;
      Rpt('~~~~~ pf24bit ~~~~~ (' + IntToStr(ColorList.Count) + ')', WHITE);
    end;
  finally
    Rpt('------ Colors in bitmap: ' + IntToStr(ColorList.Count), YELLOW);
    //ColorList.Free;
  end;
end;

procedure OptimizeKOLBitmapBeforeRLEEncoding(B: KOL.PBitmap);
var
  ColorCounts: array[0..255] of Integer;
  x, y, N, i, M: Integer;
  Src: PByte;
  C1, C2: TColor;
  Tmp: KOL.PBitmap;
begin
  FillChar(ColorCounts, Sizeof(ColorCounts), 0);
  N := 0;
  for y := 0 to B.Height - 1 do begin
    Src := B.ScanLine[y];
    if B.PixelFormat = KOL.pf4bit then begin
      x := B.Width;
      while x > 0 do begin
        inc(ColorCounts[Src^ shr 4]);
        if x > 1 then
          inc(ColorCounts[Src^ and 15]);
        dec(x, 2);
        inc(Src);
      end;
      N := 16;
    end
    else begin
      for x := B.Width downto 1 do begin
        inc(ColorCounts[Src^]);
        inc(Src);
      end;
      N := 256;
    end;
  end;
  M := 0;
  for i := 0 to N - 1 do begin
    if ColorCounts[i] > ColorCounts[M] then
      M := i;
  end;
  if M > 0 then begin
    C1 := B.DIBPalEntries[0];
    C2 := B.DIBPalEntries[M];
    Tmp := NewBitmap(0, 0);
    try
      Tmp.Assign(B);
      B.DIBPalEntries[0] := C2;
      B.DIBPalEntries[M] := C1;
      Tmp.Draw(B.Canvas.Handle, 0, 0);
    finally
      Tmp.Free;
    end;
  end;
end;

// This version of GenerateBitmapResource provided by Alex Pravdin.
// It does not use brcc32.exe, and creates res-file directly, so
// it is fast and has no restrictions on bitmap format at all.

procedure GenerateBitmapResource(Bitmap: TBitmap; const RsrcName, FileName:
  string; var Updated: Boolean; AllowCompression: Boolean);
var
  HD1: packed record // First part of RESOURCEHEADER structure before
                       // Unicode string contained bitmap resource name
    DataSize: cardinal;
    HeaderSize: cardinal;
    NFFFF: word;
    DataType: word;
  end;
  HD2: packed record // Second part of RESOURCEHEADER
    DataVersion: cardinal;
    MemFlags: word;
    PrimaryLang: byte;
    SubLang: byte;
    Version: cardinal;
    Charact: cardinal;
  end;
  br, hFR, hFtm, DIBLen, WLen, RLen, tm: DWORD;
  Buf1, Buf2: PByteArray;
  FE: boolean;
  Res: string;
  Bmp: string;
  tmStr: WideString;
  KOLBmp: KOL.PBitmap;
  KOLPF: KOL.TPixelFormat;
  ColorList: KOL.PList;
  N, i: Integer;
  Mem, MemRLE: KOL.PStream;
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'GenerateBitmapResource', 0

@@e_signature:
  end;
  Res := ProjectSourcePath + FileName + '.res';
  Bmp := ProjectSourcePath + FileName + '.bmp';
  FE := FileExists(Res);

  Rpt('Generating resource ' + RsrcName, YELLOW);
  //Bitmap.SaveToFile( Bmp );
  KOLBmp := KOL.NewDIBBitmap(Bitmap.Width, Bitmap.Height, KOL.pf32bit);
  BitBlt(KOLBmp.Canvas.Handle, 0, 0, Bitmap.Width, Bitmap.Height, Bitmap.Canvas.Handle, 0, 0, SRCCOPY);
  KOLBmp.HandleType := KOL.bmDIB;
  KOLBmp.PixelFormat := KOL.pf32bit;
  ColorList := NewList;
  try
    KOLPF := CountSystemColorsUsedInBitmap(KOLBmp, ColorList);
    if ColorList.Count > 0 then begin
      KOLBmp.PixelFormat := KOLPF;
      KOLBmp.HandleType := KOL.bmDIB;
      N := 0;
      case KOLPF of
        KOL.pf1bit:
          N := 2;
        KOL.pf4bit:
          N := 16;
        KOL.pf8bit:
          N := 256;
      end;
      if N > 0 then begin
        for i := 0 to min(ColorList.Count, N) - 1 do begin
          KOLBmp.DIBPalEntries[i] := Integer(ColorList.Items[i]);
        end;
              //
        BitBlt(KOLBmp.Canvas.Handle, 0, 0, Bitmap.Width, Bitmap.Height, Bitmap.Canvas.Handle, 0, 0, SRCCOPY);
              //
      end;
          //KOLBmp.SaveToFile( Bmp );
      Mem := NewMemoryStream;
      MemRLE := NewMemoryStream;
      try
        if AllowCompression then
          KOLBmp.CoreSaveToStream(Mem)
        else
          KOLBmp.SaveToStream(Mem);
        if (N > 0) and AllowCompression then begin
          if KOLPF = KOL.pf1bit then
            KOLBmp.PixelFormat := KOL.pf4bit;
          OptimizeKOLBitmapBeforeRLEEncoding(KOLBmp);
          KOLBmp.RLESaveToStream(MemRLE);
        end;
        if (MemRLE.Size > 0) and (MemRLE.Size < Mem.Size) then
          KOL.Swap(PtrInt(Mem), PtrInt(MemRLE));
        Mem.Position := 0;
        Mem.SaveToFile(Bmp, 0, Mem.Size);
      finally
        Mem.Free;
        MemRLE.Free;
      end;
    end
    else begin
      Bitmap.SaveToFile(Bmp);
    end;
    Rpt('Bitmap saved to ' + Bmp, YELLOW);
    KOLBmp.Free;
  finally
    ColorList.Free;
  end;

  if FE then begin
    DeleteFile(PChar(Res + '_tmp'));
    CopyFile(PChar(Res), PChar((Res + '_tmp')), False);
  end;

  hFR := CreateFile(PChar(Res), GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ,
    nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);

  if hFR = INVALID_HANDLE_VALUE then begin
    Rpt('Can not create file ' + Res + #13#10'Error: ' + SysErrorMessage(GetLastError), RED);
    Exit;
  end;

  hFtm := CreateFile(PChar(Bmp), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);

  DIBLen := GetFileSize(hFtm, nil) - 14{SizeOf( TBITMAPFILEHEADER )};
  WLen := (Length(RsrcName) + 1) * 2;

  HD1.DataSize := DIBLen;
  HD1.HeaderSize := 12{SizeOf( HD1 )}    + 16{SizeOf( HD2 )}    + WLen;
  HD1.NFFFF := $FFFF;
  HD1.DataType := 2; // RT_BITMAP
  HD2.DataVersion := 0;
  HD2.MemFlags := 0;
  HD2.PrimaryLang := LANG_NEUTRAL;
  HD2.SubLang := SUBLANG_DEFAULT;
  HD2.Version := 0;
  HD2.Charact := 0;

  RLen := HD1.HeaderSize + DIBLen + 32;
  GetMem(Buf1, RLen);
  FillChar(Buf1[0], RLen, 0);

  Buf1[4] := $20;
  Buf1[8] := $FF;
  Buf1[9] := $FF;
  Buf1[12] := $FF;
  Buf1[13] := $FF;

  tmStr := UpperCase(RsrcName) + #0;
  CopyMemory(@Buf1[32], @HD1, 12);
  CopyMemory(@Buf1[32 + 12], @tmStr[1], WLen);
  CopyMemory(@Buf1[32 + 12 + WLen], @HD2, 16);

  SetFilePointer(hFtm, 14{SizeOf( TBITMAPFILEHEADER )}, nil, FILE_BEGIN);
  ReadFile(hFtm, Buf1[32 + 12 + 16 + WLen], DIBLen, br, nil);

  WriteFile(hFR, Buf1[0], RLen, br, nil);
  CloseHandle(hFtm);
  CloseHandle(hFR);
  //------------------------------------------------
  DeleteFile(Bmp);

  if FE then begin
    hFtm := CreateFile(PChar((Res + '_tmp')), GENERIC_READ, FILE_SHARE_READ, nil,
      OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    tm := GetFileSize(hFtm, nil);
    GetMem(Buf2, tm);
    ReadFile(hFtm, Buf2[0], tm, br, nil);
    CloseHandle(hFtm);
    DeleteFile(Res + '_tmp');
    if (RLen <> tm) or (not CompareMem(@Buf1[0], @Buf2[0], Min(RLen, tm))) then begin
      Rpt('Resource ' + Res + ' changed.', WHITE);
      Updated := True;
    end;
    FreeMem(Buf2);
  end;
  FreeMem(Buf1);

end;

function SaveIcon(Icon: TIcon; const Path: string): Boolean;
var
  MS, MS2: TMemoryStream;
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'SaveIcon', 0

@@e_signature:
  end;
  Result := TRUE;
  MS := TMemoryStream.Create;
  MS2 := TMemoryStream.Create;
  try
    Icon.SaveToStream(MS);
    if FileExists(Path) then begin
      MS2.LoadFromFile(Path);
      if (MS.Size = MS2.Size) and CompareMem(MS.Memory, MS2.Memory, MS.Size) then
        Exit;
      if FileExists(Path + '.$$$') then
        DeleteFile(Path + '.$$$');
      MoveFile(PChar(Path), PChar(Path + '.$$$'));
    end;
    MS.Position := 0;
    MS.SaveToFile(Path);
    //Result := True;
    //Rpt( 'Icon stored to ' + Path );
  finally
    MS.Free;
    MS2.Free;
  end;
end;

procedure GenerateIconResource(Icon: TIcon; const RsrcName, FileName: KOLString; var Updated: Boolean);
var
  RL: TStringList;
  Buf1, Buf2: PKOLChar;
  S: string;
  I, J: Integer;
  F: THandle;
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'GenerateIconResource', 0

@@e_signature:
  end;
  {if not SaveIcon( Icon, ProjectSourcePath + FileName + '.ico' )
     and FileExists( ProjectSourcePath + FileName + '.res' ) then Exit;}
  if not SaveIcon(Icon, ProjectSourcePath + FileName + '.ico') then
    Exit;
  RL := TStringList.Create;
  RL.Add(KOLUpperCase(RsrcName) + ' ICON "' + FileName + '.ico"');
  RL.SaveToFile(ProjectSourcePath + FileName + '.rc');
  RL.Free;
  Buf1 := nil;
  Buf2 := nil;
  I := 0;
  J := 0;
  S := ProjectSourcePath + FileName + '.res';
  if FileExists(S) then begin
    I := FileSize(S);
    if I > 0 then begin
      GetMem(Buf1, I);
      F := KOL.FileCreate(S, ofOpenRead or ofShareDenyWrite or ofOpenExisting);
      if F <> THandle(-1) then begin
        KOL.FileRead(F, Buf1^, I);
        KOL.FileClose(F);
      end;
    end;
  end;
  {ShellExecute( 0, 'open', PChar( ExtractFilePath( Application.ExeName ) + 'brcc32.exe' ),
                PChar( ProjectSourcePath + FileName + '.rc' ), PChar( ProjectSourcePath ),
                SW_HIDE );}
  ExecuteWait(ExtractFilePath(Application.ExeName) + 'brcc32.exe', '"' +
    ProjectSourcePath + FileName + '.rc"', ProjectSourcePath, SW_HIDE, INFINITE, nil);
  if FileExists(S) then begin
    J := FileSize(S);
    if J > 0 then begin
      GetMem(Buf2, J);
      F := KOL.FileCreate(S, ofOpenRead or ofShareDenyWrite or ofOpenExisting);
      if F <> THandle(-1) then begin
        KOL.FileRead(F, Buf2^, J);
        KOL.FileClose(F);
      end;
    end;
  end;
  if (Buf1 = nil) or (I <> J) or (Buf2 <> nil) and not CompareMem(Buf1, Buf2, J) then begin
    Updated := TRUE;
  end;
  if Buf1 <> nil then
    FreeMem(Buf1);
  if Buf2 <> nil then
    FreeMem(Buf2);
end;

{ TKOLTimer }

procedure TKOLTimer.AssignEvents(SL: TStringList; const AName: string);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLTimer.AssignEvents', 0

@@e_signature:
  end;
  inherited;
  DoAssignEvents(SL, AName, ['OnTimer'], [@OnTimer]);
end;

constructor TKOLTimer.Create(AOwner: TComponent);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLTimer.Create', 0

@@e_signature:
  end;
  inherited;
  fInterval := 1000;
  fEnabled := True;
  FPeriodic := TRUE;
  FResolution := 0;
end;

procedure TKOLTimer.SetEnabled(const Value: Boolean);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLTimer.SetEnabled', 0

@@e_signature:
  end;
  FEnabled := Value;
  Change;
end;

procedure TKOLTimer.SetInterval(const Value: Integer);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLTimer.SetInterval', 0

@@e_signature:
  end;
  FInterval := Value;
  Change;
end;

procedure TKOLTimer.SetMultimedia(const Value: Boolean);
begin
  FMultimedia := Value;
  Change;
end;

procedure TKOLTimer.SetOnTimer(const Value: TOnEvent);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLTimer.SetOnTimer', 0

@@e_signature:
  end;
  FOnTimer := Value;
  Change;
end;

procedure TKOLTimer.SetPeriodic(const Value: Boolean);
begin
  FPeriodic := Value;
  Change;
end;

procedure TKOLTimer.SetResolution(const Value: Integer);
begin
  FResolution := Value;
  Change;
end;

procedure TKOLTimer.SetupFirst(SL: TStringList; const AName, AParent, Prefix: string);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLTimer.SetupFirst', 0

@@e_signature:
  end;
  if Multimedia then begin
    SL.Add(Prefix + AName + ' := NewMMTimer( ' + IntToStr(Interval) + ' );');
    if not Periodic then
      SL.Add(Prefix + 'PMMTimer(' + AName + ').Periodic := FALSE;');
    if Resolution > 0 then
      SL.Add(Prefix + 'PMMTimer(' + AName + ').Resolution := ' + IntToStr(Resolution) + ';');
  end
  else
    SL.Add(Prefix + AName + ' := NewTimer( ' + IntToStr(Interval) + ' );');

  //AssignEvents( SL, AName );
  GenerateTag(SL, AName, Prefix);
end;

procedure TKOLTimer.SetupLast(SL: TStringList; const AName, AParent, Prefix: string);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLTimer.SetupLast', 0

@@e_signature:
  end;
  if Enabled then
    SL.Add(Prefix + AName + '.Enabled := True;');
end;

function TKOLTimer.TypeName: string;
begin
  if Multimedia then
    Result := 'MMTimer'
  else
    Result := inherited TypeName;
end;

{ TKOLImageList }

procedure TKOLImageList.Assign(Value: TPersistent);
var
  IL: TKOLImageList;
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLImageList.Assign', 0

@@e_signature:
  end;
  if (Value <> nil) and (Value is TKOLImageList) then begin
    IL := Value as TKOLImageList;
    FImgWidth := IL.ImgWidth;
    FImgHeight := IL.ImgHeight;
    FCount := IL.Count;
    FBitmap.Assign(IL.Bitmap);
    FSystemImageList := IL.SystemImageList;
    FTransparentColor := IL.TransparentColor;
  end
  else
    inherited;
  Change;
end;

procedure TKOLImageList.AssignBitmapToKOLImgList;
var
  R: Integer;
  TmpBmp: TBitmap;
begin
  if FKOLImgList = nil then
    Exit;
  if Bitmap <> nil then begin
    //Bitmap.SaveToFile( 'c:\test1.bmp' );
    //ShowMessage( 'Bitmap.Handle=' + IntToStr( Bitmap.Handle ) );
    FKOLImgList.Clear;
    FKOLImgList.Colors := Colors;
    //FKOLImgList.BkColor := Color2RGB( BkColor );
    FKOLImgList.ImgWidth := ImgWidth;
    FKOLImgList.ImgHeight := ImgHeight;
    {Bitmap.HandleType := bmDIB;
    Bitmap.PixelFormat := pf24bit;}
    //ShowMessage( Int2Hex( Color2RGB( BkColor ), 8 ) );
    if not Bitmap.Empty then begin
      //Bitmap.SaveToFile( 'c:\test2.bmp' );
      TmpBmp := TBitmap.Create;
      try
        TmpBmp.Assign(Bitmap);
        if Masked then
          R := FKOLImgList.AddMasked(TmpBmp.Handle, Color2RGB(TransparentColor))
        else begin
          FKOLImgList.Masked := FALSE;
          R := FKOLImgList.Add(TmpBmp.Handle, 0);
        end;
        if R < 0 then
          ShowMessage('Error adding bitmap: ' + SysErrorMessage(GetLastError))
        else begin
          DoNotifyLinkedComponents(noChanged);
        end;
      finally
        TmpBmp.Free;
      end;
      //Bitmap.SaveToFile( 'c:\test3.bmp' );
      //ShowMessage( 'Result := ' + IntToStr( R ) );
      //ShowMessage( 'FKOLImgList.Handle=' + IntToStr( FKOLImgList.Handle ) );
    end;
  end;
end;

{procedure TKOLImageList.BitmapChanged(Sender: TObject);
begin
  AssignBitmapToKOLImgList;
end;}

procedure TKOLImageList.Clear;
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLImageList.Clear', 0

@@e_signature:
  end;
  if FBitmap <> nil then begin
    FBitmap.Width := 0;
    FBitmap.Height := 0;
  end;
  FCount := 0;
end;

constructor TKOLImageList.Create(AOwner: TComponent);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLImageList.Create', 0

@@e_signature:
  end;
  inherited Create(AOwner);
  FBkColor := clNone;
  FBitmap := TBitmap.Create;
  //FBitmap.OnChange := BitmapChanged;
  FImgWidth := 32;
  FImgHeight := 32;
  FTransparentColor := clDefault;
  FMasked := TRUE;
  NeedFree := False; // ImageList in KOL destroyes self when its parent
                     // control is destroyed - automatically.
  fCreationPriority := 10;
  FAllowCompression := TRUE;
end;

destructor TKOLImageList.Destroy;
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLImageList.Destroy', 0

@@e_signature:
  end;
  FKOLImgList.Free;
  FBitmap.Free;
  inherited;
end;

function TKOLImageList.GetBitmap: TBitmap;
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLImageList.GetBitmap', 0

@@e_signature:
  end;
  if SystemImageList then
    Result := nil
  else
    Result := FBitmap;
end;

function TKOLImageList.GetImageListHandle: THandle;
begin
  if FKOLImgList = nil then begin
    FKOLImgList := NewImageList(nil);
    AssignBitmapToKOLImgList;
  end;
  Result := FKOLImgList.Handle;
end;

function TKOLImageList.GetTransparentColor: TColor;
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLImageList.GetTransparentColor', 0

@@e_signature:
  end;
  Result := FTransparentColor;
  if Result = clDefault then
    if FBitmap <> nil then
      if (FBitmap.Width <> 0) and (FBitmap.Height <> 0) then
        Result := FBitmap.Canvas.Pixels[0, FBitmap.Height - 1];
end;

procedure TKOLImageList.SetAllowCompression(const Value: Boolean);
begin
  if FAllowCompression = Value then
    Exit;
  FAllowCompression := Value;
  Change;
end;

procedure TKOLImageList.SetBitmap(const Value: TBitmap);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLImageList.SetBitmap', 0

@@e_signature:
  end;
  if FBitmap = Value then
    Exit;
  FBitmap.Assign(Value);
  if (FBitmap.Height <> 0) and (FBitmap.Width <> 0) then begin
    FImgHeight := FBitmap.Height;
    {AK->}            if FImgWidth <= 0 then{<-AK}
      FImgWidth := FImgHeight;
    FCount := FBitmap.Width div FImgWidth;
  end;
  if FBitmap.HandleType = bmDDB then
    Colors := ilcColorDDB
  else begin
    //if  Colors = ilcDefault then
    case FBitmap.PixelFormat of
      pf1bit, pf4bit:
        if Colors < ilcColor4 then
          Colors := ilcColor4;
      pf8bit:
        if Colors < ilcColor8 then
          Colors := ilcColor8;
      pf15bit, pf16bit:
        if Colors < ilcColor16 then
          Colors := ilcColor16;
      pf32bit:
        if Colors < ilcColor32 then
          Colors := ilcColor32;
    //pf24bit:
    else
      if Colors < ilcColor24 then
        Colors := ilcColor24;
    end;
  end;
  if (FBitmap.Height <> 0) and (FBitmap.Width <> 0) then begin
    TransparentColor := FBitmap.Canvas.Pixels[0, FBitmap.Height - 1];
  end;
  if FKOLImgList <> nil then
    AssignBitmapToKOLImgList;
  Change;
end;

procedure TKOLImageList.SetBkColor(const Value: TColor);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLImageList.SetBkColor', 0

@@e_signature:
  end;
  FBkColor := Value;
  AssignBitmapToKOLImgList;
  Change;
end;

procedure TKOLImageList.SetColors(const Value: TImageListColors);
var
  KOLBmp: KOL.PBitmap;
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLImageList.SetColors', 0

@@e_signature:
  end;
  if FColors = Value then
    Exit;
  FColors := Value;
  if FBitmap = nil then
    Exit;
  if FBitmap.Width * FBitmap.Height = 0 then
    Exit;
  KOLBmp := NewBitmap(FBitmap.Width, FBitmap.Height);
  try
    KOLBmp.HandleType := KOL.bmDIB;
    KOLBmp.PixelFormat := KOL.pf32bit;
    BitBlt(KOLBmp.Canvas.Handle, 0, 0, FBitmap.Width, FBitmap.Height, FBitmap.Canvas.Handle, 0, 0, SrcCopy);
    case Value of
      ilcColor4:
        KOLBmp.PixelFormat := KOL.pf4bit;
      ilcColor8:
        KOLBmp.PixelFormat := KOL.pf8bit;
      ilcColor24:
        KOLBmp.PixelFormat := KOL.pf24bit;
      ilcColor32:
        KOLBmp.PixelFormat := KOL.pf32bit;
    else
      KOLBmp.HandleType := KOL.bmDDB;
    end;
    FBitmap.Handle := KOLBmp.ReleaseHandle;
  finally
    KOLBmp.Free;
  end;
  Change;
end;

procedure TKOLImageList.SetCount(const Value: Integer);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLImageList.SetCount', 0

@@e_signature:
  end;
  FCount := Value;
  if Value > 0 then begin
    {AK->}             if FImgWidth <= 0 then {<-AK} // change by Andrzej Kubaszek 28-Jan-2002
      FImgWidth := FImgHeight;
    if FBitmap <> nil then
      if FBitmap.Width > 0 then
        FImgWidth := FBitmap.Width div FCount;
  end;
  Change;
end;

procedure TKOLImageList.SetForce32bit(const Value: Boolean);
begin
  if FForce32bit = Value then
    Exit;
  FForce32bit := Value;
  Change;
end;

procedure TKOLImageList.SetImgHeight(Value: Integer);
var
  I: Integer;
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLImageList.SetImgHeight', 0

@@e_signature:
  end;
  if Value < 0 then
    Value := 0;
  if SystemImageList then
    if Value >= 32 then
      Value := 32
    else
      Value := 16
  else if FBitmap <> nil then begin
    if not FBitmap.Empty then
      if (FBitmap.Height <> 0) and (FBitmap.Width <> 0) then
        if Value <> FBitmap.Height then
          Value := FBitmap.Height;
  end;

  if FImgHeight = Value then
    Exit;
  if Count > 0 then
    if not (csLoading in ComponentState) then begin
      I := MessageBox(0,
        'Changing image list height will lead to clearing it. Are ' + 'You sure You want to change height now?',
        'TKOLImageList.ImgHeight change', MB_YESNO or MB_DEFBUTTON2 or MB_SETFOREGROUND);
      if I = ID_NO then
        Exit;
      Clear;
    end;
  FImgHeight := Value;
  if SystemImageList then
    FImgWidth := FImgHeight;
  Change;
end;

procedure TKOLImageList.SetImgWidth(Value: Integer);
var
  I: Integer;
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLImageList.SetImgWidth', 0

@@e_signature:
  end;
  if Value < 0 then
    Value := 0;
  if SystemImageList then begin
    if Value >= 32 then
      Value := 32
    else
      Value := 16;
  end
  else if FBitmap <> nil then begin
    if not FBitmap.Empty then
      if (FBitmap.Width <> 0) and (FBitmap.Height <> 0) then
        if Value > FBitmap.Width then
          Value := FBitmap.Width;
  end;

  if FImgWidth = Value then
    Exit;
  if Count > 0 then
    if not (csLoading in ComponentState) then begin
      I := MessageBox(0,
        'Changing image list width will lead to clearing it. Are ' + 'You sure You want to change width now?',
        'TKOLImageList.ImgWidth change', MB_YESNO or MB_DEFBUTTON2 or MB_SETFOREGROUND);
      if I = ID_NO then
        Exit;
      Clear;
    end;
  FImgWidth := Value;
  if SystemImageList then
    FImgHeight := FImgWidth;
  Change;
end;

procedure TKOLImageList.SetMasked(const Value: Boolean);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLImageList.SetMasked', 0

@@e_signature:
  end;
  FMasked := Value;
  Change;
end;

procedure TKOLImageList.SetSystemImageList(const Value: Boolean);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLImageList.SetSystemImageList', 0

@@e_signature:
  end;
  if Value = FSystemImageList then
    Exit;
  FSystemImageList := Value;
  if Value then begin
    Clear;
    SetImgHeight(ImgHeight);
    SetImgWidth(ImgHeight);
  end
  else
    Clear;
  Change;
end;

procedure TKOLImageList.SetTransparentColor(const Value: TColor);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLImageList.SetTransparentColor', 0

@@e_signature:
  end;
  FTransparentColor := Value;
  AssignBitmapToKOLImgList;
  Change;
end;

procedure TKOLImageList.SetupFirst(SL: TStringList; const AName, AParent, Prefix: string);
const
  Booleans: array[Boolean] of string = ('False', 'True');
const
  ColorsValues: array[TImageListColors] of string = ('ilcColor', 'ilcColor4',
    'ilcColor8', 'ilcColor16', 'ilcColor24', 'ilcColor32', 'ilcColorDDB', 'ilcDefault');
var
  RsrcName, RsrcFile, is32: string;
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLImageList.SetupFirst', 0

@@e_signature:
  end;
  SL.Add(Prefix + AName + ' := NewImageList( ' + AParent + ' );');

  GenerateTag(SL, AName, Prefix);
  if SystemImageList then
    SL.Add(Prefix + AName + '.LoadSystemIcons( ' + Booleans[ImgHeight = 16] + ' );')
  else begin
    if Colors <> ilcDefault then
      SL.Add(Prefix + AName + '.Colors := ' + ColorsValues[Colors] + ';');
    if not Masked then begin
      SL.Add(Prefix + AName + '.Masked := FALSE;');
      if BkColor <> clNone then
        SL.Add(Prefix + AName + '.BkColor := ' + Color2Str(BkColor) + ';');
    end;
    if FImgWidth <> 32 then
      SL.Add(Prefix + '  ' + AName + '.ImgWidth := ' + IntToStr(FImgWidth) + ';');
    if FImgHeight <> 32 then
      SL.Add(Prefix + '  ' + AName + '.ImgHeight := ' + IntToStr(FImgHeight) + ';');
  end;
  is32 := '';
  if Force32bit then
    is32 := '32';
  if (FBitmap.Width <> 0) and (FBitmap.Height <> 0) then begin
    if (FImgHeight = 32) and (FImgWidth <> FImgHeight) then
      SL.Add(Prefix + '  ' + AName + '.ImgWidth := ' + IntToStr(FImgWidth) + ';');
    RsrcName := UpperCase(ParentKOLForm.FormName + '_' + Name);
    RsrcFile := ParentKOLForm.FormName + '_' + Name;
    SL.Add(Prefix + '  {$R ' + RsrcFile + '.res}');
    if Masked then
      SL.Add(Prefix + AName + '.AddMasked( LoadBmp' + is32 + '( hInstance, ''' +
        RsrcName + ''', ' + AName + ' ), ' + Color2Str(TransparentColor) + ' );')
    else
      SL.Add(Prefix + AName + '.Add( LoadBmp' + is32 + '( hInstance, ''' +
        RsrcName + ''', ' + AName + ' ), 0 );');
    //Rpt( 'Generating resource: ' + ProjectSourcePath + RsrcFile + '.res' );
    GenerateBitmapResource(FBitmap, RsrcName, RsrcFile, fUpdated, AllowCompression);
  end;
end;

{ TKOLImageListEditor }

procedure TKOLImageListEditor.Edit;
var
  IL: TImageList; //Invisible;
    {$IFDEF _D6orHigher}
  ILCE: IComponentEditor;
    {$ELSE}
  ILCE: TComponentEditor;
    {$ENDIF}
  ILH: THandle;
  KIL: TKOLImageList;
  KName: string;
  I: Integer;
    //TrColor: TColor;
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLImageListEditor.Edit', 0

@@e_signature:
  end;
  if Component = nil then
    Exit;
  if not (Component is TKOLImageList) then
    Exit;
  KIL := Component as TKOLImageList;
  if KIL.SystemImageList then begin
    ShowMessage('It is not possible to edit system image list!');
    Exit;
  end;
  IL := TImageList{Invisible}.Create( {KIL.ParentForm} KIL);
  KName := KIL.Name;
  IL.Name := KName + '_edit';
  {IL.Width := KIL.ImgWidth;
  IL.Height := KIL.ImgHeight;}
  try

    case KIL.Colors of
      ilcColor4:
        I := ILC_COLOR4;
      ilcColor8:
        I := ILC_COLOR8;
      ilcColor16:
        I := ILC_COLOR16;
      ilcColor24:
        I := ILC_COLOR24;
      ilcColor32:
        I := ILC_COLOR32;
    else
      I := ILC_COLOR;
    end;

    if KIL.TransparentColor = clNone then
      ILH := ImageList_Create(KIL.ImgWidth, KIL.ImgHeight, I, KIL.Count, 1)
    else
      ILH := ImageList_Create(KIL.ImgWidth, KIL.ImgHeight, I or ILC_MASK, KIL.Count, 1);
    if ILH <> 0 then begin
      if KIL.Masked then
        ImageList_AddMasked(ILH, KIL.Bitmap.Handle, Color2RGB(KIL.TransparentColor))
      else
        ImageList_Add(ILH, KIL.Bitmap.Handle, 0);
      {
      if KIL.TransparentColor = clNone then
        ImageList_Add( ILH, KIL.Bitmap.Handle, 0 )
      else
      begin
        TrColor := KIL.TransparentColor;
        Tmp := TBitmap.Create;
        Tmp.Assign( KIL.Bitmap );
        Tmp.Mask( TrColor );
        try
          //TrColor := KIL.Bitmap.TransparentColor;
          //TrColor := KIL.Bitmap.Canvas.Pixels[ 0, KIL.Bitmap.Height - 1 ];
          //ShowMessage( '»ÒÔÓÎ¸ÁÛÂÏ ÔÓÁ‡˜Ì˚È ˆ‚ÂÚ: ' + Int2Hex( Color2RGB( TrColor ), 8 ) );
          //ImageList_AddMasked( ILH, KIL.Bitmap.Handle, Color2RGB( TrColor ) );
          ImageList_Add( ILH, KIL.Bitmap.Handle, Tmp.Handle );
        finally
          Tmp.Free;
        end;
      end;
      }
      IL.Handle := ILH;
      IL.ShareImages := False;
      //Rpt( 'Attempt to get component editor' );
      ILCE := GetComponentEditor(IL, Designer);
      if ILCE <> nil then
      try
        //Rpt( 'ILCE obtained, try to call editor' );
        ILCE.Edit;

        Rpt('Image list ' + KIL.Name + ' edited.', WHITE);
        if KIL.Bitmap.Empty then begin
          KIL.Bitmap := TBitmap.Create;
          //KIL.Bitmap.PixelFormat := pf24bit;
          Rpt('Bitmap was empty - created.', WHITE);
        end;
        KIL.Bitmap.Height := IL.Height;
        KIL.Bitmap.Width := IL.Width * IL.Count;
        KIL.Bitmap.Canvas.Brush.Color := KIL.TransparentColor;
        KIL.Bitmap.Canvas.FillRect(Rect(0, 0, KIL.Bitmap.Width, KIL.Bitmap.Height));
        for I := 0 to IL.Count - 1 do
          IL.Draw(KIL.Bitmap.Canvas, I * IL.Width, 0, I);

        KIL.FCount := IL.Count;
        KIL.AssignBitmapToKOLImgList;
        KIL.Change;
      finally
        {$IFNDEF _D6orHigher}
        ILCE.Free;
        {$ENDIF}
      end;
    end;
  finally
    IL.Free;
  end;
end;

procedure TKOLImageListEditor.ExecuteVerb(Index: Integer);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLImageListEditor.ExecuteVerb', 0

@@e_signature:
  end;
  Edit;
end;

function TKOLImageListEditor.GetVerb(Index: Integer): string;
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLImageListEditor.GetVerb', 0

@@e_signature:
  end;
  Result := '&Editor';
end;

function TKOLImageListEditor.GetVerbCount: Integer;
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLImageListEditor.GetVerbCount', 0

@@e_signature:
  end;
  Result := 1;
end;

{ TKOLOpenSaveDialog }

constructor TKOLOpenSaveDialog.Create(AOwner: TComponent);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLOpenSaveDialog.Create', 0

@@e_signature:
  end;
  inherited;
  Options := DefOpenSaveDlgOptions;
  OpenDialog := TRUE;
end;

procedure TKOLOpenSaveDialog.SetDefExtension(const Value: string);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLOpenSaveDialog.SetDefExtension', 0

@@e_signature:
  end;
  FDefExtension := Value;
  Change;
end;

procedure TKOLOpenSaveDialog.SetFilter(const Value: string);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLOpenSaveDialog.SetFilter', 0

@@e_signature:
  end;
  FFilter := Value;
  Change;
end;

procedure TKOLOpenSaveDialog.SetFilterIndex(const Value: Integer);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLOpenSaveDialog.SetFilterIndex', 0

@@e_signature:
  end;
  FFilterIndex := Value;
  if FFilterIndex < 0 then
    FFilterIndex := 0;
  Change;
end;

procedure TKOLOpenSaveDialog.SetInitialDir(const Value: string);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLOpenSaveDialog.SetInitialDir', 0

@@e_signature:
  end;
  FInitialDir := Value;
  Change;
end;

procedure TKOLOpenSaveDialog.SetNoPlaceBar(const Value: Boolean);
begin
  FNoPlaceBar := Value;
  Change;
end;

procedure TKOLOpenSaveDialog.SetOpenDialog(const Value: Boolean);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLOpenSaveDialog.SetOpenDialog', 0

@@e_signature:
  end;
  FOpenDialog := Value;
  Change;
end;

procedure TKOLOpenSaveDialog.SetOptions(const Value: TOpenSaveOptions);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLOpenSaveDialog.SetOptions', 0

@@e_signature:
  end;
  FOptions := Value;
  Change;
end;

procedure TKOLOpenSaveDialog.SetTemplateName(const Value: string);
begin
  FTemplateName := Value;
  Change;
end;

procedure TKOLOpenSaveDialog.SetTitle(const Value: string);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLOpenSaveDialog.SetTitle', 0

@@e_signature:
  end;
  FTitle := Value;
  Change;
end;

procedure TKOLOpenSaveDialog.SetupFirst(SL: TStringList; const AName, AParent, Prefix: string);
var
{$IFDEF _D2009orHigher}
  C, C2: WideString;
  i: integer;
{$ELSE}
  C: string;
{$ENDIF}
  S: string;
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLOpenSaveDialog.SetupFirst', 0

@@e_signature:
  end;
  S := '';
  if Options <> DefOpenSaveDlgOptions then begin
    if OSCreatePrompt in Options then
      S := 'OSCreatePrompt';
    if OSExtensionDiffent in Options then
      S := S + ', OSExtensionDiffent';
    if OSFileMustExist in Options then
      S := S + ', OSFileMustExist';
    if OSHideReadonly in Options then
      S := S + ', OSHideReadonly';
    if OSNoChangedir in Options then
      S := S + ', OSNoChangedir';
    if OSNoReferenceLinks in Options then
      S := S + ', OSNoReferenceLinks';
    if OSAllowMultiSelect in Options then
      S := S + ', OSAllowMultiSelect';
    if OSNoNetworkButton in Options then
      S := S + ', OSNoNetworkButton';
    if OSNoReadonlyReturn in Options then
      S := S + ', OSNoReadonlyReturn';
    if OSOverwritePrompt in Options then
      S := S + ', OSOverwritePrompt';
    if OSPathMustExist in Options then
      S := S + ', OSPathMustExist';
    if OSReadonly in Options then
      S := S + ', OSReadonly';
    if OSNoValidate in Options then
      S := S + ', OSNoValidate';
    if OSTemplate in Options then
      S := S + ', OSTemplate';
    if OSHook in Options then
      S := S + ', OSHook';
    if S <> '' then
      if S[1] = ',' then
        S := Trim(Copy(S, 2, MaxInt));
  end;

  if (ParentKOLForm <> nil) and ParentKOLForm.AssignTextToControls then
    C := StringConstant('Title', Title)
  else
    C := '''''';
  {$IFDEF _D2009orHigher}
  C2 := '';
  for i := 2 to Length(C) - 1 do
    C2 := C2 + '#' + IntToStr(ord(C[i]));
  C := C2;
  {$ENDIF}
  if C = '' then
    C := '''''';
  SL.Add(Prefix + AName + ' := NewOpenSaveDialog( ' + C + ', ' + StringConstant('InitialDir',
    InitialDir) + ', [ ' + S + ' ] );');

  GenerateTag(SL, AName, Prefix);
  if (Filter <> '') and (ParentKOLForm <> nil) and ParentKOLForm.AssignTextToControls then
    SL.Add(Prefix + '  ' + AName + '.Filter := ' + StringConstant('Filter', Filter) + ';');
  if not OpenDialog then
    SL.Add(Prefix + '  ' + AName + '.OpenDialog := FALSE;');
  if DefExtension <> '' then
    SL.Add(Prefix + '  ' + AName + '.DefExtension := ' + StringConstant('DefExtension', DefExtension) + ';');
  if TemplateName <> '' then
    SL.Add(Prefix + '  ' + AName + '.TemplateName := ' + StringConstant('TemplateName', TemplateName) + ';');
  if NoPlaceBar then begin
    SL.Add('{$IFDEF OpenSaveDialog_Extended}');
    SL.Add(Prefix + '  ' + AName + '.NoPlaceBar := TRUE;');
    SL.Add('{$ENDIF}');
  end;
end;

{ TKOLFileFilter }

procedure TKOLOpenSaveDialog.SetupLast(SL: TStringList; const AName, AParent, Prefix: string);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLOpenSaveDialog.SetupLast', 0

@@e_signature:
  end;
  SL.Add(Prefix + '  ' + AName + '.WndOwner := Result.Form.GetWindowHandle;');
end;

{ TKOLFileFilter }

procedure TKOLFileFilter.Edit;
var
  Dlg: TfmFileFilterEditor;
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLFileFilter.Edit', 0

@@e_signature:
  end;
  if GetComponent(0) = nil then
    Exit;
  Dlg := TfmFileFilterEditor.Create(Application);
  Dlg.Caption := (GetComponent(0) as TComponent).Name + '.Filter';
  Dlg.Filter := GetStrValue;
  Dlg.ShowModal;
  if Dlg.ModalResult = mrOK then begin
    SetStrValue(Dlg.Filter);
  end;
  Dlg.Free;
end;

function TKOLFileFilter.GetAttributes: TPropertyAttributes;
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLFileFilter.GetAttributes', 0

@@e_signature:
  end;
  Result := [paDialog, paReadOnly];
end;

{ TKOLOpenDirDialog }

function TKOLOpenDirDialog.AdditionalUnits: string;
begin
  Result := '';
  if AltDialog then
    Result := ', KOLDirDlgEx';
end;

procedure TKOLOpenDirDialog.AssignEvents(SL: TStringList; const AName: string);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLOpenDirDialog.AssignEvents', 0

@@e_signature:
  end;
  inherited;
  if not AltDialog then
    DoAssignEvents(SL, AName, ['OnSelChanged'], [@OnSelChanged]);
end;

constructor TKOLOpenDirDialog.Create(AOwner: TComponent);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLOpenDirDialog.Create', 0

@@e_signature:
  end;
  inherited;
  Options := [odOnlySystemDirs];
end;

procedure TKOLOpenDirDialog.SetAltDialog(const Value: Boolean);
begin
  FAltDialog := Value;
  Change;
end;

procedure TKOLOpenDirDialog.SetCenterOnScreen(const Value: Boolean);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLOpenDirDialog.SetCenterOnScreen', 0

@@e_signature:
  end;
  FCenterOnScreen := Value;
  Change;
end;

procedure TKOLOpenDirDialog.SetInitialPath(const Value: string);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLOpenDirDialog.SetInitialPath', 0

@@e_signature:
  end;
  FInitialPath := Value;
  Change;
end;

procedure TKOLOpenDirDialog.SetOnSelChanged(const Value: TOnODSelChange);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLOpenDirDialog.SetOnSelChanged', 0

@@e_signature:
  end;
  FOnSelChanged := Value;
  Change;
end;

procedure TKOLOpenDirDialog.SetOptions(const Value: TOpenDirOptions);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLOpenDirDialog.SetOptions', 0

@@e_signature:
  end;
  FOptions := Value;
  Change;
end;

procedure TKOLOpenDirDialog.SetTitle(const Value: string);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLOpenDirDialog.SetTitle', 0

@@e_signature:
  end;
  FTitle := Value;
  Change;
end;

procedure TKOLOpenDirDialog.SetupFirst(SL: TStringList; const AName, AParent, Prefix: string);
var
{$IFDEF _D2009orHigher}
  C, C2: WideString;
  i: integer;
{$ELSE}
  C: string;
{$ENDIF}
  S: string;
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLOpenDirDialog.SetupFirst', 0

@@e_signature:
  end;
  if Options <> [odOnlySystemDirs] then begin
    S := '';
    if odBrowseForComputer in Options then
      S := 'odBrowseForComputer';
    if odBrowseForPrinter in Options then
      S := S + ', odBrowseForPrinter';
    if odDontGoBelowDomain in Options then
      S := S + ', odDontGoBelowDomain';
    if odOnlyFileSystemAncestors in Options then
      S := S + ', odOnlyFileSystemAncestors';
    if odOnlySystemDirs in Options then
      S := S + ', odOnlySystemDirs';
    if odStatusText in Options then
      S := S + ', odStatusText';
    if odBrowseIncludeFiles in Options then
      S := S + ', odBrowseIncludeFiles';
    if odEditBox in Options then
      S := S + ', odEditBox';
    if odNewDialogStyle in Options then
      S := S + ', odNewDialogStyle';
    if S <> '' then
      if S[1] = ',' then
        S := Trim(Copy(S, 2, MaxInt));
  end;
  if AltDialog then begin
    SL.Add(Prefix + AName + ' := NewOpenDirDialogEx;');
    if (Title <> '') and (ParentKOLForm <> nil) and ParentKOLForm.AssignTextToControls then begin
      C := StringConstant('Title', Title);
          {$IFDEF _D2009orHigher}
      C2 := '';
      for i := 2 to Length(C) - 1 do
        C2 := C2 + '#' + IntToStr(ord(C[i]));
      C := C2;
          {$ENDIF}
      if C = '' then
        C := '''''';
      SL.Add(Prefix + AName + '.Title := ' + C + ';');
    end;
  end
  else begin
    if (ParentKOLForm <> nil) and ParentKOLForm.AssignTextToControls then
      C := StringConstant('Title', Title)
    else
      C := '''''';
      {$IFDEF _D2009orHigher}
    C2 := '';
    for i := 2 to Length(C) - 1 do
      C2 := C2 + '#' + IntToStr(ord(C[i]));
    C := C2;
      {$ENDIF}
    if C = '' then
      C := '''''';
    SL.Add(Prefix + AName + ' := NewOpenDirDialog( ' + C + ', [ ' + S + ' ] );');
  end;

  GenerateTag(SL, AName, Prefix);
  if InitialPath <> '' then
    SL.Add(Prefix + '  ' + AName + '.InitialPath := ' + StringConstant('InitialPath', InitialPath) + ';');
  if CenterOnScreen and not AltDialog then
    SL.Add(Prefix + '  ' + AName + '.CenterOnScreen := TRUE;');
  //AssignEvents( SL, AName );
end;

procedure TKOLOpenDirDialog.SetupLast(SL: TStringList; const AName, AParent, Prefix: string);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLOpenDirDialog.SetupLast', 0

@@e_signature:
  end;
  if not AltDialog then
    SL.Add(Prefix + '  ' + AName + '.WndOwner := Result.Form.GetWindowHandle;');
end;

function TKOLOpenDirDialog.TypeName: string;
begin
  Result := inherited TypeName;
  if AltDialog then
    Result := 'TOpenDirDialogEx';
end;

{ TKOLColorDialog }

constructor TKOLColorDialog.Create(AOwner: TComponent);
var
  I: Integer;
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLColorDialog.Create', 0

@@e_signature:
  end;
  inherited;
  for I := 1 to 16 do
    FCustomColors[I] := clWhite;
end;

function TKOLColorDialog.GetCustomColor(const Index: Integer): TColor;
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLColorDialog.GetCustomColor', 0

@@e_signature:
  end;
  Result := FCustomColors[Index];
end;

procedure TKOLColorDialog.SetColorCustomOption(const Value: TColorCustomOption);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLColorDialog.SetColorCustomOption', 0

@@e_signature:
  end;
  FColorCustomOption := Value;
  Change;
end;

procedure TKOLColorDialog.SetCustomColor(const Index: Integer; const Value: TColor);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLColorDialog.SetCustomColor', 0

@@e_signature:
  end;
  FCustomColors[Index] := Value;
  Change;
end;

procedure TKOLColorDialog.SetupFirst(SL: TStringList; const AName, AParent, Prefix: string);
const
  ColorDialogOptions: array[TColorCustomOption] of string = ('ccoFullOpen',
    'ccoShortOpen', 'ccoPreventFullOpen');
var
  I: Integer;
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLColorDialog.SetupFirst', 0

@@e_signature:
  end;
  SL.Add(Prefix + AName + ' := NewColorDialog( ' + ColorDialogOptions[ColorCustomOption] + ' );');

  GenerateTag(SL, AName, Prefix);
  for I := 1 to 16 do begin
    if FCustomColors[I] <> clWhite then
      SL.Add(Prefix + '  ' + AName + '.CustomColors[ ' + IntToStr(I) + ' ] := '
        + Color2Str(FCustomColors[I]) + ';');
  end;
end;

{ TKOLFontDialog }

constructor TKOLFontDialog.Create(AOwner: TComponent);
begin
  inherited;
  FFont := TKOLFont.Create(Self);
end;

destructor TKOLFontDialog.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TKOLFontDialog.SetupFirst(SL: TStringList; const AName, AParent, Prefix: string);
const
  OpenOption: array[KOL.TFontDialogOption] of string = ('fdAnsiOnly',
    'fdTrueTypeOnly', 'fdEffects', 'fdFixedPitchOnly', 'fdForceFontExist',
    'fdNoFaceSel', 'fdNoOEMFonts', 'fdNoSimulations', 'fdNoSizeSel',
    'fdNoStyleSel', 'fdNoVectorFonts', {'fdShowHelp',}
    'fdWysiwyg', 'fdLimitSize', 'fdScalableOnly', {'fdApplyButton',} 'fdInitFont');
  Device2Str: array[KOL.TFontDialogDevice] of string = ('fdBoth', 'fdScreen', 'fdPrinter');
var
  PfxName: string;
  SOpts: string;
  opt: KOL.TFontDialogOption;
begin
  PfxName := Prefix + AName;

  SL.Add('');
  SL.Add(PfxName + ' := NewFontDialog(' + AParent + ');');
  SL.Add(PfxName + '.MinFontSize := ' + Int2Str(FMinFontSize) + ';');
  SL.Add(PfxName + '.MaxFontSize := ' + Int2Str(FMaxFontSize) + ';');
  SL.Add(PfxName + '.Device := ' + Device2Str[FDevice] + ';');

  SOpts := '';
  for opt := Low(opt) to High(opt) do begin
    if (opt in FOptions) then
      SOpts := SOpts + ', ' + OpenOption[opt];
  end;
  Delete(SOpts, 1, 2);
  SL.Add(PfxName + '.Options := [' + SOpts + '];');

  FFont.GenerateCode(SL, AName, nil);
end;

procedure TKOLFontDialog.AssignEvents(SL: TStringList; const AName: string);
begin
  inherited;
  DoAssignEvents(SL, AName, ['OnHelp', 'OnApply'], [@OnHelp, @OnApply]);
end;

procedure TKOLFontDialog.SetMinFontSize(const Value: Integer);
begin
  FMinFontSize := Value;
  Change;
end;

procedure TKOLFontDialog.SetMaxFontSize(const Value: Integer);
begin
  FMaxFontSize := Value;
  Change;
end;

procedure TKOLFontDialog.SetDevice(const Value: KOL.TFontDialogDevice);
begin
  FDevice := Value;
  Change;
end;

procedure TKOLFontDialog.SetInitFont(const Value: TKOLFont{TKOLLogFont}{TLogFont});
begin
  FFont.Assign(Value);
  Change;
end;

procedure TKOLFontDialog.SetOnHelp(const Value: TOnEvent);
begin
  FOnHelp := Value;
  Change;
end;

procedure TKOLFontDialog.SetOnApply(const Value: TOnEvent);
begin
  FOnApply := Value;
  Change;
end;

procedure TKOLFontDialog.SetOptions(const Value: KOL.TFontDialogOptions);
begin
  FOptions := Value;
  Change;
end;

{ TKOLTrayIcon }

procedure TKOLTrayIcon.AssignEvents(SL: TStringList; const AName: string);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLTrayIcon.AssignEvents', 0

@@e_signature:
  end;
  inherited;
  DoAssignEvents(SL, AName, ['OnMouse'], [@OnMouse]);
end;

constructor TKOLTrayIcon.Create(AOwner: TComponent);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLTrayIcon.Create', 0

@@e_signature:
  end;
  inherited;
  FIcon := TIcon.Create;
  FActive := TRUE;
  fCreationPriority := -10;
end;

destructor TKOLTrayIcon.Destroy;
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLTrayIcon.Destroy', 0

@@e_signature:
  end;
  FIcon.Free;
  inherited;
end;

procedure TKOLTrayIcon.SetActive(const Value: Boolean);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLTrayIcon.SetActive', 0

@@e_signature:
  end;
  FActive := Value;
  Change;
end;

procedure TKOLTrayIcon.SetAutoRecreate(const Value: Boolean);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLTrayIcon.SetAutoRecreate', 0

@@e_signature:
  end;
  FAutoRecreate := Value;
  Change;
end;

procedure TKOLTrayIcon.SetIcon(const Value: TIcon);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLTrayIcon.SetIcon', 0

@@e_signature:
  end;
  if Value <> nil then
    FIcon.Assign(Value)
  else begin
    FIcon.Free;
    FIcon := TIcon.Create;
  end;
  Change;
end;

procedure TKOLTrayIcon.SetNoAutoDeactivate(const Value: Boolean);
begin
  FNoAutoDeactivate := Value;
  Change;
end;

procedure TKOLTrayIcon.SetOnMouse(const Value: TOnTrayIconMouse);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLTrayIcon.SetOnMouse', 0

@@e_signature:
  end;
  FOnMouse := Value;
  Change;
end;

procedure TKOLTrayIcon.SetTooltip(const Value: string);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLTrayIcon.SetTooltip', 0

@@e_signature:
  end;
  FTooltip := Value;
  if Length(FTooltip) > 64 then
    FTooltip := Copy(FTooltip, 1, 64); // 64 characters maximum allowed
  Change;
end;

procedure TKOLTrayIcon.SetupFirst(SL: TStringList; const AName, AParent, Prefix: string);
var
{$IFDEF _D2009orHigher}
  C, C2: WideString;
  i: integer;
{$ELSE}
  C: string;
{$ENDIF}
  RsrcName, RsrcFile: string;
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLTrayIcon.SetupFirst', 0

@@e_signature:
  end;
  if not Icon.Empty then begin
    RsrcName := UpperCase('z' + ParentKOLForm.FormName + '_' + Name);
    RsrcFile := ParentKOLForm.FormName + '_' + Name;
    GenerateIconResource(Icon, RsrcName, RsrcFile, fUpdated);
    SL.Add(Prefix + '  {$R ' + RsrcFile + '.RES}');
  end;
  if Icon.Empty or not Active then
    SL.Add(Prefix + AName + ' := NewTrayIcon( Applet, 0 );')
  else
    SL.Add(Prefix + AName + ' := NewTrayIcon( Applet, LoadIcon( hInstance, ' +
      String2Pascal(RsrcName, ' + ') + ' ) );');

  if not Active then begin
    SL.Add(Prefix + AName + '.Active := FALSE;');
    if not Icon.Empty then
      SL.Add(Prefix + AName + '.Icon := LoadIcon( hInstance, ' + String2Pascal(RsrcName, ' + ') + ' );')
  end;
  if NoAutoDeactivate then
    SL.Add(Prefix + AName + '.NoAutoDeactivate := TRUE;');
  if Tooltip <> '' then begin
    C := StringConstant('Tooltip', Tooltip);
    {$IFDEF _D2009orHigher}
    C2 := '';
    for i := 2 to Length(C) - 1 do
      C2 := C2 + '#' + IntToStr(ord(C[i]));
    C := C2;
    {$ENDIF}
    if C = '' then
      C := '''''';
    SL.Add(Prefix + AName + '.Tooltip := ' + C + ';');
  end;
  if AutoRecreate then
    SL.Add(Prefix + AName + '.AutoRecreate := TRUE;');
  GenerateTag(SL, AName, Prefix);
end;

procedure TKOLTrayIcon.SetupLast(SL: TStringList; const AName, AParent, Prefix: string);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLTrayIcon.SetupLast', 0

@@e_signature:
  end;
  if Active then
    SL.Add(Prefix + AName + '.Active := TRUE;');
end;

{ TKOLThread }

procedure TKOLThread.AssignEvents(SL: TStringList; const AName: string);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLThread.AssignEvents', 0

@@e_signature:
  end;
  //
end;

function TKOLThread.BestEventName: string;
begin
  Result := 'OnExecute';
end;

constructor TKOLThread.Create(AOwner: TComponent);
begin
  inherited;
  FPriorityBoost := TRUE;
end;

function TKOLThread.NotAutoFree: Boolean;
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLThread.NotAutoFree', 0

@@e_signature:
  end;
  Result := F_AutoFree;
end;

procedure TKOLThread.SetAutoFree(const Value: Boolean);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLThread.SetAutoFree', 0

@@e_signature:
  end;
  F_AutoFree := Value;
  Change;
end;

procedure TKOLThread.SetOnExecute(const Value: TOnThreadExecute);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLThread.SetOnExecute', 0

@@e_signature:
  end;
  FOnExecute := Value;
  Change;
end;

procedure TKOLThread.SetOnResume(const Value: TOnEvent);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLThread.SetOnResume', 0

@@e_signature:
  end;
  FOnResume := Value;
  Change;
end;

procedure TKOLThread.SetOnSuspend(const Value: TObjectMethod);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLThread.SetOnSuspend', 0

@@e_signature:
  end;
  FOnSuspend := Value;
  Change;
end;

procedure TKOLThread.SetPriorityBoost(const Value: Boolean);
begin
  FPriorityBoost := Value;
  Change;
end;

procedure TKOLThread.SetPriorityClass(const Value: TPriorityClass);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLThread.SetPriorityClass', 0

@@e_signature:
  end;
  FPriorityClass := Value;
  Change;
end;

procedure TKOLThread.SetstartSuspended(const Value: Boolean);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLThread.SetstartSuspended', 0

@@e_signature:
  end;
  FstartSuspended := Value;
  Change;
end;

procedure TKOLThread.SetThreadPriority(const Value: TThreadPriority);
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLThread.SetThreadPriority', 0

@@e_signature:
  end;
  FThreadPriority := Value;
  Change;
end;

procedure TKOLThread.SetupFirst(SL: TStringList; const AName, AParent, Prefix: string);
const
  PriorityClasses: array[TPriorityClass] of string = ('NORMAL_PRIORITY_CLASS',
    'IDLE_PRIORITY_CLASS', 'HIGH_PRIORITY_CLASS', 'REALTIME_PRIORITY_CLASS');
  ThreadPriorities: array[TThreadPriority] of string = ('THREAD_PRIORITY_NORMAL',
    'THREAD_PRIORITY_BELOW_NORMAL', 'THREAD_PRIORITY_LOWEST',
    'THREAD_PRIORITY_IDLE', 'THREAD_PRIORITY_ABOVE_NORMAL',
    'THREAD_PRIORITY_HIGHEST', 'THREAD_PRIORITY_CRITICAL');
var
  S: string;
begin
  asm
        jmp @@e_signature
        DB '#$signature$#', 0
        DB 'TKOLThread.SetupFirst', 0

@@e_signature:
  end;
  if startSuspended or (@OnSuspend <> nil) or (@OnResume <> nil) or (@OnDestroy
    <> nil) or AutoFree or (PriorityClass <> pcNormal) or (ThreadPriority <>
    tpNormal) or (Tag <> 0) then begin
    if AutoFree then
      SL.Add(Prefix + AName + ' := NewThreadAutoFree( nil );')
    else
      SL.Add(Prefix + AName + ' := NewThread;');
    SetupName(SL, AName, AParent, Prefix);
    if @OnExecute <> nil then
      SL.Add(Prefix + AName + '.OnExecute := Result.' + ParentForm.MethodName(@OnExecute) + ';');
    if @OnSuspend <> nil then
      SL.Add(Prefix + AName + '.OnSuspend := Result.' + ParentForm.MethodName(@OnSuspend) + ';');
    if @OnResume <> nil then
      SL.Add(Prefix + AName + '.OnResume := Result.' + ParentForm.MethodName(@OnResume) + ';');
    if @OnDestroy <> nil then
      SL.Add(Prefix + AName + '.OnDestroy := Result.' + ParentForm.MethodName(@OnDestroy) + ';');
    if PriorityClass <> pcNormal then
      SL.Add(Prefix + AName + '.PriorityClass := ' + PriorityClasses[PriorityClass] + ';');
    if ThreadPriority <> tpNormal then
      SL.Add(Prefix + AName + '.ThreadPriority := ' + ThreadPriorities[ThreadPriority] + ';');
    GenerateTag(SL, AName, Prefix);
    if not startSuspended then
      SL.Add(Prefix + AName + '.Resume;');
  end
  else begin
    S := 'nil';
    if @OnExecute <> nil then
      S := 'Result.' + ParentForm.MethodName(@OnExecute);
    SL.Add(Prefix + AName + ' := NewThreadEx( ' + S + ' );');
  end;
  if not PriorityBoost then
    SL.Add(Prefix + AName + '.PriorityBoost := FALSE;');
end;

end.

