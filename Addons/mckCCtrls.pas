unit mckCCtrls;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, mirror, mckCtrls, KOLCCtrls;

{$I KOLDEF.INC}

type
  TKOLTrackBar = class(TKOLControl)
  private
    FOptions: TTrackbarOptions;
    FPosition: Integer;
    FRangeMin: Integer;
    FSelStart: Integer;
    FThumbLen: Integer;
    FRangeMax: Integer;
    FLineSize: Integer;
    FPageSize: Integer;
    FSelEnd: Integer;
    FOnScroll: TOnScroll;
    procedure SetOptions(const Value: TTrackbarOptions);
    procedure SetPosition(const Value: Integer);
    procedure SetLineSize(const Value: Integer);
    procedure SetPageSize(const Value: Integer);
    procedure SetRangeMax(const Value: Integer);
    procedure SetRangeMin(const Value: Integer);
    procedure SetSelEnd(const Value: Integer);
    procedure SetSelStart(const Value: Integer);
    procedure SetThumbLen(const Value: Integer);
    procedure SetOnScroll(const Value: TOnScroll);
  protected
    function AdditionalUnits: string; override;
    function TabStopByDefault: Boolean; override;
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: string); override;
    function SetupParams(const AName, AParent: TDelphiString): TDelphiString; override;
    procedure SetupConstruct(SL: TStringList; const AName, AParent, Prefix: string); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Options: TTrackbarOptions read FOptions write SetOptions;
    property Position: Integer read FPosition write SetPosition;
    property RangeMin: Integer read FRangeMin write SetRangeMin;
    property RangeMax: Integer read FRangeMax write SetRangeMax;
    property PageSize: Integer read FPageSize write SetPageSize;
    property LineSize: Integer read FLineSize write SetLineSize;
    property ThumbLen: Integer read FThumbLen write SetThumbLen;
    property SelStart: Integer read FSelStart write SetSelStart;
    property SelEnd: Integer read FSelEnd write SetSelEnd;
    property OnScroll: TOnScroll read FOnScroll write SetOnScroll;
    property TabStop;
    property TabOrder;
  end;

  { SPC CONTROLS }

  TSPCDirectoryEditBox = class(TKOLControl)
  private
    { Private declarations }
    fPath: string;
    fCaptionEmpty: string;
    fTitle: string;
    fNotAvailable: Boolean;
    procedure SetTitle(Value: string);
    procedure SetCaptionEmpty(Value: string);
    procedure SetPath(Value: string);
  protected
    { Protected declarations }
    function AdditionalUnits: string; override;
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: string); override;
  public
    { Public declarations }
    constructor Create(Owner: TComponent); override;
  published
    { Published declarations }
    property Path: string read fPath write SetPath;
    property Title: string read fTitle write SetTitle;
    property Font;
    property CaptionEmpty: string read fCaptionEmpty write SetCaptionEmpty;
    property OnChange;
    property OnShow: Boolean read FNotAvailable;
    property OnScroll: Boolean read FNotAvailable;
    property OnResize: Boolean read FNotAvailable;
    property OnPaint: Boolean read FNotAvailable;
    property OnMove: Boolean read FNotAvailable;
    property OnMouseWheel: Boolean read FNotAvailable;
    property OnMouseUp: Boolean read FNotAvailable;
    property OnMouseMove: Boolean read FNotAvailable;
    property OnMouseLeave: Boolean read FNotAvailable;
    property OnMouseEnter: Boolean read FNotAvailable;
    property OnMouseDown: Boolean read FNotAvailable;
    property OnMouseDblClk: Boolean read FNotAvailable;
    property OnMessage: Boolean read FNotAvailable;
    property OnHide: Boolean read FNotAvailable;
    property OnEraseBkgnd: Boolean read FNotAvailable;
    property OnDropFiles: Boolean read FNotAvailable;
    property OnDestroy: Boolean read FNotAvailable;
    property OnClick: Boolean read FNotAvailable;
  end;

  TSortBy = (sbName, sbExtention);
  TSPCFileListBox = class(TKOLListBox)
  private
    { Private declarations }
    fIntegralHeight: Boolean;
    fDoCase: TCase;
    fPath: string;
    fFilters: string;
    FNotAvailable: Boolean;
    fExecuteOnDblClk: Boolean;
    fSortBy: TSortBy;
    procedure SetPath(Value: string);
    procedure SetFilters(Value: string);
    procedure SetIntegralHeight(Value: Boolean);
    procedure SetCase(Value: TCase);
    procedure SetExecuteOnDblClk(Value: Boolean);
    procedure SetSortBy(Value: TSortBy);
  protected
    { Protected declarations }
    function AdditionalUnits: string; override;
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: string); override;
  public
    { Public declarations }
    constructor Create(Owner: TComponent); override;
  published
    { Published declarations }
    property SortBy: TSortBy read fSortBy write SetSortBy;
    property Path: string read fPath write SetPath;
    property ExecuteOnDblClk: Boolean read fExecuteOnDblClk write SetExecuteOnDblClk;
    property Filters: string read fFilters write SetFilters;
    property DoCase: TCase read fDoCase write SetCase;
    property IntegralHeight: Boolean read fIntegralHeight write SetIntegralHeight;
    property OnChange: Boolean read FNotAvailable;
    property OnShow: Boolean read FNotAvailable;
    property OnResize: Boolean read FNotAvailable;
    property OnPaint: Boolean read FNotAvailable;
    property OnMove: Boolean read FNotAvailable;
    property OnMouseWheel: Boolean read FNotAvailable;
    property OnMouseUp: Boolean read FNotAvailable;
    property OnMouseMove: Boolean read FNotAvailable;
    property OnMouseLeave: Boolean read FNotAvailable;
    property OnMouseEnter: Boolean read FNotAvailable;
    property OnMouseDown: Boolean read FNotAvailable;
    //    property OnMOuseDblClk: Boolean read FNotAvailable;
    property OnMessage: Boolean read FNotAvailable;
    property OnMeasureItem: Boolean read FNotAvailable;
    property OnLeave: Boolean read FNotAvailable;
    property OnKeyUp: Boolean read FNotAvailable;
    property OnKeyDown: Boolean read FNotAvailable;
    property OnHide: Boolean read FNotAvailable;
    property OnEnter: Boolean read FNotAvailable;
    property OnDropFiles: Boolean read FNotAvailable;
    property OnDropDown: Boolean read FNotAvailable;
    property OnDrawItem: Boolean read FNotAvailable;
    property OnDestroy: Boolean read FNotAvailable;
    property OnEraseBkgnd: Boolean read FNotAvailable;
    property OnCloseUp: Boolean read FNotAvailable;
    property OnClick: Boolean read FNotAvailable;
    property OnChar: Boolean read FNotAvailable;
    property OnScroll: Boolean read FNotAvailable;
    //    property Items: Boolean read FNotAvailable;
  end;

  TSPCDirectoryListBox = class(TKOLListView)
  private
    { Private declarations }
    fIntegralHeight: Boolean;
    fDoIndent: Boolean;
    fPath: string;
    FNotAvailable: Boolean;
    fFileListBox: TSPCFileListBox;
    procedure SetPath(Value: string);
    procedure SetIndent(Value: Boolean);
    procedure SetIntegralHeight(Value: Boolean);
    procedure SetFileListBox(Value: TSPCFileListBox);
  protected
    { Protected declarations }
    function AdditionalUnits: string; override;
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: string); override;
    procedure SetupLast(SL: TStringList; const AName, AParent, Prefix: string); override;
  public
    { Public declarations }
    constructor Create(Owner: TComponent); override;
  published
    { Published declarations }
    property FileListBox: TSPCFileListBox read fFileListBox write SetFileListBox;
    property Items: Boolean read FNotAvailable;
    property ImageListState: Boolean read FNotAvailable;
    property ImageListSmall: Boolean read FNotAvailable;
    property ImageListNormal: Boolean read FNotAvailable;
    property Path: string read fPath write SetPath;
    property IntegralHeight: Boolean read fIntegralHeight write SetIntegralHeight;
    property DoIndent: Boolean read fDoIndent write SetIndent;
    //    property OnChange: Boolean read FNotAvailable;
    property OnShow: Boolean read FNotAvailable;
    property OnResize: Boolean read FNotAvailable;
    property OnPaint: Boolean read FNotAvailable;
    property OnMove: Boolean read FNotAvailable;
    property OnMouseWheel: Boolean read FNotAvailable;
    property OnMouseUp: Boolean read FNotAvailable;
    property OnMouseMove: Boolean read FNotAvailable;
    property OnMouseLeave: Boolean read FNotAvailable;
    property OnMouseEnter: Boolean read FNotAvailable;
    property OnMouseDown: Boolean read FNotAvailable;
    property OnMessage: Boolean read FNotAvailable;
    property OnMeasureItem: Boolean read FNotAvailable;
    property OnLeave: Boolean read FNotAvailable;
    property OnKeyUp: Boolean read FNotAvailable;
    property OnKeyDown: Boolean read FNotAvailable;
    property OnHide: Boolean read FNotAvailable;
    property OnEnter: Boolean read FNotAvailable;
    property OnDropFiles: Boolean read FNotAvailable;
    property OnDropDown: Boolean read FNotAvailable;
    property OnDrawItem: Boolean read FNotAvailable;
    property OnDestroy: Boolean read FNotAvailable;
    property OnEraseBkgnd: Boolean read FNotAvailable;
    property OnCloseUp: Boolean read FNotAvailable;
    property OnClick: Boolean read FNotAvailable;
    property OnChar: Boolean read FNotAvailable;
    property OnScroll: Boolean read FNotAvailable;
    property OnLVStateChange: Boolean read FNotAvailable;
    property OnLVData: Boolean read FNotAvailable;
    property OnLVDelete: Boolean read FNotAvailable;
    property OnEndEditLVItem: Boolean read FNotAvailable;
    property OnDeleteLVItem: Boolean read FNotAvailable;
    property OnDeleteAllLVItems: Boolean read FNotAvailable;
    property OnCompareLVItems: Boolean read FNotAvailable;
    property OnColumnClick: Boolean read FNotAvailable;
  end;

  TSPCDriveComboBox = class(TKOLComboBox)
  private
    { Private declarations }
    fDrive: char;
    FNotAvailable: Boolean;
    fDirectoryListBox: TSPCDirectoryListBox;
    procedure SetDrive(Value: char);
    procedure SetDirectoryListBox(Value: TSPCDirectoryListBox);
  protected
    { Protected declarations }
    function AdditionalUnits: string; override;
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: string); override;
    procedure SetupLast(SL: TStringList; const AName, AParent, Prefix: string); override;
  public
    { Public declarations }
    constructor Create(Owner: TComponent); override;
  published
    { Published declarations }
    property DirectoryListBox: TSPCDirectoryListBox read fDirectoryListBox write SetDirectoryListBox;
    property Drive: char read fDrive write SetDrive;
    property OnSelChange: Boolean read FNotAvailable;
    property OnShow: Boolean read FNotAvailable;
    property OnResize: Boolean read FNotAvailable;
    property OnPaint: Boolean read FNotAvailable;
    property OnMove: Boolean read FNotAvailable;
    property OnMouseWheel: Boolean read FNotAvailable;
    property OnMouseUp: Boolean read FNotAvailable;
    property OnMouseMove: Boolean read FNotAvailable;
    property OnMouseLeave: Boolean read FNotAvailable;
    property OnMouseEnter: Boolean read FNotAvailable;
    property OnMouseDown: Boolean read FNotAvailable;
    property OnMOuseDblClk: Boolean read FNotAvailable;
    property OnMessage: Boolean read FNotAvailable;
    property OnMeasureItem: Boolean read FNotAvailable;
    property OnLeave: Boolean read FNotAvailable;
    property OnKeyUp: Boolean read FNotAvailable;
    property OnKeyDown: Boolean read FNotAvailable;
    property OnHide: Boolean read FNotAvailable;
    property OnEnter: Boolean read FNotAvailable;
    property OnDropFiles: Boolean read FNotAvailable;
    property OnDropDown: Boolean read FNotAvailable;
    property OnDrawItem: Boolean read FNotAvailable;
    property OnDestroy: Boolean read FNotAvailable;
    property OnEraseBkgnd: Boolean read FNotAvailable;
    property OnCloseUp: Boolean read FNotAvailable;
    property OnClick: Boolean read FNotAvailable;
    property OnChar: Boolean read FNotAvailable;
    property Items: Boolean read FNotAvailable;
  end;

  TSPCFilterComboBox = class(TKOLComboBox)
  private
    { Private declarations }
    fLines: TStrings;
    FNotAvailable: Boolean;
    fFileListBox: TSPCFileListBox;
    //    procedure SetText(Value: TStrings);
    //    function GetText: TStrings;
    procedure SetFileListBox(Value: TSPCFileListBox);
  protected
    { Protected declarations }
    function AdditionalUnits: string; override;
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: string); override;
    procedure SetupLast(SL: TStringList; const AName, AParent, Prefix: string); override;
  public
    { Public declarations }
    constructor Create(Owner: TComponent); override;
  published
    { Published declarations }
    property FileListBox: TSPCFileListBox read fFileListBox write SetFileListBox;
    //    property Items: Boolean read FNotAvailable;
    //    property Filters: TStrings read GetText write SetText;
    property OnSelChange: Boolean read FNotAvailable;
    property OnShow: Boolean read FNotAvailable;
    property OnResize: Boolean read FNotAvailable;
    property OnPaint: Boolean read FNotAvailable;
    property OnMove: Boolean read FNotAvailable;
    property OnMouseWheel: Boolean read FNotAvailable;
    property OnMouseUp: Boolean read FNotAvailable;
    property OnMouseMove: Boolean read FNotAvailable;
    property OnMouseLeave: Boolean read FNotAvailable;
    property OnMouseEnter: Boolean read FNotAvailable;
    property OnMouseDown: Boolean read FNotAvailable;
    property OnMOuseDblClk: Boolean read FNotAvailable;
    property OnMessage: Boolean read FNotAvailable;
    property OnMeasureItem: Boolean read FNotAvailable;
    property OnLeave: Boolean read FNotAvailable;
    property OnKeyUp: Boolean read FNotAvailable;
    property OnKeyDown: Boolean read FNotAvailable;
    property OnHide: Boolean read FNotAvailable;
    property OnEnter: Boolean read FNotAvailable;
    property OnDropFiles: Boolean read FNotAvailable;
    property OnDropDown: Boolean read FNotAvailable;
    property OnDrawItem: Boolean read FNotAvailable;
    property OnDestroy: Boolean read FNotAvailable;
    property OnEraseBkgnd: Boolean read FNotAvailable;
    property OnCloseUp: Boolean read FNotAvailable;
    property OnClick: Boolean read FNotAvailable;
    property OnChar: Boolean read FNotAvailable;
  end;

  TSPCStatusBar = class(TKOLControl)
  private
    { Private declarations }
    FNotAvailable: Boolean;
    fSimpleStatusText: string;
    fSizeGrip: Boolean;
    procedure SetSimpleStatusText(Value: string);
    procedure SetSizeGrip(Value: Boolean);
  protected
    { Protected declarations }
    function AdditionalUnits: string; override;
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: string); override;
  public
    { Public declarations }
    constructor Create(Owner: TComponent); override;
  published
    { Published declarations }
    property SizeGrip: Boolean read fSizeGrip write SetSizeGrip;
    property OnShow: Boolean read FNotAvailable;
    property SimpleStatusText: string read fSimpleStatusText write SetSimpleStatusText;
    property Caption: Boolean read FNotAvailable;
    property OnPaint: Boolean read FNotAvailable;
    property OnMove: Boolean read FNotAvailable;
    property OnMouseWheel: Boolean read FNotAvailable;
    property OnMouseUp: Boolean read FNotAvailable;
    property OnMouseMove: Boolean read FNotAvailable;
    property OnMouseLeave: Boolean read FNotAvailable;
    property OnMouseEnter: Boolean read FNotAvailable;
    property OnMouseDown: Boolean read FNotAvailable;
    property OnMOuseDblClk: Boolean read FNotAvailable;
    property OnMessage: Boolean read FNotAvailable;
    property OnMeasureItem: Boolean read FNotAvailable;
    property OnLeave: Boolean read FNotAvailable;
    property OnKeyUp: Boolean read FNotAvailable;
    property OnKeyDown: Boolean read FNotAvailable;
    property OnHide: Boolean read FNotAvailable;
    property OnEnter: Boolean read FNotAvailable;
    property OnDropFiles: Boolean read FNotAvailable;
    property OnDropDown: Boolean read FNotAvailable;
    property OnDrawItem: Boolean read FNotAvailable;
    property OnDestroy: Boolean read FNotAvailable;
    property OnEraseBkgnd: Boolean read FNotAvailable;
    property OnCloseUp: Boolean read FNotAvailable;
    property OnClick: Boolean read FNotAvailable;
    property OnChar: Boolean read FNotAvailable;
    property Items: Boolean read FNotAvailable;
  end;

procedure Register;

(*)
{$R mckCCtrls.dcr}
(*)

implementation

procedure Register;
begin
  RegisterComponents('KOLAddons', [TKOLTrackBar, TSPCDirectoryEditBox,
    TSPCDirectoryListBox, TSPCDriveComboBox, TSPCFileListBox, TSPCFilterComboBox,
      TSPCStatusBar]);
end;

{ TKOLTrackBar }

function TKOLTrackBar.AdditionalUnits: string;
begin
  Result := ', KOLCCtrls';
end;

constructor TKOLTrackBar.Create(AOwner: TComponent);
begin
  inherited;
  Width := 200;
  DefaultWidth := Width;
  Height := 40;
  DefaultHeight := Height;
end;

procedure TKOLTrackBar.SetLineSize(const Value: Integer);
begin
  FLineSize := Value;
  Change;
end;

procedure TKOLTrackBar.SetOnScroll(const Value: TOnScroll);
begin
  FOnScroll := Value;
  Change;
end;

procedure TKOLTrackBar.SetOptions(const Value: TTrackbarOptions);
begin
  FOptions := Value;
  Change;
end;

procedure TKOLTrackBar.SetPageSize(const Value: Integer);
begin
  FPageSize := Value;
  Change;
end;

procedure TKOLTrackBar.SetPosition(const Value: Integer);
begin
  FPosition := Value;
  Change;
end;

procedure TKOLTrackBar.SetRangeMax(const Value: Integer);
begin
  FRangeMax := Value;
  Change;
end;

procedure TKOLTrackBar.SetRangeMin(const Value: Integer);
begin
  FRangeMin := Value;
  Change;
end;

procedure TKOLTrackBar.SetSelEnd(const Value: Integer);
begin
  FSelEnd := Value;
  Change;
end;

procedure TKOLTrackBar.SetSelStart(const Value: Integer);
begin
  FSelStart := Value;
  Change;
end;

procedure TKOLTrackBar.SetThumbLen(const Value: Integer);
begin
  FThumbLen := Value;
  Change;
end;

procedure TKOLTrackBar.SetupConstruct(SL: TStringList; const AName,
  AParent, Prefix: string);
var
  S                 : string;
begin
  S := GenerateTransparentInits;
  SL.Add(Prefix + AName + ' := PTrackbar( New' + TypeName + '( '
    + SetupParams(AName, AParent) + ' )' + S + ');');
end;

procedure TKOLTrackBar.SetupFirst(SL: TStringList; const AName, AParent,
  Prefix: string);
begin
  inherited;
  if RangeMin <> 0 then
    SL.Add(Prefix + AName + '.RangeMin := ' + IntToStr(RangeMin) + ';');
  if RangeMax <> 0 then
    SL.Add(Prefix + AName + '.RangeMax := ' + IntToStr(RangeMax) + ';');
  if PageSize <> 0 then
    SL.Add(Prefix + AName + '.PageSize := ' + IntToStr(PageSize) + ';');
  if LineSize <> 0 then
    SL.Add(Prefix + AName + '.LineSize := ' + IntToStr(LineSize) + ';');
  if Position <> 0 then
    SL.Add(Prefix + AName + '.Position := ' + IntToStr(Position) + ';');
  if ThumbLen <> 0 then
    SL.Add(Prefix + AName + '.ThumbLen := ' + IntToStr(ThumbLen) + ';');
  if SelStart <> 0 then
    SL.Add(Prefix + AName + '.SelStart := ' + IntToStr(SelStart) + ';');
  if SelEnd <> 0 then
    SL.Add(Prefix + AName + '.SelEnd := ' + IntToStr(SelEnd) + ';');
end;

function TKOLTrackBar.SetupParams(const AName, AParent: TDelphiString): TDelphiString;
var
  S                 : string;
begin
  S := '';
  if trbAutoTicks in Options then S := 'trbAutoTicks,';
  if trbEnableSelRange in Options then S := S + 'trbEnableSelRange,';
  if trbFixedLength in Options then S := S + 'trbFixedLength,';
  if trbNoThumb in Options then S := S + 'trbNoThumb,';
  if trbNoTicks in Options then S := S + 'trbNoTicks,';
  if trbTooltips in Options then S := S + 'trbTooltips,';
  if trbTopLeftMarks in Options then S := S + 'trbTopLeftMarks,';
  if trbVertical in Options then S := S + 'trbVertical,';
  if trbNoBorder in Options then S := S + 'trbNoBorder,';
  S := Copy(S, 1, Length(S) - 1);
  Result := AParent + ', [' + S + '], ';
  if TMethod(OnScroll).Code <> nil then
    Result := Result + 'Result.' + ParentForm.MethodName(TMethod(OnScroll).Code)
  else
    Result := Result + 'nil';
end;

function TKOLTrackBar.TabStopByDefault: Boolean;
begin
  Result := TRUE;
end;

{ TSPCDirectoryEditBox }

constructor TSPCDirectoryEditBox.Create;
var
  TS                : string;
begin
  inherited;
  Width := 145;
  Height := 21;
  Title := 'Select folder:';
  GetDir(0, TS);
  Path := TS;
  Font.FontHeight := -11;
  Color := $FFFFFF;
end;

function TSPCDirectoryEditBox.AdditionalUnits;
begin
  Result := ', KOLCCtrls';
end;

procedure TSPCDirectoryEditBox.SetPath(Value: string);
begin
  if DirectoryExists(Value) then fPath := Value else fPath := '';
  Change;
end;

procedure TSPCDirectoryEditBox.SetCaptionEmpty(Value: string);
begin
  fCaptionEmpty := Value;
  Change;
end;

procedure TSPCDirectoryEditBox.SetTitle(Value: string);
begin
  fTitle := Value;
  Change;
end;

procedure TSPCDirectoryEditBox.SetupFirst;
begin
  inherited;
  SL.Add(Prefix + AName + '.Color:=' + IntToStr(Color) + ';');
  SL.Add(Prefix + AName + '.Title:=''' + Title + ''';');
  SL.Add(Prefix + AName + '.CaptionEmpty:=''' + CaptionEmpty + ''';');
  SL.Add(Prefix + AName + '.Initialize;');
  SL.Add(Prefix + AName + '.Path:=''' + Path + ''';');
  SL.Add(Prefix + AName + '.Top:=' + IntToStr(Top) + ';');
  SL.Add(Prefix + AName + '.Left:=' + IntToStr(Left) + ';');
  SL.Add(Prefix + AName + '.Width:=' + IntToStr(Width) + ';');
  SL.Add(Prefix + AName + '.Height:=' + IntToStr(Height) + ';');
end;

{ TSPCDirectoryListBox }

procedure TSPCDirectoryListBox.SetIndent(Value: Boolean);
begin
  fDoIndent := Value;
  Change;
end;

constructor TSPCDirectoryListBox.Create;
var
  TS                : string;
begin
  inherited;
  Width := 145;
  Height := 105;
  DoIndent := True;
  GetDir(0, TS);
  Path := TS;
end;

function TSPCDirectoryListBox.AdditionalUnits;
begin
  Result := ', KOLCCtrls';
end;

function Boolean2Str(b: Boolean): string;
begin
  if b then
    Result := 'True'
  else
    Result := 'False';
end;

procedure TSPCDirectoryListBox.SetupFirst;
//var St: string;
begin
  inherited;
  SL.Add(Prefix + AName + '.Top:=' + IntToStr(Top) + ';');
  SL.Add(Prefix + AName + '.Left:=' + IntToStr(Left) + ';');
  SL.Add(Prefix + AName + '.Width:=' + IntToStr(Width) + ';');
  SL.Add(Prefix + AName + '.Height:=' + IntToStr(Height) + ';');
  SL.Add(Prefix + AName + '.DoIndent:=' + Boolean2Str(DoIndent) + ';');
  SL.Add(Prefix + AName + '.IntegralHeight:=' + Boolean2Str(IntegralHeight) + ';');
  SL.Add(Prefix + AName + '.Path:=''' + Path + ''';');
end;

procedure TSPCDirectoryListBox.SetupLast(SL: TStringList; const AName, AParent, Prefix: string);
begin
  if Assigned(fFileListBox) then if Length(fFileListBox.Name) > 0 then SL.Add(Prefix + AName + '.FileListBox:=Result.' + fFileListBox.Name + ';');
end;

procedure TSPCDirectoryListBox.SetFileListBox(Value: TSPCFileListBox);
begin
  fFileListBox := Value;
  Change;
end;

procedure TSPCDirectoryListBox.SetIntegralHeight(Value: Boolean);
begin
  fIntegralHeight := Value;
  Change;
end;

procedure TSPCDirectoryListBox.SetPath(Value: string);
var
  fT                : string;
begin
  fT := Value;
  if Value[Length(Value)] = '\' then fT := Value else
    if Length(Value) = 1 then fT := Value + ':\' else fT := Value + '\';
  if DirectoryExists(fT) then fPath := fT else fPath := '';
  Change;
end;

{ TSPCDriveComboBox }

constructor TSPCDriveComboBox.Create;
var
  TS                : string;
begin
  inherited;
  Width := 145;
  Height := 22;
  Color := clWhite;
  GetDir(0, TS);
  Drive := TS[1];
end;

function TSPCDriveComboBox.AdditionalUnits;
begin
  Result := ', KOLCCtrls';
end;

procedure TSPCDriveComboBox.SetupFirst;
begin
  inherited;
  SL.Add(Prefix + AName + '.Top:=' + IntToStr(Top) + ';');
  SL.Add(Prefix + AName + '.Left:=' + IntToStr(Left) + ';');
  SL.Add(Prefix + AName + '.Width:=' + IntToStr(Width) + ';');
  SL.Add(Prefix + AName + '.Height:=' + IntToStr(Height) + ';');
  SL.Add(Prefix + AName + '.Color:=' + IntToStr(Color) + ';');
  SL.Add(Prefix + AName + '.Drive:=''' + Drive + ''';');
end;

procedure TSPCDriveComboBox.SetupLast(SL: TStringList; const AName, AParent, Prefix: string);
begin
  if Assigned(fDirectoryListBox) then if Length(fDirectoryListBox.Name) > 0 then SL.Add(Prefix + AName + '.DirectoryListBox:=Result.' + fDirectoryListBox.Name + ';');
end;

procedure TSPCDriveComboBox.SetDirectoryListBox(Value: TSPCDirectoryListBox);
begin
  fDirectoryListBox := Value;
  Change;
end;

procedure TSPCDriveComboBox.SetDrive;
var
  fC                : Char;
begin
  fC := Value;
  if DirectoryExists(fC + ':') then
    fDrive := Value;
  Change;
end;

{ TSPCFileListBox }

constructor TSPCFileListBox.Create;
var
  TS                : string;
begin
  inherited;
  Width := 145;
  Height := 105;
  Filters := '*.*';
  DoCase := ctLower;
  GetDir(0, TS);
  Path := TS;
  Font.FontHeight := -11;
end;

function TSPCFileListBox.AdditionalUnits;
begin
  Result := ', KOLCCtrls';
end;

procedure TSPCFileListBox.SetupFirst;
var
  St                : string;
begin
  inherited;
  case DoCase of
    ctDefault: St := 'ctDefault';
    ctLower: St := 'ctLower';
    ctUpper: St := 'ctUpper';
  end;
  SL.Add(Prefix + AName + '.DoCase:=' + St + ';');
  SL.Add(Prefix + AName + '.IntegralHeight:=' + Boolean2Str(IntegralHeight) + ';');
  SL.Add(Prefix + AName + '.Filters:=''' + Filters + ''';');
  SL.Add(Prefix + AName + '.Color:=' + IntToStr(Color) + ';');
  SL.Add(Prefix + AName + '.Top:=' + IntToStr(Top) + ';');
  SL.Add(Prefix + AName + '.Left:=' + IntToStr(Left) + ';');
  SL.Add(Prefix + AName + '.Width:=' + IntToStr(Width) + ';');
  SL.Add(Prefix + AName + '.Height:=' + IntToStr(Height) + ';');
  SL.Add(Prefix + AName + '.Path:=''' + Path + ''';');
  SL.Add(Prefix + AName + '.ExecuteOnDblClk:=' + Boolean2Str(ExecuteOnDblClk) + ';');
  case fSortBy of
    sbName: SL.Add(Prefix + AName + '._SortBy:=sbName;');
    sbExtention: SL.Add(Prefix + AName + '._SortBy:=sbExtention;');
  end;
end;

procedure TSPCFileListBox.SetCase(Value: TCase);
begin
  fDoCase := Value;
  Change;
end;

procedure TSPCFileListBox.SetIntegralHeight(Value: Boolean);
begin
  fIntegralHeight := Value;
  Change;
end;

procedure TSPCFileListBox.SetFilters(Value: string);
begin
  fFilters := Value;
  Change;
end;

procedure TSPCFileListBox.SetPath(Value: string);
begin
  if DirectoryExists(Value) then
  begin
    if Value[Length(Value)] = '\' then fPath := Value else fPath := Value + '\';
  end else fPath := '';
  Change;
end;

procedure TSPCFileListBox.SetExecuteOnDblClk(Value: Boolean);
begin
  fExecuteOnDblClk := Value;
  Change;
end;

procedure TSPCFileListBox.SetSortBy(Value: TSortBy);
begin
  fSortBy := Value;
  Change;
end;

{ TSPCFilterComboBox }

constructor TSPCFilterComboBox.Create;
//var
//  TS: string;
begin
  inherited;
  Width := 145;
  Height := 22;
  Color := clWhite;
  fLines := TStringList.Create;
  Font.FontHeight := -11;
end;

function TSPCFilterComboBox.AdditionalUnits;
begin
  Result := ', KOLCCtrls';
end;

procedure TSPCFilterComboBox.SetFileListBox(Value: TSPCFileListBox);
begin
  fFileListBox := Value;
  Change;
end;

{procedure TSPCFilterComboBox.SetText;
begin
  fLines.Text:=Value.Text;
  Change;
end;

function TSPCFilterComboBox.GetText: TStrings;
begin
  Result:=fLines;
end;}

procedure TSPCFilterComboBox.SetupFirst;
//var
//  i: Integer;
begin
  inherited;
  if (Length(FLines.Text) > 0) then
    AddLongTextField(SL, Prefix, AName + '.Text:=', FLines.Text, ';');
  SL.Add(Prefix + AName + '.Color:=' + IntToStr(Color) + ';');
  //   SL.Add( Prefix + AName + '.BuildList;');
  SL.Add(Prefix + AName + '.Top:=' + IntToStr(Top) + ';');
  SL.Add(Prefix + AName + '.Left:=' + IntToStr(Left) + ';');
  SL.Add(Prefix + AName + '.Width:=' + IntToStr(Width) + ';');
  SL.Add(Prefix + AName + '.Height:=' + IntToStr(Height) + ';');
end;

procedure TSPCFilterComboBox.SetupLast(SL: TStringList; const AName, AParent, Prefix: string);
begin
  if Assigned(fFileListBox) then if Length(fFileListBox.Name) > 0 then SL.Add(Prefix + AName + '.FileListBox:=Result.' + fFileListBox.Name + ';');
end;

{ TSPCStatusBar }

constructor TSPCStatusBar.Create;
//var
//  TS: string;
begin
  inherited;
  Width := 145;
  Height := 19;
  Align := TKOLAlign(caBottom);
end;

function TSPCStatusBar.AdditionalUnits;
begin
  Result := ', KOLCCtrls';
end;

procedure TSPCStatusBar.SetupFirst;
//var
//  St: string;
begin
  inherited;
  SL.Add(Prefix + AName + '.Top:=' + IntToStr(Top) + ';');
  SL.Add(Prefix + AName + '.Left:=' + IntToStr(Left) + ';');
  SL.Add(Prefix + AName + '.Width:=' + IntToStr(Width) + ';');
  SL.Add(Prefix + AName + '.Height:=' + IntToStr(Height) + ';');
  SL.Add(Prefix + AName + '.SimpleStatusText:=''' + SimpleStatusText + ''';');
  SL.Add(Prefix + AName + '.SizeGrip:=' + Boolean2Str(SizeGrip) + ';');
end;

procedure TSPCStatusBar.SetSimpleStatusText(Value: string);
begin
  fSimpleStatusText := Value;
  Change;
end;

procedure TSPCStatusBar.SetSizeGrip(Value: Boolean);
begin
  fSizeGrip := Value;
  Change;
end;

end.

