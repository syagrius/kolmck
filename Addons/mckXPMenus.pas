unit mckXPMenus;

interface

uses
  Windows, mirror, Messages, Graphics, Classes, Math, SysUtils;

type
  {$IFDEF _DXE2orHigher}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TKOLXPMainMenu = class(TKOLMainMenu)
  private
    { Private declarations }
    FBackColor: TColor;
    FGutterColor: TColor;
    FSelectedColor: TColor;
    FCheckColor: TColor;
    FFont:TKOLFont;
    FItemHeight: integer;
    FItemWidth: integer;
    procedure SetBackColor(const Value:TColor);
    procedure SetGutterColor(const Value:TColor);
    procedure SetSelectedColor(const Value:TColor);
    procedure SetCheckColor(const Value:TColor);
    procedure SetFont(const Value:TKOLFont);
    procedure SetItemHeight(const Value:integer);
    procedure SetItemWidth(const Value:integer);
  protected
    { Protected declarations }
    function AdditionalUnits: string; override;

  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
    procedure SetupFirst(SL: TStringList; const AName,AParent, Prefix: String); override;
    procedure SetupLast(SL: TStringList; const AName, AParent, Prefix: String); override;
    function TypeName: String; override;
  published
    { Published declarations }
    property  BackColor: TColor read FBackColor write SetBackColor;
    property  GutterColor: TColor read FGutterColor write SetGutterColor;
    property  SelectedColor: TColor read FSelectedColor write SetSelectedColor;
    property  Font: TKOLFont read FFont write SetFont;
    property  ItemHeight: integer read FItemHeight write SetItemHeight;
    property  ItemWidth: integer read FItemWidth write SetItemWidth;
    property  CheckColor: TColor read FCheckColor write SetCheckColor;
  end;

  {$IFDEF _DXE2orHigher}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TKOLXPPopupMenu = class(TKOLPopupMenu)
  private
    { Private declarations }
    FBackColor: TColor;
    FGutterColor: TColor;
    FSelectedColor: TColor;
    FCheckColor: TColor;
    FFont:TKOLFont;
    FItemHeight: integer;
    FItemWidth: integer;
    procedure SetBackColor(const Value:TColor);
    procedure SetGutterColor(const Value:TColor);
    procedure SetSelectedColor(const Value:TColor);
    procedure SetCheckColor(const Value:TColor);
    procedure SetFont(const Value:TKOLFont);
    procedure SetItemHeight(const Value:integer);
    procedure SetItemWidth(const Value:integer);
  protected
    { Protected declarations }
    function AdditionalUnits: string; override;

  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
    function TypeName: String; override;
    procedure SetupFirst(SL: TStringList; const AName,AParent, Prefix: String); override;
    procedure SetupLast(SL: TStringList; const AName, AParent, Prefix: String); override;
  published
    { Published declarations }
    property  BackColor: TColor read FBackColor write SetBackColor;
    property  GutterColor: TColor read FGutterColor write SetGutterColor;
    property  SelectedColor: TColor read FSelectedColor write SetSelectedColor;
    property  Font: TKOLFont read FFont write SetFont;
    property  ItemHeight: integer read FItemHeight write SetItemHeight;
    property  ItemWidth: integer read FItemWidth write SetItemWidth;
    property  CheckColor: TColor read FCheckColor write SetCheckColor;
  end;


procedure Register;

implementation

type
  TRGB = packed record
    R, G, B: Byte;
  end;

procedure Register;
begin
  RegisterComponents('KOLAddons', [TKOLXPMainMenu]);
  RegisterComponents('KOLAddons', [TKOLXPPopupMenu]);
end;

function GetRGB(const Color: TColor): TRGB;
var
  iColor: TColor;
begin
  iColor := ColorToRGB(Color);
  Result.R := GetRValue(iColor);
  Result.G := GetGValue(iColor);
  Result.B := GetBValue(iColor);
end;


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

// XP Main Menu

constructor TKOLXPMainMenu.Create(AOwner:TComponent);
begin
  FGutterColor := clBtnFace;
  FBackColor :=  GetLightColor(clBtnFace, 85);
  FSelectedColor := GetLightColor(clHighlight, 65);
  fFont := TKOLFont.Create(Self);
  FCheckColor:= clBlack;
  inherited;
end;

destructor TKOLXPMainMenu.Destroy;
begin
  fFont.Free ;
  inherited;
end;


function TKOLXPMainMenu.AdditionalUnits;
begin
  Result := ', XPMenus';
end;

function TKOLXPMainMenu.TypeName;
begin
  Result := 'XPMenu';
end;

procedure TKOLXPMainMenu.SetupFirst(SL: TStringList; const AName,AParent, Prefix: String);
var i:integer;
     MI: TKOLMenuItem;
     s:string;
begin
  if Count = 0 then Exit;
  SL.Add( Prefix + AName + ' := NewXPMenu( ' + AParent + ', 0, [ ' );

  for I := 0 to Count - 1 do
  begin
    MI := Items[ I ];
    MI.SetupTemplate( SL, I = 0, ParentKOLForm );
  end;

  S := ''''' ], ' + OnMenuItemMethodName() + ', false );';
  if Count <> 0 then
    S := ', ' + S;
  if Length( S ) + Length( SL[ SL.Count - 1 ] ) > 64 then
    SL.Add( Prefix + '  ' + S )
  else
    SL[ SL.Count - 1 ] := SL[ SL.Count - 1 ] + S;
  if Name <> '' then
  begin
    SL.Add( '   {$IFDEF USE_NAMES}' );
    SL.Add( Prefix + AName + '.Name := ''' + Name + ''';' );
    SL.Add( '   {$ENDIF}' );
  end;
  for I := 0 to Count - 1 do
  begin
    MI := Items[ I ];
    MI.SetupAttributes( SL, AName );
  end;
  GenerateTag( SL, AName, Prefix );
end;

procedure TKOLXPMainMenu.SetupLast(SL: TStringList; const AName, AParent, Prefix: String);
begin
   inherited;
   if   fBackColor<>GetLightColor(clBtnFace, 85) then SL.Add(Prefix + AName +'.BackColor :='+ Color2Str(fBackColor)+';');
   if   fGutterColor<>clBtnFace then SL.Add(Prefix + AName +'.GutterColor :='+ Color2Str(fGutterColor)+';');
   if   fSelectedColor<>GetLightColor(clHighlight, 65) then SL.Add(Prefix + AName +'.SelectedColor :='+ Color2Str(fSelectedColor)+';');
   if   fCheckColor<>clBlack then SL.Add(Prefix + AName +'.CheckColor :='+ Color2Str(fCheckColor)+';');
   fFont.GenerateCode(SL,AName,nil);
   if   fItemHeight<>0 then SL.Add(Prefix + AName +'.ItemHeight :='+ inttoStr(fItemHeight)+';');
   if   fItemWidth<>0 then SL.Add(Prefix + AName +'.ItemWidth :='+ inttoStr(fItemWidth)+';');
   SL.Add(Prefix + AName +'.DrawXPStyle;');
end;

procedure TKOLXPMainMenu.SetBackColor(const Value:TColor);
begin
  if FBackColor=Value then begin
    FBackColor:=Value;
    Change;
  end;
end;

procedure TKOLXPMainMenu.SetGutterColor(const Value:TColor);
begin
  if FGutterColor=Value then begin
    FGutterColor:=Value;
    Change;
  end;
end;

procedure TKOLXPMainMenu.SetSelectedColor(const Value:TColor);
begin
  if FSelectedColor<>Value then begin
    FSelectedColor:=Value;
    Change;
  end;
end;

procedure TKOLXPMainMenu.SetCheckColor(const Value:TColor);
begin
  if FCheckColor<>Value then begin
    FCheckColor:=Value;
    Change;
  end;  
end;

procedure TKOLXPMainMenu.SetFont(const Value:TKOLFont);
begin
  FFont:=Value;
  Change;
end;

procedure TKOLXPMainMenu.SetItemHeight(const Value:integer);
begin
  if FItemHeight<>Value then begin
     FItemHeight:= Value;
     Change;
  end;
end;

procedure TKOLXPMainMenu.SetItemWidth(const Value:integer);
begin
  if FItemWidth<>Value then begin
     FItemWidth:= Value;
     Change;
  end;
end;

// XP Popup Menu

constructor TKOLXPPopupMenu.Create(AOwner:TComponent);
begin
  FGutterColor := clBtnFace;
  FBackColor :=  GetLightColor(clBtnFace, 85);
  FSelectedColor := GetLightColor(clHighlight, 65);
  fFont := TKOLFont.Create(Self);
  FCheckColor:= clBlack;
  inherited;
end;

destructor TKOLXPPopupMenu.Destroy;
begin
  fFont.Free ;
  inherited;
end;

function TKOLXPPopupMenu.AdditionalUnits;
begin
  Result := ', XPMenus';
end;

function TKOLXPPopupMenu.TypeName;
begin
  Result := 'XPMenu';
end;

procedure TKOLXPPopupMenu.SetupFirst(SL: TStringList; const AName,AParent, Prefix: String);
var i:integer;
     MI: TKOLMenuItem;
     s:string;
begin
  if Count = 0 then Exit;
  SL.Add( Prefix + AName + ' := NewXPMenu( ' + AParent + ', 0, [ ' );

  for I := 0 to Count - 1 do
  begin
    MI := Items[ I ];
    MI.SetupTemplate( SL, I = 0, ParentKOLForm );
  end;

  S := ''''' ], ' + OnMenuItemMethodName() + ', true );';
  if Count <> 0 then
    S := ', ' + S;
  if Length( S ) + Length( SL[ SL.Count - 1 ] ) > 64 then
    SL.Add( Prefix + '  ' + S )
  else
    SL[ SL.Count - 1 ] := SL[ SL.Count - 1 ] + S;
  if Name <> '' then
  begin
    SL.Add( '   {$IFDEF USE_NAMES}' );
    SL.Add( Prefix + AName + '.Name := ''' + Name + ''';' );
    SL.Add( '   {$ENDIF}' );
  end;
  for I := 0 to Count - 1 do
  begin
    MI := Items[ I ];
    MI.SetupAttributes( SL, AName );
  end;
  GenerateTag( SL, AName, Prefix );
end;

procedure TKOLXPPopupMenu.SetupLast(SL: TStringList; const AName, AParent, Prefix: String);
begin
   inherited;
   if   fBackColor<>GetLightColor(clBtnFace, 85) then SL.Add(Prefix + AName +'.BackColor :='+ Color2Str(fBackColor)+';');
   if   fGutterColor<>clBtnFace then SL.Add(Prefix + AName +'.GutterColor :='+ Color2Str(fGutterColor)+';');
   if   fSelectedColor<>GetLightColor(clHighlight, 65) then SL.Add(Prefix + AName +'.SelectedColor :='+ Color2Str(fSelectedColor)+';');
   if   fCheckColor<>clBlack then SL.Add(Prefix + AName +'.CheckColor :='+ Color2Str(fCheckColor)+';');
   fFont.GenerateCode(SL,AName,nil);
   if   fItemHeight<>0 then SL.Add(Prefix + AName +'.ItemHeight :='+ inttoStr(fItemHeight)+';');
   if   fItemWidth<>0 then SL.Add(Prefix + AName +'.ItemWidth :='+ inttoStr(fItemWidth)+';');
   SL.Add(Prefix + AName +'.DrawXPStyle;');
end;

procedure TKOLXPPopupMenu.SetBackColor(const Value:TColor);
begin
  if FBackColor<>Value then begin
    FBackColor:=Value;
    Change;
  end;
end;

procedure TKOLXPPopupMenu.SetGutterColor(const Value:TColor);
begin
  if FGutterColor<>Value then begin
    FGutterColor:=Value;
    Change;
  end;
end;

procedure TKOLXPPopupMenu.SetSelectedColor(const Value:TColor);
begin
  if FSelectedColor<>Value then begin
    FSelectedColor:=Value;
    Change;
  end
end;

procedure TKOLXPPopupMenu.SetCheckColor(const Value:TColor);
begin
  if FCheckColor<>Value then begin
    FCheckColor:=Value;
    Change;
  end;  
end;

procedure TKOLXPPopupMenu.SetFont(const Value:TKOLFont);
begin
  FFont:=Value;
  Change;
end;

procedure TKOLXPPopupMenu.SetItemHeight(const Value:integer);
begin
  if FItemHeight<>Value then begin
     FItemHeight:=Value;
     Change;
  end;
end;

procedure TKOLXPPopupMenu.SetItemWidth(const Value:integer);
begin
  if FItemWidth<>Value then begin
     FItemWidth:= Value;
     Change;
  end;
end;



end.
