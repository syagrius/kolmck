unit mckHashs;

interface

uses
  Windows, Messages, Classes, Controls, mirror, mckCtrls, KOL, Graphics;

type

  TKOLHAVAL = class(TKOLObj)
  private

    // fOnMyEvent: TOnMyEvent;
    // procedure SetOnMyEvent(Value: TOnMyEvent);

  protected
    function AdditionalUnits: string; override;
    procedure AssignEvents(SL: TStringList; const AName: string); override;
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: string); override;
    procedure SetupLast(SL: TStringList; const AName, AParent, Prefix: string); override;

  public
    constructor Create(Owner: TComponent); override;
    function TypeName: string; override;

  published

    // property OnMyEvent: TOnMyEvent read fOnMyEvent write SetOnMyEvent;

  end;

  TKOLMD4 = class(TKOLObj)
  private

    // fOnMyEvent: TOnMyEvent;
    // procedure SetOnMyEvent(Value: TOnMyEvent);

  protected
    function AdditionalUnits: string; override;
    procedure AssignEvents(SL: TStringList; const AName: string); override;
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: string); override;
    procedure SetupLast(SL: TStringList; const AName, AParent, Prefix: string); override;

  public
    constructor Create(Owner: TComponent); override;
    function TypeName: string; override;

  published

    // property OnMyEvent: TOnMyEvent read fOnMyEvent write SetOnMyEvent;

  end;

  TKOLMD5 = class(TKOLObj)
  private

    // fOnMyEvent: TOnMyEvent;
    // procedure SetOnMyEvent(Value: TOnMyEvent);

  protected
    function AdditionalUnits: string; override;
    procedure AssignEvents(SL: TStringList; const AName: string); override;
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: string); override;
    procedure SetupLast(SL: TStringList; const AName, AParent, Prefix: string); override;

  public
    constructor Create(Owner: TComponent); override;
    function TypeName: string; override;

  published

    // property OnMyEvent: TOnMyEvent read fOnMyEvent write SetOnMyEvent;

  end;

  TKOLRMD128 = class(TKOLObj)
  private

    // fOnMyEvent: TOnMyEvent;
    // procedure SetOnMyEvent(Value: TOnMyEvent);

  protected
    function AdditionalUnits: string; override;
    procedure AssignEvents(SL: TStringList; const AName: string); override;
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: string); override;
    procedure SetupLast(SL: TStringList; const AName, AParent, Prefix: string); override;

  public
    constructor Create(Owner: TComponent); override;
    function TypeName: string; override;

  published

    // property OnMyEvent: TOnMyEvent read fOnMyEvent write SetOnMyEvent;

  end;

  TKOLRMD160 = class(TKOLObj)
  private

    // fOnMyEvent: TOnMyEvent;
    // procedure SetOnMyEvent(Value: TOnMyEvent);

  protected
    function AdditionalUnits: string; override;
    procedure AssignEvents(SL: TStringList; const AName: string); override;
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: string); override;
    procedure SetupLast(SL: TStringList; const AName, AParent, Prefix: string); override;

  public
    constructor Create(Owner: TComponent); override;
    function TypeName: string; override;

  published

    // property OnMyEvent: TOnMyEvent read fOnMyEvent write SetOnMyEvent;

  end;

  TKOLSHA1 = class(TKOLObj)
  private

    // fOnMyEvent: TOnMyEvent;
    // procedure SetOnMyEvent(Value: TOnMyEvent);

  protected
    function AdditionalUnits: string; override;
    procedure AssignEvents(SL: TStringList; const AName: string); override;
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: string); override;
    procedure SetupLast(SL: TStringList; const AName, AParent, Prefix: string); override;

  public
    constructor Create(Owner: TComponent); override;
    function TypeName: string; override;

  published

    // property OnMyEvent: TOnMyEvent read fOnMyEvent write SetOnMyEvent;

  end;

  TKOLSHA256 = class(TKOLObj)
  private

    // fOnMyEvent: TOnMyEvent;
    // procedure SetOnMyEvent(Value: TOnMyEvent);

  protected
    function AdditionalUnits: string; override;
    procedure AssignEvents(SL: TStringList; const AName: string); override;
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: string); override;
    procedure SetupLast(SL: TStringList; const AName, AParent, Prefix: string); override;

  public
    constructor Create(Owner: TComponent); override;
    function TypeName: string; override;

  published

    // property OnMyEvent: TOnMyEvent read fOnMyEvent write SetOnMyEvent;

  end;

  TKOLSHA384 = class(TKOLObj)
  private

    // fOnMyEvent: TOnMyEvent;
    // procedure SetOnMyEvent(Value: TOnMyEvent);

  protected
    function AdditionalUnits: string; override;
    procedure AssignEvents(SL: TStringList; const AName: string); override;
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: string); override;
    procedure SetupLast(SL: TStringList; const AName, AParent, Prefix: string); override;

  public
    constructor Create(Owner: TComponent); override;
    function TypeName: string; override;

  published

    // property OnMyEvent: TOnMyEvent read fOnMyEvent write SetOnMyEvent;

  end;

  TKOLSHA512 = class(TKOLObj)
  private

    // fOnMyEvent: TOnMyEvent;
    // procedure SetOnMyEvent(Value: TOnMyEvent);

  protected
    function AdditionalUnits: string; override;
    procedure AssignEvents(SL: TStringList; const AName: string); override;
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: string); override;
    procedure SetupLast(SL: TStringList; const AName, AParent, Prefix: string); override;

  public
    constructor Create(Owner: TComponent); override;
    function TypeName: string; override;

  published

    // property OnMyEvent: TOnMyEvent read fOnMyEvent write SetOnMyEvent;

  end;

  TKOLTIGER = class(TKOLObj)
  private

    // fOnMyEvent: TOnMyEvent;
    // procedure SetOnMyEvent(Value: TOnMyEvent);

  protected
    function AdditionalUnits: string; override;
    procedure AssignEvents(SL: TStringList; const AName: string); override;
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: string); override;
    procedure SetupLast(SL: TStringList; const AName, AParent, Prefix: string); override;

  public
    constructor Create(Owner: TComponent); override;
    function TypeName: string; override;

  published

    // property OnMyEvent: TOnMyEvent read fOnMyEvent write SetOnMyEvent;

  end;

procedure Register;

{$R *.dcr}

implementation

procedure Register;
begin
  RegisterComponents('KOL HASHES', [TKOLHAVAL, TKOLMD4, TKOLMD5, TKOLRMD128,
    TKOLRMD160, TKOLSHA1, TKOLSHA256, TKOLSHA384, TKOLSHA512, TKOLTIGER]);
end;

{ днаюбкемхе лндскъ }

function TKOLHAVAL.AdditionalUnits;
begin
  Result := ', KOLHashs';
end;

function TKOLHAVAL.TypeName: string;
begin
  Result := 'TKOLHAVAL';
end;
////////////////////////////////////////////////////////////////////////////////

{--------------------------}
{ пецхярпюжхъ напюанрвхйнб }
{--------------------------}

procedure TKOLHAVAL.AssignEvents;
begin
  inherited;
  // DoAssignEvents(SL, AName, ['OnMyEvent'], [@OnMyEvent]);
  // DoAssignEvents(SL, AName, ['OnEvent1', 'OnEvent2'], [@OnEvent1, @OnEvent2]);
end;

{--------------------------}
{ днаюбкемхе б unitX_X.inc }
{--------------------------}

procedure TKOLHAVAL.SetupFirst;
//const
// spc = ', ';
// Boolean2Str: array [Boolean] of String = ('FALSE', 'TRUE');

begin
  SL.Add(Prefix + AName + ' := NewHAVAL;');

  // Boolean2Str[TRUE]
  // Color2Str(myColor)
  // SL.Add(Prefix + AName + '.myStr := ''' + myStr + ''';');
end;

{--------------------------}
{ днаюбкемхе б unitX_X.inc }
{--------------------------}

procedure TKOLHAVAL.SetupLast;
begin
  // SL.Add(Prefix + AName + '.myInt := ' + Int2Str(myInt) + ';');
end;
////////////////////////////////////////////////////////////////////////////////

{-------------}
{ йнмярпсйрнп }
{-------------}

constructor TKOLHAVAL.Create;
begin
  inherited;

  // fmyInt := 10;
end;

{ procedure TKOLHAVAL.SetOnMyEvent;
begin
fOnMyEvent := Value;
Change;
end; }

{ днаюбкемхе лндскъ }

function TKOLMD4.AdditionalUnits;
begin
  Result := ', KOLHashs';
end;

function TKOLMD4.TypeName: string;
begin
  Result := 'TKOLMD4';
end;
////////////////////////////////////////////////////////////////////////////////

{--------------------------}
{ пецхярпюжхъ напюанрвхйнб }
{--------------------------}

procedure TKOLMD4.AssignEvents;
begin
  inherited;
  // DoAssignEvents(SL, AName, ['OnMyEvent'], [@OnMyEvent]);
  // DoAssignEvents(SL, AName, ['OnEvent1', 'OnEvent2'], [@OnEvent1, @OnEvent2]);
end;

{--------------------------}
{ днаюбкемхе б unitX_X.inc }
{--------------------------}

procedure TKOLMD4.SetupFirst;
//const
// spc = ', ';
// Boolean2Str: array [Boolean] of String = ('FALSE', 'TRUE');

begin
  SL.Add(Prefix + AName + ' := NewMD4;');

  // Boolean2Str[TRUE]
  // Color2Str(myColor)
  // SL.Add(Prefix + AName + '.myStr := ''' + myStr + ''';');
end;

{--------------------------}
{ днаюбкемхе б unitX_X.inc }
{--------------------------}

procedure TKOLMD4.SetupLast;
begin
  // SL.Add(Prefix + AName + '.myInt := ' + Int2Str(myInt) + ';');
end;
////////////////////////////////////////////////////////////////////////////////

{-------------}
{ йнмярпсйрнп }
{-------------}

constructor TKOLMD4.Create;
begin
  inherited;

  // fmyInt := 10;
end;

{ procedure TKOLMD4.SetOnMyEvent;
begin
fOnMyEvent := Value;
Change;
end; }

{ днаюбкемхе лндскъ }

function TKOLMD5.AdditionalUnits;
begin
  Result := ', KOLHashs';
end;

function TKOLMD5.TypeName: string;
begin
  Result := 'TKOLMD5';
end;
////////////////////////////////////////////////////////////////////////////////

{--------------------------}
{ пецхярпюжхъ напюанрвхйнб }
{--------------------------}

procedure TKOLMD5.AssignEvents;
begin
  inherited;
  // DoAssignEvents(SL, AName, ['OnMyEvent'], [@OnMyEvent]);
  // DoAssignEvents(SL, AName, ['OnEvent1', 'OnEvent2'], [@OnEvent1, @OnEvent2]);
end;

{--------------------------}
{ днаюбкемхе б unitX_X.inc }
{--------------------------}

procedure TKOLMD5.SetupFirst;
//const
// spc = ', ';
// Boolean2Str: array [Boolean] of String = ('FALSE', 'TRUE');

begin
  SL.Add(Prefix + AName + ' := NewMD5;');

  // Boolean2Str[TRUE]
  // Color2Str(myColor)
  // SL.Add(Prefix + AName + '.myStr := ''' + myStr + ''';');
end;

{--------------------------}
{ днаюбкемхе б unitX_X.inc }
{--------------------------}

procedure TKOLMD5.SetupLast;
begin
  // SL.Add(Prefix + AName + '.myInt := ' + Int2Str(myInt) + ';');
end;
////////////////////////////////////////////////////////////////////////////////

{-------------}
{ йнмярпсйрнп }
{-------------}

constructor TKOLMD5.Create;
begin
  inherited;

  // fmyInt := 10;
end;

{ procedure TKOLMD5.SetOnMyEvent;
begin
fOnMyEvent := Value;
Change;
end; }

{ днаюбкемхе лндскъ }

function TKOLRMD128.AdditionalUnits;
begin
  Result := ', KOLHashs';
end;

function TKOLRMD128.TypeName: string;
begin
  Result := 'TKOLRMD128';
end;
////////////////////////////////////////////////////////////////////////////////

{--------------------------}
{ пецхярпюжхъ напюанрвхйнб }
{--------------------------}

procedure TKOLRMD128.AssignEvents;
begin
  inherited;
  // DoAssignEvents(SL, AName, ['OnMyEvent'], [@OnMyEvent]);
  // DoAssignEvents(SL, AName, ['OnEvent1', 'OnEvent2'], [@OnEvent1, @OnEvent2]);
end;

{--------------------------}
{ днаюбкемхе б unitX_X.inc }
{--------------------------}

procedure TKOLRMD128.SetupFirst;
//const
// spc = ', ';
// Boolean2Str: array [Boolean] of String = ('FALSE', 'TRUE');

begin
  SL.Add(Prefix + AName + ' := NewRMD128;');

  // Boolean2Str[TRUE]
  // Color2Str(myColor)
  // SL.Add(Prefix + AName + '.myStr := ''' + myStr + ''';');
end;

{--------------------------}
{ днаюбкемхе б unitX_X.inc }
{--------------------------}

procedure TKOLRMD128.SetupLast;
begin
  // SL.Add(Prefix + AName + '.myInt := ' + Int2Str(myInt) + ';');
end;
////////////////////////////////////////////////////////////////////////////////

{-------------}
{ йнмярпсйрнп }
{-------------}

constructor TKOLRMD128.Create;
begin
  inherited;

  // fmyInt := 10;
end;

{ procedure TKOLRMD128.SetOnMyEvent;
begin
fOnMyEvent := Value;
Change;
end; }

{ днаюбкемхе лндскъ }

function TKOLRMD160.AdditionalUnits;
begin
  Result := ', KOLHashs';
end;

function TKOLRMD160.TypeName: string;
begin
  Result := 'TKOLRMD160';
end;
////////////////////////////////////////////////////////////////////////////////

{--------------------------}
{ пецхярпюжхъ напюанрвхйнб }
{--------------------------}

procedure TKOLRMD160.AssignEvents;
begin
  inherited;
  // DoAssignEvents(SL, AName, ['OnMyEvent'], [@OnMyEvent]);
  // DoAssignEvents(SL, AName, ['OnEvent1', 'OnEvent2'], [@OnEvent1, @OnEvent2]);
end;

{--------------------------}
{ днаюбкемхе б unitX_X.inc }
{--------------------------}

procedure TKOLRMD160.SetupFirst;
//const
// spc = ', ';
// Boolean2Str: array [Boolean] of String = ('FALSE', 'TRUE');

begin
  SL.Add(Prefix + AName + ' := NewRMD160;');

  // Boolean2Str[TRUE]
  // Color2Str(myColor)
  // SL.Add(Prefix + AName + '.myStr := ''' + myStr + ''';');
end;

{--------------------------}
{ днаюбкемхе б unitX_X.inc }
{--------------------------}

procedure TKOLRMD160.SetupLast;
begin
  // SL.Add(Prefix + AName + '.myInt := ' + Int2Str(myInt) + ';');
end;
////////////////////////////////////////////////////////////////////////////////

{-------------}
{ йнмярпсйрнп }
{-------------}

constructor TKOLRMD160.Create;
begin
  inherited;

  // fmyInt := 10;
end;

{ procedure TKOLRMD160.SetOnMyEvent;
begin
fOnMyEvent := Value;
Change;
end; }

{ днаюбкемхе лндскъ }

function TKOLSHA1.AdditionalUnits;
begin
  Result := ', KOLHashs';
end;

function TKOLSHA1.TypeName: string;
begin
  Result := 'TKOLSHA1';
end;
////////////////////////////////////////////////////////////////////////////////

{--------------------------}
{ пецхярпюжхъ напюанрвхйнб }
{--------------------------}

procedure TKOLSHA1.AssignEvents;
begin
  inherited;
  // DoAssignEvents(SL, AName, ['OnMyEvent'], [@OnMyEvent]);
  // DoAssignEvents(SL, AName, ['OnEvent1', 'OnEvent2'], [@OnEvent1, @OnEvent2]);
end;

{--------------------------}
{ днаюбкемхе б unitX_X.inc }
{--------------------------}

procedure TKOLSHA1.SetupFirst;
//const
// spc = ', ';
// Boolean2Str: array [Boolean] of String = ('FALSE', 'TRUE');

begin
  SL.Add(Prefix + AName + ' := NewSHA1;');

  // Boolean2Str[TRUE]
  // Color2Str(myColor)
  // SL.Add(Prefix + AName + '.myStr := ''' + myStr + ''';');
end;

{--------------------------}
{ днаюбкемхе б unitX_X.inc }
{--------------------------}

procedure TKOLSHA1.SetupLast;
begin
  // SL.Add(Prefix + AName + '.myInt := ' + Int2Str(myInt) + ';');
end;
////////////////////////////////////////////////////////////////////////////////

{-------------}
{ йнмярпсйрнп }
{-------------}

constructor TKOLSHA1.Create;
begin
  inherited;

  // fmyInt := 10;
end;

{ procedure TKOLSHA1.SetOnMyEvent;
begin
fOnMyEvent := Value;
Change;
end; }

{ днаюбкемхе лндскъ }

function TKOLSHA256.AdditionalUnits;
begin
  Result := ', KOLHashs';
end;

function TKOLSHA256.TypeName: string;
begin
  Result := 'TKOLSHA256';
end;
////////////////////////////////////////////////////////////////////////////////

{--------------------------}
{ пецхярпюжхъ напюанрвхйнб }
{--------------------------}

procedure TKOLSHA256.AssignEvents;
begin
  inherited;
  // DoAssignEvents(SL, AName, ['OnMyEvent'], [@OnMyEvent]);
  // DoAssignEvents(SL, AName, ['OnEvent1', 'OnEvent2'], [@OnEvent1, @OnEvent2]);
end;

{--------------------------}
{ днаюбкемхе б unitX_X.inc }
{--------------------------}

procedure TKOLSHA256.SetupFirst;
//const
// spc = ', ';
// Boolean2Str: array [Boolean] of String = ('FALSE', 'TRUE');

begin
  SL.Add(Prefix + AName + ' := NewSHA256;');

  // Boolean2Str[TRUE]
  // Color2Str(myColor)
  // SL.Add(Prefix + AName + '.myStr := ''' + myStr + ''';');
end;

{--------------------------}
{ днаюбкемхе б unitX_X.inc }
{--------------------------}

procedure TKOLSHA256.SetupLast;
begin
  // SL.Add(Prefix + AName + '.myInt := ' + Int2Str(myInt) + ';');
end;
////////////////////////////////////////////////////////////////////////////////

{-------------}
{ йнмярпсйрнп }
{-------------}

constructor TKOLSHA256.Create;
begin
  inherited;

  // fmyInt := 10;
end;

{ procedure TKOLSHA256.SetOnMyEvent;
begin
fOnMyEvent := Value;
Change;
end; }

{ днаюбкемхе лндскъ }

function TKOLSHA384.AdditionalUnits;
begin
  Result := ', KOLHashs';
end;

function TKOLSHA384.TypeName: string;
begin
  Result := 'TKOLSHA384';
end;
////////////////////////////////////////////////////////////////////////////////

{--------------------------}
{ пецхярпюжхъ напюанрвхйнб }
{--------------------------}

procedure TKOLSHA384.AssignEvents;
begin
  inherited;
  // DoAssignEvents(SL, AName, ['OnMyEvent'], [@OnMyEvent]);
  // DoAssignEvents(SL, AName, ['OnEvent1', 'OnEvent2'], [@OnEvent1, @OnEvent2]);
end;

{--------------------------}
{ днаюбкемхе б unitX_X.inc }
{--------------------------}

procedure TKOLSHA384.SetupFirst;

begin
  SL.Add(Prefix + AName + ' := NewSHA384;');

  // Boolean2Str[TRUE]
  // Color2Str(myColor)
  // SL.Add(Prefix + AName + '.myStr := ''' + myStr + ''';');
end;

{--------------------------}
{ днаюбкемхе б unitX_X.inc }
{--------------------------}

procedure TKOLSHA384.SetupLast;
begin
  // SL.Add(Prefix + AName + '.myInt := ' + Int2Str(myInt) + ';');
end;
////////////////////////////////////////////////////////////////////////////////

{-------------}
{ йнмярпсйрнп }
{-------------}

constructor TKOLSHA384.Create;
begin
  inherited;

  // fmyInt := 10;
end;

{ procedure TKOLSHA384.SetOnMyEvent;
begin
fOnMyEvent := Value;
Change;
end; }

{ днаюбкемхе лндскъ }

function TKOLSHA512.AdditionalUnits;
begin
  Result := ', KOLHashs';
end;

function TKOLSHA512.TypeName: string;
begin
  Result := 'TKOLSHA512';
end;
////////////////////////////////////////////////////////////////////////////////

{--------------------------}
{ пецхярпюжхъ напюанрвхйнб }
{--------------------------}

procedure TKOLSHA512.AssignEvents;
begin
  inherited;
  // DoAssignEvents(SL, AName, ['OnMyEvent'], [@OnMyEvent]);
  // DoAssignEvents(SL, AName, ['OnEvent1', 'OnEvent2'], [@OnEvent1, @OnEvent2]);
end;

{--------------------------}
{ днаюбкемхе б unitX_X.inc }
{--------------------------}

procedure TKOLSHA512.SetupFirst;
//const
// spc = ', ';
// Boolean2Str: array [Boolean] of String = ('FALSE', 'TRUE');

begin
  SL.Add(Prefix + AName + ' := NewSHA512;');

  // Boolean2Str[TRUE]
  // Color2Str(myColor)
  // SL.Add(Prefix + AName + '.myStr := ''' + myStr + ''';');
end;

{--------------------------}
{ днаюбкемхе б unitX_X.inc }
{--------------------------}

procedure TKOLSHA512.SetupLast;
begin
  // SL.Add(Prefix + AName + '.myInt := ' + Int2Str(myInt) + ';');
end;
////////////////////////////////////////////////////////////////////////////////

{-------------}
{ йнмярпсйрнп }
{-------------}

constructor TKOLSHA512.Create;
begin
  inherited;

  // fmyInt := 10;
end;

{ procedure TKOLSHA512.SetOnMyEvent;
begin
fOnMyEvent := Value;
Change;
end; }

{ днаюбкемхе лндскъ }

function TKOLTIGER.AdditionalUnits;
begin
  Result := ', KOLHashs';
end;

function TKOLTIGER.TypeName: string;
begin
  Result := 'TKOLTIGER';
end;
////////////////////////////////////////////////////////////////////////////////

{--------------------------}
{ пецхярпюжхъ напюанрвхйнб }
{--------------------------}

procedure TKOLTIGER.AssignEvents;
begin
  inherited;
  // DoAssignEvents(SL, AName, ['OnMyEvent'], [@OnMyEvent]);
  // DoAssignEvents(SL, AName, ['OnEvent1', 'OnEvent2'], [@OnEvent1, @OnEvent2]);
end;

{--------------------------}
{ днаюбкемхе б unitX_X.inc }
{--------------------------}

procedure TKOLTIGER.SetupFirst;
//const
// spc = ', ';
// Boolean2Str: array [Boolean] of String = ('FALSE', 'TRUE');

begin
  SL.Add(Prefix + AName + ' := NewTIGER;');

  // Boolean2Str[TRUE]
  // Color2Str(myColor)
  // SL.Add(Prefix + AName + '.myStr := ''' + myStr + ''';');
end;

{--------------------------}
{ днаюбкемхе б unitX_X.inc }
{--------------------------}

procedure TKOLTIGER.SetupLast;
begin
  // SL.Add(Prefix + AName + '.myInt := ' + Int2Str(myInt) + ';');
end;
////////////////////////////////////////////////////////////////////////////////

{-------------}
{ йнмярпсйрнп }
{-------------}

constructor TKOLTIGER.Create;
begin
  inherited;

  // fmyInt := 10;
end;

{ procedure TKOLTIGER.SetOnMyEvent;
begin
fOnMyEvent := Value;
Change;
end; }

end.

