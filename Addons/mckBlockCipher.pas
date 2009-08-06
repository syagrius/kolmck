unit mckBlockCipher;

interface

uses
  Windows, Messages, Classes, Controls, mirror, mckCtrls, KOL, Graphics, KOLBlockCipher;

type

  TKOLTEA = class(TKOLObj)
  private

    // fOnMyEvent: TOnMyEvent;
    // procedure SetOnMyEvent(Value: TOnMyEvent);

  protected
    function AdditionalUnits: String; override;
    procedure AssignEvents(SL: TStringList; const AName: String); override;
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: String); override;
    procedure SetupLast(SL: TStringList; const AName, AParent, Prefix: String); override;

  public
    constructor Create(Owner: TComponent); override;
    function TypeName: String; override;

  published

    // property OnMyEvent: TOnMyEvent read fOnMyEvent write SetOnMyEvent;

  end;

  TKOLDES = class(TKOLObj)
  private

    // fOnMyEvent: TOnMyEvent;
    // procedure SetOnMyEvent(Value: TOnMyEvent);

  protected
    function AdditionalUnits: String; override;
    procedure AssignEvents(SL: TStringList; const AName: String); override;
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: String); override;
    procedure SetupLast(SL: TStringList; const AName, AParent, Prefix: String); override;

  public
    constructor Create(Owner: TComponent); override;
    function TypeName: String; override;

  published

    // property OnMyEvent: TOnMyEvent read fOnMyEvent write SetOnMyEvent;

  end;

  TKOL3DES = class(TKOLObj)
  private

    // fOnMyEvent: TOnMyEvent;
    // procedure SetOnMyEvent(Value: TOnMyEvent);

  protected
    function AdditionalUnits: String; override;
    procedure AssignEvents(SL: TStringList; const AName: String); override;
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: String); override;
    procedure SetupLast(SL: TStringList; const AName, AParent, Prefix: String); override;

  public
    constructor Create(Owner: TComponent); override;
    function TypeName: String; override;

  published

    // property OnMyEvent: TOnMyEvent read fOnMyEvent write SetOnMyEvent;

  end;

  TKOLICE = class(TKOLObj)
  private

    // fOnMyEvent: TOnMyEvent;
    // procedure SetOnMyEvent(Value: TOnMyEvent);

  protected
    function AdditionalUnits: String; override;
    procedure AssignEvents(SL: TStringList; const AName: String); override;
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: String); override;
    procedure SetupLast(SL: TStringList; const AName, AParent, Prefix: String); override;

  public
    constructor Create(Owner: TComponent); override;
    function TypeName: String; override;

  published

    // property OnMyEvent: TOnMyEvent read fOnMyEvent write SetOnMyEvent;

  end;

  TKOLICE2 = class(TKOLObj)
  private

    // fOnMyEvent: TOnMyEvent;
    // procedure SetOnMyEvent(Value: TOnMyEvent);

  protected
    function AdditionalUnits: String; override;
    procedure AssignEvents(SL: TStringList; const AName: String); override;
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: String); override;
    procedure SetupLast(SL: TStringList; const AName, AParent, Prefix: String); override;

  public
    constructor Create(Owner: TComponent); override;
    function TypeName: String; override;

  published

    // property OnMyEvent: TOnMyEvent read fOnMyEvent write SetOnMyEvent;

  end;

  TKOLThinICE = class(TKOLObj)
  private

    // fOnMyEvent: TOnMyEvent;
    // procedure SetOnMyEvent(Value: TOnMyEvent);

  protected
    function AdditionalUnits: String; override;
    procedure AssignEvents(SL: TStringList; const AName: String); override;
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: String); override;
    procedure SetupLast(SL: TStringList; const AName, AParent, Prefix: String); override;

  public
    constructor Create(Owner: TComponent); override;
    function TypeName: String; override;

  published

    // property OnMyEvent: TOnMyEvent read fOnMyEvent write SetOnMyEvent;

  end;

  TKOLRC2 = class(TKOLObj)
  private

    // fOnMyEvent: TOnMyEvent;
    // procedure SetOnMyEvent(Value: TOnMyEvent);

  protected
    function AdditionalUnits: String; override;
    procedure AssignEvents(SL: TStringList; const AName: String); override;
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: String); override;
    procedure SetupLast(SL: TStringList; const AName, AParent, Prefix: String); override;

  public
    constructor Create(Owner: TComponent); override;
    function TypeName: String; override;

  published

    // property OnMyEvent: TOnMyEvent read fOnMyEvent write SetOnMyEvent;

  end;

  TKOLRC4 = class(TKOLObj)
  private

    // fOnMyEvent: TOnMyEvent;
    // procedure SetOnMyEvent(Value: TOnMyEvent);

  protected
    function AdditionalUnits: String; override;
    procedure AssignEvents(SL: TStringList; const AName: String); override;
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: String); override;
    procedure SetupLast(SL: TStringList; const AName, AParent, Prefix: String); override;

  public
    constructor Create(Owner: TComponent); override;
    function TypeName: String; override;

  published

    // property OnMyEvent: TOnMyEvent read fOnMyEvent write SetOnMyEvent;

  end;

  TKOLRC5 = class(TKOLObj)
  private

    // fOnMyEvent: TOnMyEvent;
    // procedure SetOnMyEvent(Value: TOnMyEvent);

  protected
    function AdditionalUnits: String; override;
    procedure AssignEvents(SL: TStringList; const AName: String); override;
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: String); override;
    procedure SetupLast(SL: TStringList; const AName, AParent, Prefix: String); override;

  public
    constructor Create(Owner: TComponent); override;
    function TypeName: String; override;

  published

    // property OnMyEvent: TOnMyEvent read fOnMyEvent write SetOnMyEvent;

  end;

  TKOLMisty1 = class(TKOLObj)
  private

    // fOnMyEvent: TOnMyEvent;
    // procedure SetOnMyEvent(Value: TOnMyEvent);

  protected
    function AdditionalUnits: String; override;
    procedure AssignEvents(SL: TStringList; const AName: String); override;
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: String); override;
    procedure SetupLast(SL: TStringList; const AName, AParent, Prefix: String); override;

  public
    constructor Create(Owner: TComponent); override;
    function TypeName: String; override;

  published

    // property OnMyEvent: TOnMyEvent read fOnMyEvent write SetOnMyEvent;

  end;

  TKOLIDEA = class(TKOLObj)
  private

    // fOnMyEvent: TOnMyEvent;
    // procedure SetOnMyEvent(Value: TOnMyEvent);

  protected
    function AdditionalUnits: String; override;
    procedure AssignEvents(SL: TStringList; const AName: String); override;
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: String); override;
    procedure SetupLast(SL: TStringList; const AName, AParent, Prefix: String); override;

  public
    constructor Create(Owner: TComponent); override;
    function TypeName: String; override;

  published

    // property OnMyEvent: TOnMyEvent read fOnMyEvent write SetOnMyEvent;

  end;

  TKOLGOST = class(TKOLObj)
  private

    // fOnMyEvent: TOnMyEvent;
    // procedure SetOnMyEvent(Value: TOnMyEvent);

  protected
    function AdditionalUnits: String; override;
    procedure AssignEvents(SL: TStringList; const AName: String); override;
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: String); override;
    procedure SetupLast(SL: TStringList; const AName, AParent, Prefix: String); override;

  public
    constructor Create(Owner: TComponent); override;
    function TypeName: String; override;

  published

    // property OnMyEvent: TOnMyEvent read fOnMyEvent write SetOnMyEvent;

  end;

  TKOLCast128 = class(TKOLObj)
  private

    // fOnMyEvent: TOnMyEvent;
    // procedure SetOnMyEvent(Value: TOnMyEvent);

  protected
    function AdditionalUnits: String; override;
    procedure AssignEvents(SL: TStringList; const AName: String); override;
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: String); override;
    procedure SetupLast(SL: TStringList; const AName, AParent, Prefix: String); override;

  public
    constructor Create(Owner: TComponent); override;
    function TypeName: String; override;

  published

    // property OnMyEvent: TOnMyEvent read fOnMyEvent write SetOnMyEvent;

  end;

  TKOLBlowfish = class(TKOLObj)
  private

    // fOnMyEvent: TOnMyEvent;
    // procedure SetOnMyEvent(Value: TOnMyEvent);

  protected
    function AdditionalUnits: String; override;
    procedure AssignEvents(SL: TStringList; const AName: String); override;
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: String); override;
    procedure SetupLast(SL: TStringList; const AName, AParent, Prefix: String); override;

  public
    constructor Create(Owner: TComponent); override;
    function TypeName: String; override;

  published

    // property OnMyEvent: TOnMyEvent read fOnMyEvent write SetOnMyEvent;

  end;

  TKOLTwoFish = class(TKOLObj)
  private

    // fOnMyEvent: TOnMyEvent;
    // procedure SetOnMyEvent(Value: TOnMyEvent);

  protected
    function AdditionalUnits: String; override;
    procedure AssignEvents(SL: TStringList; const AName: String); override;
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: String); override;
    procedure SetupLast(SL: TStringList; const AName, AParent, Prefix: String); override;

  public
    constructor Create(Owner: TComponent); override;
    function TypeName: String; override;

  published

    // property OnMyEvent: TOnMyEvent read fOnMyEvent write SetOnMyEvent;

  end;

  TKOLSerpent = class(TKOLObj)
  private

    // fOnMyEvent: TOnMyEvent;
    // procedure SetOnMyEvent(Value: TOnMyEvent);

  protected
    function AdditionalUnits: String; override;
    procedure AssignEvents(SL: TStringList; const AName: String); override;
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: String); override;
    procedure SetupLast(SL: TStringList; const AName, AParent, Prefix: String); override;

  public
    constructor Create(Owner: TComponent); override;
    function TypeName: String; override;

  published

    // property OnMyEvent: TOnMyEvent read fOnMyEvent write SetOnMyEvent;

  end;

  TKOLRijndael = class(TKOLObj)
  private

    // fOnMyEvent: TOnMyEvent;
    // procedure SetOnMyEvent(Value: TOnMyEvent);

  protected
    function AdditionalUnits: String; override;
    procedure AssignEvents(SL: TStringList; const AName: String); override;
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: String); override;
    procedure SetupLast(SL: TStringList; const AName, AParent, Prefix: String); override;

  public
    constructor Create(Owner: TComponent); override;
    function TypeName: String; override;

  published

    // property OnMyEvent: TOnMyEvent read fOnMyEvent write SetOnMyEvent;

  end;

  TKOLRC6 = class(TKOLObj)
  private

    // fOnMyEvent: TOnMyEvent;
    // procedure SetOnMyEvent(Value: TOnMyEvent);

  protected
    function AdditionalUnits: String; override;
    procedure AssignEvents(SL: TStringList; const AName: String); override;
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: String); override;
    procedure SetupLast(SL: TStringList; const AName, AParent, Prefix: String); override;

  public
    constructor Create(Owner: TComponent); override;
    function TypeName: String; override;

  published

    // property OnMyEvent: TOnMyEvent read fOnMyEvent write SetOnMyEvent;

  end;

  TKOLMars = class(TKOLObj)
  private

    // fOnMyEvent: TOnMyEvent;
    // procedure SetOnMyEvent(Value: TOnMyEvent);

  protected
    function AdditionalUnits: String; override;
    procedure AssignEvents(SL: TStringList; const AName: String); override;
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: String); override;
    procedure SetupLast(SL: TStringList; const AName, AParent, Prefix: String); override;

  public
    constructor Create(Owner: TComponent); override;
    function TypeName: String; override;

  published

    // property OnMyEvent: TOnMyEvent read fOnMyEvent write SetOnMyEvent;

  end;

  TKOLCast256 = class(TKOLObj)
  private

    // fOnMyEvent: TOnMyEvent;
    // procedure SetOnMyEvent(Value: TOnMyEvent);

  protected
    function AdditionalUnits: String; override;
    procedure AssignEvents(SL: TStringList; const AName: String); override;
    procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: String); override;
    procedure SetupLast(SL: TStringList; const AName, AParent, Prefix: String); override;

  public
    constructor Create(Owner: TComponent); override;
    function TypeName: String; override;

  published

    // property OnMyEvent: TOnMyEvent read fOnMyEvent write SetOnMyEvent;

  end;

procedure Register;

{$R *.dcr}

implementation

procedure Register;
begin
RegisterComponents('KOL Ciphers', [TKOLTEA,TKOLRC5,TKOLRC2,TKOLMisty1,TKOLIDEA,
TKOLGOST,TKOLCast128,TKOLBlowfish,TKOLTwoFish,TKOLSerpent,TKOLRijndael,TKOLRC6,
TKOLMars,TKOLCast256,TKOLICE,TKOLICE2,TKOLThinICE,TKOLDES,TKOL3DES,TKOLRC4]);
end;

//========================================================
function TKOLTEA.AdditionalUnits;
begin
Result := ', KOLBlockCipher';
end;

function TKOLTEA.TypeName: String;
begin
Result := 'TKOLTEA';
end;

procedure TKOLTEA.AssignEvents;
begin
inherited;
end;

procedure TKOLTEA.SetupFirst;

begin
 SL.Add(Prefix + AName + ' := NewTEA;');
end;

procedure TKOLTEA.SetupLast;
begin
end;

constructor TKOLTEA.Create;
begin
 inherited;
end;


//========================================================
function TKOLRC5.AdditionalUnits;
begin
Result := ', KOLBlockCipher';
end;

function TKOLRC5.TypeName: String;
begin
Result := 'TKOLRC5';
end;

procedure TKOLRC5.AssignEvents;
begin
inherited;
end;

procedure TKOLRC5.SetupFirst;

begin
 SL.Add(Prefix + AName + ' := NewRC5;');
end;

procedure TKOLRC5.SetupLast;
begin
end;

constructor TKOLRC5.Create;
begin
 inherited;
end;


//========================================================
function TKOLRC2.AdditionalUnits;
begin
Result := ', KOLBlockCipher';
end;

function TKOLRC2.TypeName: String;
begin
Result := 'TKOLRC2';
end;

procedure TKOLRC2.AssignEvents;
begin
inherited;
end;

procedure TKOLRC2.SetupFirst;

begin
 SL.Add(Prefix + AName + ' := NewRC2;');
end;

procedure TKOLRC2.SetupLast;
begin
end;

constructor TKOLRC2.Create;
begin
 inherited;
end;

//========================================================
function TKOLRC4.AdditionalUnits;
begin
Result := ', KOLBlockCipher';
end;

function TKOLRC4.TypeName: String;
begin
Result := 'TKOLRC4';
end;

procedure TKOLRC4.AssignEvents;
begin
inherited;
end;

procedure TKOLRC4.SetupFirst;

begin
 SL.Add(Prefix + AName + ' := NewRC4;');
end;

procedure TKOLRC4.SetupLast;
begin
end;

constructor TKOLRC4.Create;
begin
 inherited;
end;


//========================================================
function TKOLRC6.AdditionalUnits;
begin
Result := ', KOLBlockCipher';
end;

function TKOLRC6.TypeName: String;
begin
Result := 'TKOLRC6';
end;

procedure TKOLRC6.AssignEvents;
begin
inherited;
end;

procedure TKOLRC6.SetupFirst;

begin
 SL.Add(Prefix + AName + ' := NewRC6;');
end;

procedure TKOLRC6.SetupLast;
begin
end;

constructor TKOLRC6.Create;
begin
 inherited;
end;


//========================================================
function TKOLMisty1.AdditionalUnits;
begin
Result := ', KOLBlockCipher';
end;

function TKOLMisty1.TypeName: String;
begin
Result := 'TKOLMisty1';
end;

procedure TKOLMisty1.AssignEvents;
begin
inherited;
end;

procedure TKOLMisty1.SetupFirst;

begin
 SL.Add(Prefix + AName + ' := NewMisty1;');
end;

procedure TKOLMisty1.SetupLast;
begin
end;

constructor TKOLMisty1.Create;
begin
 inherited;
end;


//========================================================
function TKOLIDEA.AdditionalUnits;
begin
Result := ', KOLBlockCipher';
end;

function TKOLIDEA.TypeName: String;
begin
Result := 'TKOLIDEA';
end;

procedure TKOLIDEA.AssignEvents;
begin
inherited;
end;

procedure TKOLIDEA.SetupFirst;

begin
 SL.Add(Prefix + AName + ' := NewIDEA;');
end;

procedure TKOLIDEA.SetupLast;
begin
end;

constructor TKOLIDEA.Create;
begin
 inherited;
end;


//========================================================
function TKOLGOST.AdditionalUnits;
begin
Result := ', KOLBlockCipher';
end;

function TKOLGOST.TypeName: String;
begin
Result := 'TKOLGOST';
end;

procedure TKOLGOST.AssignEvents;
begin
inherited;
end;

procedure TKOLGOST.SetupFirst;

begin
 SL.Add(Prefix + AName + ' := NewGOST;');
end;

procedure TKOLGOST.SetupLast;
begin
end;

constructor TKOLGOST.Create;
begin
 inherited;
end;


//========================================================
function TKOLCast128.AdditionalUnits;
begin
Result := ', KOLBlockCipher';
end;

function TKOLCast128.TypeName: String;
begin
Result := 'TKOLCast128';
end;

procedure TKOLCast128.AssignEvents;
begin
inherited;
end;

procedure TKOLCast128.SetupFirst;

begin
 SL.Add(Prefix + AName + ' := NewCast128;');
end;

procedure TKOLCast128.SetupLast;
begin
end;

constructor TKOLCast128.Create;
begin
 inherited;
end;


//========================================================
function TKOLBlowfish.AdditionalUnits;
begin
Result := ', KOLBlockCipher';
end;

function TKOLBlowfish.TypeName: String;
begin
Result := 'TKOLBlowfish';
end;

procedure TKOLBlowfish.AssignEvents;
begin
inherited;
end;

procedure TKOLBlowfish.SetupFirst;

begin
 SL.Add(Prefix + AName + ' := NewBlowfish;');
end;

procedure TKOLBlowfish.SetupLast;
begin
end;

constructor TKOLBlowfish.Create;
begin
 inherited;
end;

//========================================================
function TKOLTwoFish.AdditionalUnits;
begin
Result := ', KOLBlockCipher';
end;

function TKOLTwoFish.TypeName: String;
begin
Result := 'TKOLTwoFish';
end;

procedure TKOLTwoFish.AssignEvents;
begin
inherited;
end;

procedure TKOLTwoFish.SetupFirst;

begin
 SL.Add(Prefix + AName + ' := NewTwoFish;');
end;

procedure TKOLTwoFish.SetupLast;
begin
end;

constructor TKOLTwoFish.Create;
begin
 inherited;
end;

//========================================================
function TKOLSerpent.AdditionalUnits;
begin
Result := ', KOLBlockCipher';
end;

function TKOLSerpent.TypeName: String;
begin
Result := 'TKOLSerpent';
end;

procedure TKOLSerpent.AssignEvents;
begin
inherited;
end;

procedure TKOLSerpent.SetupFirst;

begin
 SL.Add(Prefix + AName + ' := NewSerpent;');
end;

procedure TKOLSerpent.SetupLast;
begin
end;

constructor TKOLSerpent.Create;
begin
 inherited;
end;

//========================================================
function TKOLRijndael.AdditionalUnits;
begin
Result := ', KOLBlockCipher';
end;

function TKOLRijndael.TypeName: String;
begin
Result := 'TKOLRijndael';
end;

procedure TKOLRijndael.AssignEvents;
begin
inherited;
end;

procedure TKOLRijndael.SetupFirst;

begin
 SL.Add(Prefix + AName + ' := NewRijndael;');
end;

procedure TKOLRijndael.SetupLast;
begin
end;

constructor TKOLRijndael.Create;
begin
 inherited;
end;

//========================================================
function TKOLMars.AdditionalUnits;
begin
Result := ', KOLBlockCipher';
end;

function TKOLMars.TypeName: String;
begin
Result := 'TKOLMars';
end;

procedure TKOLMars.AssignEvents;
begin
inherited;
end;

procedure TKOLMars.SetupFirst;

begin
 SL.Add(Prefix + AName + ' := NewMars;');
end;

procedure TKOLMars.SetupLast;
begin
end;

constructor TKOLMars.Create;
begin
 inherited;
end;

//========================================================
function TKOLCast256.AdditionalUnits;
begin
Result := ', KOLBlockCipher';
end;

function TKOLCast256.TypeName: String;
begin
Result := 'TKOLCast256';
end;

procedure TKOLCast256.AssignEvents;
begin
inherited;
end;

procedure TKOLCast256.SetupFirst;

begin
 SL.Add(Prefix + AName + ' := NewCast256;');
end;

procedure TKOLCast256.SetupLast;
begin
end;

constructor TKOLCast256.Create;
begin
 inherited;
end;

//========================================================
function TKOLDES.AdditionalUnits;
begin
Result := ', KOLBlockCipher';
end;

function TKOLDES.TypeName: String;
begin
Result := 'TKOLDES';
end;

procedure TKOLDES.AssignEvents;
begin
inherited;
end;

procedure TKOLDES.SetupFirst;

begin
 SL.Add(Prefix + AName + ' := NewDES;');
end;

procedure TKOLDES.SetupLast;
begin
end;

constructor TKOLDES.Create;
begin
 inherited;
end;

//========================================================
function TKOL3DES.AdditionalUnits;
begin
Result := ', KOLBlockCipher';
end;

function TKOL3DES.TypeName: String;
begin
Result := 'TKOL3DES';
end;

procedure TKOL3DES.AssignEvents;
begin
inherited;
end;

procedure TKOL3DES.SetupFirst;

begin
 SL.Add(Prefix + AName + ' := New3DES;');
end;

procedure TKOL3DES.SetupLast;
begin
end;

constructor TKOL3DES.Create;
begin
 inherited;
end;

//========================================================
function TKOLICE.AdditionalUnits;
begin
Result := ', KOLBlockCipher';
end;

function TKOLICE.TypeName: String;
begin
Result := 'TKOLICE';
end;

procedure TKOLICE.AssignEvents;
begin
inherited;
end;

procedure TKOLICE.SetupFirst;

begin
 SL.Add(Prefix + AName + ' := NewICE;');
end;

procedure TKOLICE.SetupLast;
begin
end;

constructor TKOLICE.Create;
begin
 inherited;
end;

//========================================================
function TKOLICE2.AdditionalUnits;
begin
Result := ', KOLBlockCipher';
end;

function TKOLICE2.TypeName: String;
begin
Result := 'TKOLICE2';
end;

procedure TKOLICE2.AssignEvents;
begin
inherited;
end;

procedure TKOLICE2.SetupFirst;

begin
 SL.Add(Prefix + AName + ' := NewICE2;');
end;

procedure TKOLICE2.SetupLast;
begin
end;

constructor TKOLICE2.Create;
begin
 inherited;
end;

//========================================================
function TKOLThinICE.AdditionalUnits;
begin
Result := ', KOLBlockCipher';
end;

function TKOLThinICE.TypeName: String;
begin
Result := 'TKOLThinICE';
end;

procedure TKOLThinICE.AssignEvents;
begin
inherited;
end;

procedure TKOLThinICE.SetupFirst;

begin
 SL.Add(Prefix + AName + ' := NewThinICE;');
end;

procedure TKOLThinICE.SetupLast;
begin
end;

constructor TKOLThinICE.Create;
begin
 inherited;
end;


end.
