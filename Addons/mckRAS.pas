unit mckRAS;

interface

uses
  Windows, Classes, Messages, Forms, SysUtils,
  KOLRAS, mirror;

type

  TKOLRAS = class(TKOLObj)
  private

    fRASName:           string;
    FOnConnecting:      TOnConnectingEvent;
    FOnError:           TOnErrorEvent;

  protected

    function  AdditionalUnits: string; override;
    procedure SetupFirst( SL: TStringList; const AName, AParent, Prefix: String ); override;
    procedure SetupLast( SL: TStringList; const AName, AParent, Prefix: String ); override;
    procedure AssignEvents( SL: TStringList; const AName: String ); override;

    procedure SetRASName(Value: string);
    procedure SetOnConnecting(Value: TOnConnectingEvent);
    procedure SetOnError(Value: TOnErrorEvent);

  published

    property RASName: string read FRASName write SetRASName;
    property OnConnecting: TOnConnectingEvent read FOnConnecting write SetOnConnecting;
    property OnError: TOnErrorEvent read FOnError write SetOnError;

  end;

  procedure Register;

implementation

{$R *.dcr}

procedure TKOLRAS.SetRASName(Value: String);
begin
  fRASName := Value;
  Change;
end;

procedure TKOLRAS.SetOnConnecting;
begin
   fOnConnecting := Value;
   Change;
end;

procedure TKOLRAS.SetOnError;
begin
   fOnError := Value;
   Change;
end;

function TKOLRAS.AdditionalUnits;
begin
   Result := ', KOLRAS';
end;

procedure TKOLRAS.SetupFirst(SL: TStringList; const AName,
  AParent, Prefix: String);
begin
  SL.Add( Prefix + AName + ' := NewRASObj;' );
  if fRASName <> '' then
  SL.Add( Prefix + AName + '.RASName := ''' + fRASName + ''';');
end;

procedure TKOLRAS.SetupLast(SL: TStringList; const AName,
  AParent, Prefix: String);
begin
   //
end;

procedure TKOLRAS.AssignEvents(SL: TStringList; const AName: String);
begin
  inherited;
  DoAssignEvents( SL, AName,
  [ 'OnConnecting', 'OnError' ],
  [ @OnConnecting , @OnError  ]);
end;

procedure Register;
begin
  RegisterComponents('KOLAddons', [TKOLRAS]);
end;

end.

