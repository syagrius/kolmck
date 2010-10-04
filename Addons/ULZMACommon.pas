unit ULZMACommon;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses KOL;

type TLZMAProgressAction=(LPAMax,LPAPos);
     TLZMAProgress=procedure (const Action:TLZMAProgressAction;const Value:int64) of object;

function ReadByte(const stream:PStream):byte;
procedure WriteByte(const stream:PStream;b:byte);

const CodeProgressInterval = 50;//approx. number of times an OnProgress event will be fired during coding

implementation

function ReadByte(const stream:PStream):byte;
begin
stream.Read(result,1);
end;

procedure WriteByte(const stream:PStream;b:byte);
begin
stream.Write(b,1);
end;

end.
