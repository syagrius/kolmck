unit UBitTreeDecoder;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses KOL, URangeDecoder;

type PBitTreeDecoder = ^TBitTreeDecoder;
     TBitTreeDecoder=object(TObj)
       public
         Models: array of smallint;
         NumBitLevels:integer;
         constructor Create(const AnumBitLevels:integer);
         procedure _Init;
         function Decode(const ArangeDecoder:PRangeDecoder):integer;
         function ReverseDecode(const ArangeDecoder:PRangeDecoder):integer;overload;
       end;

function ReverseDecode(var AModels: array of smallint; const AstartIndex:integer;const ArangeDecoder:PRangeDecoder; const ANumBitLevels:integer):integer;overload;

implementation

constructor TBitTreeDecoder.Create(const AnumBitLevels:integer);
begin
self.NumBitLevels := AnumBitLevels;
setlength(Models,1 shl AnumBitLevels);
end;

procedure TBitTreeDecoder._Init;
begin
urangedecoder.InitBitModels(Models);
end;

function TBitTreeDecoder.Decode(const ArangeDecoder:PRangeDecoder):integer;
var m,bitIndex:integer;
begin
m:=1;
for bitIndex := NumBitLevels downto 1 do begin
    m:=m shl 1 + ArangeDecoder.DecodeBit(Models, m);
    end;
result:=m - (1 shl NumBitLevels);
end;

function TBitTreeDecoder.ReverseDecode(const ArangeDecoder:PRangeDecoder):integer;
var m,symbol,bitindex,bit:integer;
begin
m:=1;
symbol:=0;
for bitindex:=0 to numbitlevels-1 do begin
    bit:=ArangeDecoder.DecodeBit(Models, m);
    m:=(m shl 1) + bit;
    symbol:=symbol or (bit shl bitIndex);
    end;
result:=symbol;
end;

function ReverseDecode(var AModels: array of smallint;const AstartIndex:integer;
            const ArangeDecoder:PRangeDecoder;const ANumBitLevels:integer):integer;
var m,symbol,bitindex,bit:integer;
begin
m:=1;
symbol:=0;
for bitindex:=0 to ANumBitLevels -1 do begin
    bit := ArangeDecoder.DecodeBit(AModels, AstartIndex + m);
    m := (m shl 1) + bit;
    symbol := symbol or bit shl bitindex;
    end;
result:=symbol;
end;

end.
