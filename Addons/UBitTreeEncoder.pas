unit UBitTreeEncoder;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses KOL, URangeDecoder,URangeEncoder;

type PBitTreeEncoder =^TBitTreeEncoder;
     TBitTreeEncoder=object(TObj)
       public
         Models: array of smallint;
         NumBitLevels:integer;
         constructor Create(const AnumBitLevels:integer);
         procedure _Init;
         procedure Encode(const ArangeEncoder:PRangeEncoder;const Asymbol:integer);
         procedure ReverseEncode(const ArangeEncoder:PRangeEncoder;Asymbol:integer);
         function GetPrice(const Asymbol:integer):integer;
         function ReverseGetPrice(Asymbol:integer):integer;overload;
       end;

procedure ReverseEncode(var AModels:array of smallint;const AstartIndex:integer;const ArangeEncoder:PRangeEncoder;const ANumBitLevels:integer; Asymbol:integer);
function ReverseGetPrice(var AModels:array of smallint;const AstartIndex,ANumBitLevels:integer; Asymbol:integer):integer;

implementation

constructor TBitTreeEncoder.Create(const AnumBitLevels:integer);
begin
self.NumBitLevels:=AnumBitLevels;
setlength(Models,1 shl AnumBitLevels);
end;

procedure TBitTreeEncoder._Init;
begin
URangeDecoder.InitBitModels(Models);
end;

procedure TBitTreeEncoder.Encode(const ArangeEncoder:PRangeEncoder;const Asymbol:integer);
var m,bitindex,bit:integer;
begin
m := 1;
for bitIndex := NumBitLevels -1 downto 0 do begin
    bit := (Asymbol shr bitIndex) and 1;
    ArangeEncoder.Encode(Models, m, bit);
    m := (m shl 1) or bit;
    end;
end;

procedure TBitTreeEncoder.ReverseEncode(const ArangeEncoder:PRangeEncoder;Asymbol:integer);
var m,i,bit:integer;
begin
m:=1;
for i:= 0 to NumBitLevels -1 do begin
    bit := Asymbol and 1;
    ArangeEncoder.Encode(Models, m, bit);
    m := (m shl 1) or bit;
    Asymbol := Asymbol shr 1;
    end;
end;

function TBitTreeEncoder.GetPrice(const Asymbol:integer):integer;
var price,m,bitindex,bit:integer;
begin
price := 0;
m := 1;
for bitIndex := NumBitLevels - 1 downto 0 do begin
    bit := (Asymbol shr bitIndex) and 1;
    price := price + RangeEncoder.GetPrice(Models[m], bit);
    m := (m shl 1) + bit;
    end;
result:=price;
end;

function TBitTreeEncoder.ReverseGetPrice(Asymbol:integer):integer;
var price,m,i,bit:integer;
begin
price := 0;
m := 1;
for i:= NumBitLevels downto 1 do begin
    bit := Asymbol and 1;
    Asymbol := Asymbol shr 1;
    price :=price + RangeEncoder.GetPrice(Models[m], bit);
    m := (m shl 1) or bit;
    end;
result:=price;
end;

function ReverseGetPrice(var AModels:array of smallint;const AstartIndex,ANumBitLevels:integer;Asymbol:integer):integer;
var price,m,i,bit:integer;
begin
price := 0;
m := 1;
for i := ANumBitLevels downto 1 do begin
    bit := Asymbol and 1;
    Asymbol := Asymbol shr 1;
    price := price + RangeEncoder.GetPrice(AModels[AstartIndex + m], bit);
    m := (m shl 1) or bit;
    end;
result:=price;
end;

procedure ReverseEncode(var AModels:array of smallint;const AstartIndex:integer;const ArangeEncoder:PRangeEncoder;const ANumBitLevels:integer;Asymbol:integer);
var m,i,bit:integer;
begin
m:=1;
for i := 0 to ANumBitLevels -1 do begin
    bit := Asymbol and 1;
    ArangeEncoder.Encode(AModels, AstartIndex + m, bit);
    m := (m shl 1) or bit;
    Asymbol := Asymbol shr 1;
    end;
end;

end.
