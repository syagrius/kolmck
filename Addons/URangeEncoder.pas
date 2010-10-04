unit URangeEncoder;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses KOL,ULZMACommon;

const kNumBitPriceShiftBits = 6;
      kTopMask = not ((1 shl 24) - 1);
      kNumBitModelTotalBits = 11;
      kBitModelTotal = (1 shl kNumBitModelTotalBits);
      kNumMoveBits = 5;
      kNumMoveReducingBits = 2;

type PRangeEncoder = ^TRangeEncoder;
     TRangeEncoder=object(TObj)
       private
         ProbPrices: array [0..kBitModelTotal shr kNumMoveReducingBits-1] of integer;
       public
         Stream:PStream;
         Low,Position:int64;
         Range,cacheSize,cache:integer;
         procedure SetStream(const Astream:PStream);
         procedure ReleaseStream;
         procedure _Init;
         procedure FlushData;
         procedure FlushStream;
         procedure ShiftLow;
         procedure EncodeDirectBits(const v,AnumTotalBits:integer);
         function GetProcessedSizeAdd:int64;
         procedure Encode(var Aprobs: array of smallint;const Aindex,Asymbol:integer);
         constructor Create;
         function GetPrice(const AProb,Asymbol:integer):integer;
         function GetPrice0(const AProb:integer):integer;
         function GetPrice1(const AProb:integer):integer;
       end;

var RangeEncoder:PRangeEncoder;

procedure InitBitModels(var probs:array of smallint);

implementation

procedure TRangeEncoder.SetStream(const Astream:PStream);
begin
self.Stream:=AStream;
end;

procedure TRangeEncoder.ReleaseStream;
begin
stream:=nil;
end;

procedure TRangeEncoder._Init;
begin
position := 0;
Low := 0;
Range := -1;
cacheSize := 1;
cache := 0;
end;

procedure TRangeEncoder.FlushData;
var i:integer;
begin
for i:=0 to 4 do
    ShiftLow();
end;

procedure TRangeEncoder.FlushStream;
begin
//stream.flush;
end;

procedure TRangeEncoder.ShiftLow;
var LowHi:integer;
    temp:integer;
begin
LowHi := (Low shr 32);
if (LowHi <> 0) or (Low < int64($FF000000)) then begin
   position := position + cacheSize;
   temp := cache;
   repeat
     WriteByte(stream,temp + LowHi);
     temp := $FF;
     dec(cacheSize);
     until(cacheSize = 0);
   cache := (Low shr 24);
   end;
inc(cacheSize);
Low := (Low and integer($FFFFFF)) shl 8;
end;

procedure TRangeEncoder.EncodeDirectBits(const v,AnumTotalBits:integer);
var i:integer;
begin
for i := AnumTotalBits - 1 downto 0 do begin
    Range := Range shr 1;
    if (((v shr i) and 1) = 1) then
       Low := Low + Range;
    if ((Range and kTopMask) = 0) then begin
       Range := range shl 8;
       ShiftLow;
       end;
    end;
end;

function TRangeEncoder.GetProcessedSizeAdd:int64;
begin
result:=cacheSize + position + 4;
end;

procedure InitBitModels(var probs:array of smallint);
var i:integer;
begin
for i := 0 to length(probs) -1 do
    probs[i] := kBitModelTotal shr 1;
end;

procedure TRangeEncoder.Encode(var Aprobs: array of smallint;const Aindex,Asymbol:integer);
var prob,newbound:integer;
begin
prob := Aprobs[Aindex];
newBound := (Range shr kNumBitModelTotalBits) * prob;
if (Asymbol = 0) then begin
   Range := newBound;
   Aprobs[Aindex] := (prob + ((kBitModelTotal - prob) shr kNumMoveBits));
   end else begin
       Low := Low + (newBound and int64($FFFFFFFF));
       Range := Range - newBound;
       Aprobs[Aindex] := (prob - ((prob) shr kNumMoveBits));
       end;
if ((Range and kTopMask) = 0) then begin
   Range := Range shl 8;
   ShiftLow;
   end;
end;

constructor TRangeEncoder.Create;
var kNumBits:integer;
    i,j,start,_end:integer;
begin
kNumBits := (kNumBitModelTotalBits - kNumMoveReducingBits);
for i := kNumBits - 1 downto 0 do begin
    start := 1 shl (kNumBits - i - 1);
    _end := 1 shl (kNumBits - i);
    for j := start to _end -1 do
        ProbPrices[j] := (i shl kNumBitPriceShiftBits) +
            (((_end - j) shl kNumBitPriceShiftBits) shr (kNumBits - i - 1));
    end;
end;

function TRangeEncoder.GetPrice(const AProb,Asymbol:integer):integer;
begin
result:=ProbPrices[(((AProb - Asymbol) xor ((-Asymbol))) and (kBitModelTotal - 1)) shr kNumMoveReducingBits];
end;

function TRangeEncoder.GetPrice0(const AProb:integer):integer;
begin
result:= ProbPrices[AProb shr kNumMoveReducingBits];
end;

function TRangeEncoder.GetPrice1(const AProb:integer):integer;
begin
result:= ProbPrices[(kBitModelTotal - AProb) shr kNumMoveReducingBits];
end;

initialization
New(RangeEncoder, Create);
finalization
RangeEncoder.Free;
end.
