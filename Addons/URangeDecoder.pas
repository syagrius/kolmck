unit URangeDecoder;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses KOL,ULZMACommon;

type PRangeDecoder = ^TRangeDecoder;
     TRangeDecoder=object(TObj)
       public
         Range,Code:integer;
         Stream:PStream;
         procedure SetStream(const AStream:PStream);
         procedure ReleaseStream;
         procedure _Init;
         function DecodeDirectBits(const AnumTotalBits:integer):integer;
         function DecodeBit(var Aprobs: array of smallint;const Aindex:integer):integer;
       end;

procedure InitBitModels(var Aprobs: array of smallint);

implementation

const kTopMask = not ((1 shl 24) - 1);
      kNumBitModelTotalBits = 11;
      kBitModelTotal = (1 shl kNumBitModelTotalBits);
      kNumMoveBits = 5;

procedure TRangeDecoder.SetStream(const AStream:PStream);
begin
self.Stream:=AStream;
end;

procedure TRangeDecoder.ReleaseStream;
begin
stream:=nil;
end;

procedure TRangeDecoder._Init;
var i:integer;
begin
code:=0;
Range:=-1;
for i:=0 to 4 do begin
    code:=(code shl 8) or byte(ReadByte(stream));
    end;
end;

function TRangeDecoder.DecodeDirectBits(const AnumTotalBits:integer):integer;
var i,t:integer;
begin
result:=0;
for i := AnumTotalBits downto 1 do begin
    range:=range shr 1;
    t := ((Code - Range) shr 31);
    Code := Code - Range and (t - 1);
    result := (result shl 1) or (1 - t);
    if ((Range and kTopMask) = 0) then begin
       Code := (Code shl 8) or ReadByte(stream);
       Range := Range shl 8;
       end;
    end;
end;

function TRangeDecoder.DecodeBit(var Aprobs: array of smallint;const Aindex:integer):integer;
var prob,newbound:integer;
begin
prob:=Aprobs[Aindex];
newbound:=(Range shr kNumBitModelTotalBits) * prob;
if (integer((integer(Code) xor integer($80000000))) < integer((integer(newBound) xor integer($80000000)))) then begin
   Range := newBound;
   Aprobs[Aindex] := (prob + ((kBitModelTotal - prob) shr kNumMoveBits));
   if ((Range and kTopMask) = 0) then begin
      Code := (Code shl 8) or ReadByte(stream);
      Range := Range shl 8;
      end;
   result:=0;
   end else begin
       Range := Range - newBound;
       Code := Code - newBound;
       Aprobs[Aindex] := (prob - ((prob) shr kNumMoveBits));
       if ((Range and kTopMask) = 0) then begin
          Code := (Code shl 8) or ReadByte(stream);
          Range := Range shl  8;
          end;
       result:=1;
       end;
end;

procedure InitBitModels(var Aprobs: array of smallint);
var i:integer;
begin
for i:=0 to length(Aprobs)-1 do
    Aprobs[i] := kBitModelTotal shr 1;
end;

end.
