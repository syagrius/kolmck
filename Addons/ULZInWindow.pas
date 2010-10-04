unit ULZInWindow;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses KOL;

type TLZInWindow= object(TObj)
       public
         bufferBase: array of byte;// pointer to buffer with data
         stream:PStream;
         posLimit:integer; // offset (from _buffer) of first byte when new block reading must be done
         streamEndWasReached:boolean; // if (true) then _streamPos shows real end of stream

         pointerToLastSafePosition:integer;

         bufferOffset:integer;

         blockSize:integer;  // Size of Allocated memory block
         pos:integer;             // offset (from _buffer) of curent byte
         keepSizeBefore:integer;  // how many BYTEs must be kept in buffer before _pos
         keepSizeAfter:integer;   // how many BYTEs must be kept buffer after _pos
         streamPos:integer;   // offset (from _buffer) of first not read byte from Stream

         procedure MoveBlock;
         procedure ReadBlock;
         procedure _Free;
         procedure _Create(const AkeepSizeBefore, AkeepSizeAfter, AkeepSizeReserv:integer);
         procedure SetStream(const Astream:PStream);
         procedure ReleaseStream;
         procedure _Init;virtual;
         procedure MovePos;virtual;
         function GetIndexByte(const Aindex:integer):byte;
         // index + limit have not to exceed _keepSizeAfter;
         function GetMatchLen(const Aindex:integer;Adistance,Alimit:integer):integer;
         function GetNumAvailableBytes:integer;
         procedure ReduceOffsets(const AsubValue:integer);
       end;

implementation

procedure TLZInWindow.MoveBlock;
var offset,numbytes,i:integer;
begin
offset := bufferOffset + pos - keepSizeBefore;
// we need one additional byte, since MovePos moves on 1 byte.
if (offset > 0) then
   dec(offset);

numBytes := bufferOffset + streamPos - offset;

// check negative offset ????
for i := 0 to numBytes -1 do
    bufferBase[i] := bufferBase[offset + i];
bufferOffset := bufferOffset - offset;
end;

procedure TLZInWindow.ReadBlock;
var size,numreadbytes,pointerToPostion:integer;
begin
if streamEndWasReached then
   exit;
while (true) do begin
      size := (0 - bufferOffset) + blockSize - streamPos;
      if size = 0 then
         exit;
      numReadBytes := stream.Read(bufferBase[bufferOffset + streamPos], size);
      if (numReadBytes = 0) then begin
         posLimit := streamPos;
         pointerToPostion := bufferOffset + posLimit;
         if (pointerToPostion > pointerToLastSafePosition) then
            posLimit := pointerToLastSafePosition - bufferOffset;
         streamEndWasReached := true;
         exit;
         end;
      streamPos := streamPos + numReadBytes;
      if (streamPos >= pos + keepSizeAfter) then
         posLimit := streamPos - keepSizeAfter;
    end;
end;

procedure TLZInWindow._Free;
begin
setlength(bufferBase,0);
end;

procedure TLZInWindow._Create(const AkeepSizeBefore, AkeepSizeAfter, AkeepSizeReserv:integer);
var _blocksize:integer;
begin
self.keepSizeBefore := AkeepSizeBefore;
self.keepSizeAfter := AkeepSizeAfter;
_blocksize := AkeepSizeBefore + AkeepSizeAfter + AkeepSizeReserv;
if (length(bufferBase) = 0) or (self.blockSize <> _blocksize) then begin
   _Free;
   self.blockSize := _blocksize;
   setlength(bufferBase,self.blockSize);
   end;
pointerToLastSafePosition := self.blockSize - AkeepSizeAfter;
end;

procedure TLZInWindow.SetStream(const Astream:PStream);
begin
self.stream:=Astream;
end;

procedure TLZInWindow.ReleaseStream;
begin
stream:=nil;
end;

procedure TLZInWindow._Init;
begin
bufferOffset := 0;
pos := 0;
streamPos := 0;
streamEndWasReached := false;
ReadBlock;
end;

procedure TLZInWindow.MovePos;
var pointerToPostion:integer;
begin
inc(pos);
if pos > posLimit then begin
   pointerToPostion := bufferOffset + pos;
   if pointerToPostion > pointerToLastSafePosition then
      MoveBlock;
   ReadBlock;
   end;
end;

function TLZInWindow.GetIndexByte(const Aindex:integer):byte;
begin
result:=bufferBase[bufferOffset + pos + Aindex];
end;

function TLZInWindow.GetMatchLen(const Aindex:integer;Adistance,Alimit:integer):integer;
var pby,i:integer;
begin
if streamEndWasReached then
   if (pos + Aindex) + Alimit > streamPos then
      Alimit := streamPos - (pos + Aindex);
inc(Adistance);
// Byte *pby = _buffer + (size_t)_pos + Aindex;
pby := bufferOffset + pos + Aindex;

i:=0;
while (i<Alimit)and(bufferBase[pby + i] = bufferBase[pby + i - Adistance]) do begin
      inc(i);
      end;
result:=i;
end;

function TLZInWindow.GetNumAvailableBytes:integer;
begin
result:=streamPos - pos;
end;

procedure TLZInWindow.ReduceOffsets(const Asubvalue:integer);
begin
bufferOffset := bufferOffset + Asubvalue;
posLimit := posLimit - Asubvalue;
pos := pos - Asubvalue;
streamPos := streamPos - Asubvalue;
end;

end.
