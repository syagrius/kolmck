unit ULZOutWindow;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses KOL;

type PLZOutWindow = ^TLZOutWindow;
     TLZOutWindow=object(TObj)
       public
         buffer: array of byte;
         pos:integer;
         windowSize:integer;
         streamPos:integer;
         stream:PStream;
         procedure _Create(const AwindowSize:integer);
         procedure SetStream(const Astream:PStream);
         procedure ReleaseStream;
         procedure _Init(const Asolid:boolean);
         procedure Flush;
         procedure CopyBlock(const Adistance:integer; Alen:integer);
         procedure PutByte(const Ab:byte);
         function GetByte(const Adistance:integer):byte;
       end;

implementation

procedure TLZOutWindow._Create(const AwindowSize:integer);
begin
if (length(buffer)=0) or (self.windowSize <> AwindowSize) then
   setlength(buffer,AwindowSize);
self.windowSize := AwindowSize;
pos := 0;
streamPos := 0;
end;

procedure TLZOutWindow.SetStream(const Astream:PStream);
begin
ReleaseStream;
self.stream:=Astream;
end;

procedure TLZOutWindow.ReleaseStream;
begin
flush;
self.stream:=nil;
end;

procedure TLZOutWindow._Init(const Asolid:boolean);
begin
if not Asolid then begin
   streamPos:=0;
   Pos:=0;
   end;
end;

procedure TLZOutWindow.Flush;
var size:integer;
begin
size := pos - streamPos;
if (size = 0) then
   exit;
stream.write(buffer[streamPos], size);
if (pos >= windowSize) then
   pos := 0;
streamPos := pos;
end;

procedure TLZOutWindow.CopyBlock(const Adistance:integer;Alen:integer);
var _pos:integer;
begin
_pos := self.pos - Adistance - 1;
if _pos < 0 then
   _pos := _pos + windowSize;
while Alen<>0 do begin
      if _pos >= windowSize then
         _pos := 0;
      buffer[self.pos] := buffer[_pos];
      inc(self.pos);
      inc(_pos);
      if self.pos >= windowSize then
         Flush();
    dec(Alen);
    end;
end;

procedure TLZOutWindow.PutByte(const Ab:byte);
begin
buffer[pos] := Ab;
inc(pos);
if (pos >= windowSize) then
   Flush();
end;

function TLZOutWindow.GetByte(const Adistance:integer):byte;
var _pos:integer;
begin
_pos := self.pos - Adistance - 1;
if (_pos < 0) then
   _pos := _pos + windowSize;
result:=buffer[_pos];
end;

end.
