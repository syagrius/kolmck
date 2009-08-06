{$IFDEF FPC}
 {$mode delphi}
{$ENDIF}
unit KOLHTTPDownload;
{

  ("`-''-/").___..--''"`-._
 `6_ 6  )   `-.  (     ).`-.__.`)
 (_Y_.)'  ._   )  `._ `. ``-..-'
  _..`--'_..-_/  /--'_.' ,'
(il).-''  (li).'  ((!.-'

  Download with HTTP-protocol

  Copyright © 2007-2008 Denis Fateyev (Danger)
  Website: <http://www.fateyev.com>
  E-Mail: <denis@fateyev.com>

  'ParseURL' and 'Posn' functions are copyright (C) 1997-2001 by Francois Piette
  "Permission is granted to anyone to use this software for any purpose, including
  commercial applications, and to alter it and redistribute it freely."  }

  {* TKOLHTTPDownload is the non-visual component that provides a downloading resources with HTTP-protocol. Now uses WinInet routines.
  |<pre>
  |Copyright (C) 2007-2008 Denis Fateyev (Danger) (<a href="mailto:denis@fateyev.com">denis@fateyev.com</a>).
  |</pre>
  |TKOLHTTPDownload coming under the form of a KOL library unit, it can be simply used
  by creating object at runtime, setting the necessary properties:
  !uses Windows, Messages, KOL, ..., KOLHTTPDownload;
  ! //...
  !var DL : PHTTPDownload;
  ! //...
  !DL := NewHTTPDownload;
  !DL.OnDownload:= MyDownload_Proc;
  !DL.GetResource( 'http://example.com/foo/bar.zip' );
  !DL. ...
  !DL.Free;
  |<p>Certainly you can use the 'MCK mirror' provided with component to manage control properties at design time. }

interface

// This conditional define allows some manupulations with HTTP-headers,
// you can disable it (if you really don't need it) by commenting the following line.
{$DEFINE USE_CUSTOMHEADERS}

//-----------------------------------------------------
uses
 Windows, WinInet, KOL;

//-----------------------------------------------------
const
 iDefProxyPort = 3128;
 iTimeOutValue = 200;  // 0.2 sec
 iDataBufSize  = 4096; // 4 KByte buffer
 strUserAgent  = 'Dangers HTTPClient/2.1';
 strConnectType = 'Connection: close';
 strProxyConnectType = 'Proxy-Connection: close';

//-----------------------------------------------------
 { THTTPHeader }

type
 PHTTPHeader = ^THTTPHeader;
 THTTPHeader = record
 {* |<p>Most important values that can be extracted from http-servers response
 |(see <a href="thttpdownload.htm#parseheaders">ParseHeaders</a> procedure
 |below for more details).</p> }
  HTTPVersion:  KOLstring;
  StatusCode:   Integer;
  ReasonPhrase: KOLstring;
  ServerDate:   KOLstring;
  ServerStr:    KOLstring;
  LastModified: KOLstring;
  Location:     KOLstring;
  SetCookie:    KOLstring;
  Expires:      KOLstring;
  ContentLength: Integer;
  TransferEncoding: KOLstring;
  ContentType:   KOLstring;
 end;

//-----------------------------------------------------
 { THTTPDownload }

 PHTTPDownload = ^THTTPDownload;
 PDownloadWorker = ^TDownloadWorker;
 TKOLHTTPDownload = PHTTPDownload;
 THTTPHdrRecvEvent = procedure( Sender: PHTTPDownload; HeaderList: PStrList ) of object;
 {* |Event to be called when http-headers received from http-server. }

 THTTPProgressEvent = procedure( Sender: PHTTPDownload;
   BytesRecv: Integer; BytesAll: Integer ) of object;
 {* |Event to be called when download progress is changed. }

 THTTPErrorEvent = procedure( Sender: PHTTPDownload; Error: Word ) of object;
 {* |Event to be called when error occured while download process. }

 THTTPDownloadEvent = procedure( Sender: PHTTPDownload; Data: PStream ) of object;
 {* |Event to be called when resource download completed. }

 THTTPDownload = object( TObj )
 {* |This object implements all functionality of component.<br>
 |Use <i>NewHTTPDownload</i> constuction function for creation of object instance. Here is the prototype:
 ! function NewHTTPDownload: PHTTPDownload; }
  private
   fWorker: PDownloadWorker;
   fHeaderList: PStrList;
   {$IFDEF USE_CUSTOMHEADERS}
   fCHeaderList: PStrList;
   {$ENDIF}
   fDataStream: PStream;
   fResource: string;
   fBusy: Boolean;
   fPort: Word;
   fHostName: string;
   fPath: string;
   fUserName: string;
   fPassword: string;
   fProxySrv: string;
   fProxyPort: Word;
   fPreConfigProxy: Boolean;

   fOnError: THTTPErrorEvent;
   fOnHeaderReceived: THTTPHdrRecvEvent;
   fOnProgress: THTTPProgressEvent;
   fOnDownload: THTTPDownloadEvent;

  public
    function CheckConnection( AResourceName: string ): Boolean;
    {* Simple check if a connection to host that provides specified resource can be established,
    and requested resource can be retrieved. By example:
    !     CheckConnection( 'http://www.example.com/foo/bar.zip' );
    Note that this function may give the wrong results if destination host doesn't accept 'ping' requests.
    |Return value: <i>True</i> if a connection is made successfully, or <i>False</i> otherwise. }

    function  GetResource( AResourceName: string ): Boolean;
    {* |Initiate download process for the specified resource.<br>
    |The parameter <i>AResourceName</i> must contains full path of the requested resource
    in such syntax:
    !     protocol://[user[:password]@]server[:port]/path
    |If parameter <i>port</i> not specified, then <i>standard http-port (80)</i> will be used in request.
    Authorization parameters can be omitted too, if isn't needed.
    In simple case can be used, by example:
    !     GetResource( 'http://www.example.com/foo/bar.zip' );
    |Return value: the function returns <i>False</i> if resource request has invalid syntax,
    |otherwise <i>True</i> returned. }

    procedure SetProxySettings( AProxyServer: string; iProxyPort: Integer = iDefProxyPort );
    {* |Proxy settings for the resource request.<br>
    |<i>iProxyPort</i> parameter can be omitted then <i>standard proxy port (3128)</i> will be used. }

    procedure SetAuthInfo( AUserName: string; APassword: string );
    {* Authorization parameters for the resource request. }

    function  ParseHeaders( var Header: PHTTPHeader ): Boolean;
    {* Extract http-headers information and put into the specified HTTPHeader. By example:
    !var
    !  DL: PHTTPDownload;
    !  Header: PHTTPHeader;
    !// ...
    !procedure TForm1.DLHeaderReceived( Sender: PHTTPDownload; HeaderList: PStrList );
    !begin
    !  New( Header );
    !  DL.ParseHeaders( Header );
    !  // ... do something with Header ...
    !  MsgOk( Header.ReasonPhrase );
    !  // ...
    !  Dispose( Header );
    !end;
    |Return value: <i>False</i> if http-headers doesn't exists (nothing to analyze). }

    {$IFDEF USE_CUSTOMHEADERS}
    procedure AddCustomHeader( AHeader: string );
    {* |Add custom line to requests http-header. By example:
    !var
    !  DL: PHTTPDownload;
    !// ...
    !procedure TForm1.Button1Click( Sender: PObj );
    !begin
    !  DL.AddCustomHeader( 'Cookie: PHPSESSID=abcdef' );
    !  DL.GetResource( 'http://www.example.com/foo/bar.zip' );
    !end;
    Once assigned these headers will be added automatically to each request sent to http-server
    (while the current THTTPDownload object is in use). Custom headers are not assigned by default.
    |To clear user defined http-headers list, call <i>ClearCustomHeaders</i> procedure.
    |Note that <i>'Connection: close'</i> or <i>'Proxy-Connection: close'</i> (depends on connection type)
    |will be included in the request headers anyway.<br>
    |You must add <b>USE_CUSTOMHEADERS</b> conditional symbol into the project options list. }

    procedure SetCustomHeaders( AHeaderList: PStrList );
    {* |Assign the custom http-headers list from another one. By example:
    !var
    !  DL: PHTTPDownload;
    !  CList: PStrList;
    !// ...
    !procedure TForm1.Button1Click( Sender: PObj );
    !begin
    !  CList:= NewStrList;
    !  CList.Add( 'Cookie: PHPSESSID=abcdef' );
    !  DL.SetCustomHeaders( CList );
    !  DL.GetResource( 'http://www.example.com/foo/bar.zip' );
    !  CList.Free;
    !end;
    |You must add <b>USE_CUSTOMHEADERS</b> conditional symbol into the project options list. }

    procedure ClearCustomHeaders;
    {* |Clear user defined http-headers list (restore to defaults).
    |You must add <b>USE_CUSTOMHEADERS</b> conditional symbol into the project options list. }
    {$ENDIF}

    procedure CancelDownload;
    {* |Drop current download process immediately. }

    property Resource: string read fResource;
    {* |Currently requested resource. By default: <i>None.</i> }

    property  ProxyServer: string read fProxySrv write fProxySrv;
    {* |IP-address or hostname of http-proxy server. By default: <i>None.</i> }

    property  ProxyPort: Word read fProxyPort write fProxyPort;
    {* |TCP Port of http-proxy server. By default: <i>3128.</i> }

    property  UserName: string read fUserName write fUserName;
    {* |HTTP Autorization parameters: username. By default: <i>None.</i> }

    property  Password: string read fPassword write fPassword;
    {* |HTTP Autorization parameters: password. By default: <i>None.</i> }
    
    property  UsePreconfigProxy: Boolean read fPreConfigProxy write fPreConfigProxy;
    {*|Parameter that allows to use connection settings stored in Internet Explorer.
     Retrieves the proxy or direct configuration from the Windows registry.
     |By default: <i>False.</i> }

    property  HeaderList: PStrList read fHeaderList;
    {*|Retrieves all received http-headers in raw format (as is).
    Most important parameters can be retrieved with ParseHeaders procedure. }

    {$IFDEF USE_CUSTOMHEADERS}
    property  CustomHeaderList: PStrList read fCHeaderList;
    {*|Retrieves custom http-header list assigned by user.
    See SetCustomHeaders procedure for more details. }
    {$ENDIF}

    property  ReceivedData: PStream read fDataStream;
    {*|Retrieves downloaded resource if present. }

    property  Busy: Boolean read fBusy;
    {*| If <i>True</i>, the object is busy and resource download is in progress at the moment.
    If you wish, you can terminate download process at any moment with CancelDownload procedure. }

    property  OnError: THTTPErrorEvent read fOnError write fOnError;
    {* |Event to be called when error occured while download process. }

    property  OnHeaderReceived: THTTPHdrRecvEvent read fOnHeaderReceived write fOnHeaderReceived;
    {* |Event to be called when http-headers received from http-server. }

    property  OnProgress: THTTPProgressEvent read fOnProgress write fOnProgress;
    {* |Event to be called when download progress is changed.
    Note that there's no way to automatically determine the whole size of requested resource
    |if <i>'Content-Length'</i> field is missing in the http-header (i.e. if <i>Transfer-Encoding</i>
    |header field (rfc-2068 section 14.40) is present and indicates that the <i>"chunked"</i> transfer
    |coding has been applied). Therefore, if <i>'Content-Length'</i> is present, <i>BytesAll</i>
    |parameter indicates the requested resource size, otherwise it's equal to <i>'-1'</i>. }

    property  OnDownload: THTTPDownloadEvent read fOnDownload write fOnDownload;
    {* |Event to be called when resource download completed. }

   destructor Destroy; virtual;
 end;

//-----------------------------------------------------
 { TDownloadWorker }

 TDownloadWorker = object (TObj )
  private
   // Contains parent object's pointer (or NIL if download terminated)
   fOwner: PHTTPDownload;
   fWThread: PThread;
   fDLThread: PThread;
   fCritSection: TRTLCriticalSection;
   fDataBuf: PChar;
   fPort: Word;
   fHostName: string;
   fPath: string;
   fUserName: string;
   fPassword: string;
   fProxySrv: string;
   fProxyPort:  Word;
   fPreConfigProxy: Boolean;
   iContentLen: Integer;
   iReadCount: Integer;

   function On_DownloadExecute( Sender: PThread ): Integer;
   function On_WatchExecute( Sender: PThread ): Integer;
   procedure On_UpdateProgress;

  public
   procedure StartDownload;
   function StopDownload: Integer;
   destructor Destroy; virtual;
 end;

//-----------------------------------------------------
function NewHTTPDownload: PHTTPDownload;
function NewDownloadWorker( AOwner: PHTTPDownload ): PDownloadWorker;
//-----------------------------------------------------

implementation

//-----------------------------------------------------
function NewHTTPDownload: PHTTPDownload;
begin
 New( Result, Create );
 with ( Result^ ) do
 begin
  fBusy:= false;
  fPreConfigProxy:= false;
  fProxyPort:= iDefProxyPort;
 end;
end;

//-----------------------------------------------------
function NewDownloadWorker( AOwner: PHTTPDownload ): PDownloadWorker;
begin
 New( Result, Create );
 with ( Result^ ) do
 begin
  fOwner:= AOwner;
  InitializeCriticalSection( fCritSection );
 end;
end;

//-----------------------------------------------------
function StrPas(const Str: PChar): string;
begin
  Result:= Str;
end;

//-----------------------------------------------------
{ Find the count'th occurence of the s string in the t string.  }
{ If count < 0 then look from the back                          }
function Posn(const s , t : String; Count : Integer) : Integer;
var
    i, h, Last : Integer;
    u          : String;
begin
    u := t;
    if Count > 0 then
    begin
        Result := Length(t);
        for i := 1 to Count do
        begin
            h := Pos(s, u);
            if h > 0 then
                u := Copy(u, h + 1, Length(u))
            else
            begin
                u := '';
                Inc(Result);
            end;
        end;
        Result := Result - Length(u);
    end
    else if Count < 0 then
    begin
        Last := 0;
        for i := Length(t) downto 1 do
        begin
            u := Copy(t, i, Length(t));
            h := Pos(s, u);
            if (h <> 0) and ((h + i) <> Last) then
            begin
                Last := h + i - 1;
                Inc(count);
                if Count = 0 then
                    break;
            end;
        end;
        if Count = 0 then
            Result := Last
        else
            Result := 0;
    end
    else
        Result := 0;
end;

//-----------------------------------------------------
{ Syntax of an URL: protocol://[user[:password]@]server[:port]/path  }
procedure ParseURL(const url : String; var Proto, User, Pass, Host, Port, Path : String);
var
    p, q    : Integer;
    s       : String;
    CurPath : String;
begin
    CurPath := Path;
    proto   := '';
    User    := '';
    Pass    := '';
    Host    := '';
    Port    := '';
    Path    := '';

    if Length(url) < 1 then Exit;

    { Handle path beginning with "./" or "../".          }
    { This code handle only simple cases !               }
    { Handle path relative to current document directory }
    if (Copy(url, 1, 2) = './') then
    begin
        p := Posn('/', CurPath, -1);
        if p > Length(CurPath) then
            p := 0;
        if p = 0 then
            CurPath := '/'
        else
            CurPath := Copy(CurPath, 1, p);
        Path := CurPath + Copy(url, 3, Length(url));
        Exit;
    end
    { Handle path relative to current document parent directory }
    else if (Copy(url, 1, 3) = '../') then
    begin
        p := Posn('/', CurPath, -1);
        if p > Length(CurPath) then
            p := 0;
        if p = 0 then
            CurPath := '/'
        else
            CurPath := Copy(CurPath, 1, p);

        s := Copy(url, 4, Length(url));
        { We could have several levels }
        while TRUE do
        begin
            CurPath := Copy(CurPath, 1, p-1);
            p := Posn('/', CurPath, -1);
            if p > Length(CurPath) then
                p := 0;
            if p = 0 then
                CurPath := '/'
            else
                CurPath := Copy(CurPath, 1, p);
            if (Copy(s, 1, 3) <> '../') then
                break;
            s := Copy(s, 4, Length(s));
        end;

        Path := CurPath + Copy(s, 1, Length(s));
        Exit;
    end;

    p := pos('://',url);
    if p = 0 then
    begin
        if (url[1] = '/') then
        begin
            { Relative path without protocol specified }
            proto := 'http';
            p     := 1;
            if (Length(url) > 1) and (url[2] <> '/') then
            begin
                { Relative path }
                Path := Copy(url, 1, Length(url));
                Exit;
            end;
        end
        else if lowercase(Copy(url, 1, 5)) = 'http:' then
        begin
            proto := 'http';
            p     := 6;
            if (Length(url) > 6) and (url[7] <> '/') then
            begin
                { Relative path }
                Path := Copy(url, 6, Length(url));
                Exit;
            end;
        end
        else if lowercase(Copy(url, 1, 7)) = 'mailto:' then
        begin
            proto := 'mailto';
            p := pos(':', url);
        end;
    end
    else
    begin
        proto := Copy(url, 1, p - 1);
        inc(p, 2);
    end;
    s := Copy(url, p + 1, Length(url));

    p := pos('/', s);
    q := pos('?', s);
    if (q > 0) and ((q < p) or (p = 0)) then
        p := q;
    if p = 0 then
        p := Length(s) + 1;
    Path := Copy(s, p, Length(s));
    s    := Copy(s, 1, p-1);

    p := Posn(':', s, -1);
    if p > Length(s) then
        p := 0;
    q := Posn('@', s, -1);
    if q > Length(s) then
        q := 0;
    if (p = 0) and (q = 0) then
    begin   { no user, password or port }
        Host := s;
        Exit;
    end
    else if q < p then
    begin  { a port given }
        Port := Copy(s, p + 1, Length(s));
        Host := Copy(s, q + 1, p - q - 1);
        if q = 0 then
            Exit; { no user, password }
        s := Copy(s, 1, q - 1);
    end
    else
    begin
        Host := Copy(s, q + 1, Length(s));
        s := Copy(s, 1, q - 1);
    end;
    p := pos(':', s);
    if p = 0 then
        User := s
    else
    begin
        User := Copy(s, 1, p - 1);
        Pass := Copy(s, p + 1, Length(s));
    end;
end;

//---------------- { THTTPDownload } -------------------------------

function THTTPDownload.ParseHeaders( var Header: PHTTPHeader ): Boolean;
var
 i: Integer; S: KOLstring;
begin
 Result:= false;

 if ( not Assigned( fHeaderList ) ) then Exit;
  // HTTP/1.1 200 OK
 Header.ReasonPhrase:= fHeaderList.Items[0];
 Header.HTTPVersion:= Parse( Header.ReasonPhrase, ' ' );
 Header.StatusCode:= Str2Int( Parse(Header.ReasonPhrase, ' ') );
 // avoid curious things if value isn't present in the list
 Header.ContentLength:= -1;
 // begin from second list item
 for i:= 2 to fHeaderList.Count do
 begin
  S:= fHeaderList.Items[i-1];
  // Date: Wed, 09 May 2007 14:31:23 GMT
  if ( Pos('Date: ', S) > 0 ) then
  begin
   Parse(S, ' '); Header.ServerDate:= S;
   Continue;
  end;
  // Server: Apache x.x.x (Unix)
  if ( Pos('Server: ', S) > 0 ) then
  begin
   Parse(S, ' '); Header.ServerStr:= S;
   Continue;
  end;
  // Last-Modified: Wed, 09 May 2007 14:31:23 GMT
  if ( Pos('Last-Modified: ', S) > 0 ) then
  begin
   Parse(S, ' '); Header.LastModified:= S;
   Continue;
  end;
  // Set-Cookie: PHPSESSID=xxxxxxxxx
  if ( Pos('Set-Cookie: ', S) > 0 ) then
  begin
   Parse(S, ' '); Header.SetCookie:= S;
   Continue;
  end;
  // Expires: Wed, 10 May 2007 14:31:23 GMT
  if ( Pos('Expires: ', S) > 0 ) then
  begin
   Parse(S, ' '); Header.Expires:= S;
   Continue;
  end;
  // Location: foobar.html
  if ( Pos('Location: ', S) > 0 ) then
  begin
   Parse(S, ' '); Header.Location:= S;
   Continue;
  end;
  // Content-Length: 12345
  if ( Pos('Content-Length: ', S) > 0 ) then
  begin
   Parse(S, ' '); Header.ContentLength:= Str2Int( S );
   Continue;
  end;
  // Transfer-Encoding: chunked
  if ( Pos('Transfer-Encoding: ', S) > 0 ) then
  begin
   Parse(S, ' '); Header.TransferEncoding:= S;
   Continue;
  end;
  // Content-Type: application/zip
  if ( Pos('Content-Type: ', S) > 0 ) then
  begin
   Parse(S, ' '); Header.ContentType:= S;
   Continue;
  end;
 end;

 Result:= true;
end;

//-----------------------------------------------------
procedure THTTPDownload.SetProxySettings( AProxyServer: string; iProxyPort: Integer = iDefProxyPort );
begin
 fProxySrv:= AProxyServer;
 fProxyPort:= iProxyPort;
end;

//-----------------------------------------------------
procedure THTTPDownload.SetAuthInfo( AUserName: string; APassword: string );
begin
 fUserName:= AUserName;
 fPassword:= APassword;
end;

//-----------------------------------------------------
{$IFDEF USE_CUSTOMHEADERS}
procedure THTTPDownload.AddCustomHeader( AHeader: string );
begin
 if ( Length( AHeader ) > 0 ) then
 begin
  if ( not Assigned( fCHeaderList ) ) then
  begin
   fCHeaderList:= NewStrList;
   fCHeaderList.Add2AutoFree( @Self );
  end; // 'if ( not Assigned( fCHeaderList )'
  fCHeaderList.Add( AHeader );
 end;
end;

//-----------------------------------------------------
procedure THTTPDownload.ClearCustomHeaders;
begin
 if Assigned( fCHeaderList ) then fCHeaderList.Clear;
end;

//-----------------------------------------------------
procedure THTTPDownload.SetCustomHeaders( AHeaderList: PStrList );
begin
 if Assigned( AHeaderList ) then
 begin
  if ( not Assigned( fCHeaderList ) ) then
  begin
   fCHeaderList:= NewStrList;
   fCHeaderList.Add2AutoFree( @Self );
  end; // 'if ( not Assigned( fCHeaderList )'
  fCHeaderList.Assign( AHeaderList );
 end;
end;
{$ENDIF}

//-----------------------------------------------------
function THTTPDownload.CheckConnection( AResourceName: string ): Boolean;
begin
  Result:= false;

  // I'm wondering why FLAG_ICC_FORCE_CONNECTION declaration is missing in WinInet.pas
  if ( InternetCheckConnection( PChar( AResourceName ), $00000001 {FLAG_ICC_FORCE_CONNECTION}, 0 ) ) then
   Result:= true
  else
   if Assigned( fOnError ) then fOnError( @Self, GetLastError );
end;

//-----------------------------------------------------
function THTTPDownload.GetResource( AResourceName: string ): Boolean;
var
 strPort, strProto: string;
begin
 Result:= false;
 CancelDownload;

 if ( not fBusy ) then
 begin
  fResource:= AResourceName;
  // checking request data
  ParseURL( fResource, strProto, fUserName, fPassword, fHostName, strPort, fPath );
  if ( strProto = '' ) then strProto:= 'http';
  if ( ( fHostName = '' ) or ( fPath = '' ) or ( strProto <> 'http' ) ) then
  begin
   if Assigned( fOnError ) then fOnError( @Self, ERROR_INTERNET_INVALID_URL );
   Exit;
  end;
  if ( strPort = '' ) then fPort:= INTERNET_DEFAULT_HTTP_PORT
  else fPort:= Str2Int( strPort );

  if Assigned( fOnHeaderReceived ) then
   if ( not Assigned( fHeaderList ) ) then
   begin
    fHeaderList:= NewStrList;
    fHeaderList.Add2AutoFree( @Self );
   end;

  if Assigned( fOnDownload ) then
  begin
   if ( not Assigned( fDataStream ) ) then
   begin
    fDataStream:= NewMemoryStream;
    fDataStream.Add2AutoFree( @Self );
   end
   else fDataStream.Size:= 0;
  end;

  fBusy:= true;
  fWorker:= NewDownloadWorker( @Self );
  fWorker.StartDownload;
  Result:= true;  
 end;

end;

//-----------------------------------------------------
procedure THTTPDownload.CancelDownload;
begin
 if ( fBusy ) then
  fWorker.StopDownload;
end;

//-----------------------------------------------------
destructor THTTPDownload.Destroy;
begin
 CancelDownload;
 fResource:= '';
 fHostName:= '';
 fPath:= '';
 fProxySrv:= '';
 fUserName:= '';
 fPassword:= '';
 inherited;
end;

//---------------- { TDownloadWorker } -------------------------------

procedure TDownloadWorker.StartDownload;
begin
 fWThread:= NewThread;
 fWThread.OnExecute:= On_WatchExecute;
 fWThread.Add2AutoFree( @Self );
 fWThread.Resume;
end;

//-----------------------------------------------------
function TDownloadWorker.On_WatchExecute( Sender: PThread ): Integer;
begin
 Result:= 0; // stub

 // create download working thread
 fDLThread:= NewThreadEx( On_DownloadExecute );
 // wait for download thread finished (any way)
 fDLThread.WaitFor;
 // destroy worker object
 Free;
end;

//-----------------------------------------------------
function TDownloadWorker.StopDownload: Integer;
var
 lpOwner: PHTTPDownload;
begin
 Result:= 0; // stub

 lpOwner:= nil; // avoid compiler warning
 EnterCriticalSection( fCritSection );
 try
  if Assigned( fOwner ) then
  begin
   lpOwner:= PHTTPDownload( fOwner );
   fOwner:= nil;
  end;
 finally
  LeaveCriticalSection( fCritSection );
 end;

 // trying to terminate thread gracefully
 if ( not fDLThread.Terminated ) then fDLThread.WaitForTime( iTimeOutValue );
 // terminate thread forcefully
 if ( not fDLThread.Terminated ) then fDLThread.Terminate;

 if Assigned( lpOwner ) then
 begin
  // don't keep partially downloaded file
  if Assigned( lpOwner.fDataStream ) then
   lpOwner.fDataStream.Size:= 0;
  lpOwner.fBusy:= false;
 end;

end;

//-----------------------------------------------------
procedure TDownloadWorker.On_UpdateProgress;
begin
 if Assigned( fOwner ) then
  fOwner.OnProgress( fOwner, iReadCount, iContentLen );
end;

//-----------------------------------------------------
function TDownloadWorker.On_DownloadExecute( Sender: PThread ): Integer;
var
 hSession, hConnect, hRequest: HINTERNET;
 iBufSize, lpdwIndex, iNumRead: Cardinal;
 Buf: PChar; i, iErrorCode: Integer;

 procedure CloseHandles;
 begin
   InternetCloseHandle( hRequest );
   InternetCloseHandle( hConnect );
   InternetCloseHandle( hSession );
 end;

begin
 Result:= 0; // stub

 EnterCriticalSection( fCritSection );
 try
  if Assigned( fOwner ) then
  begin
   fHostName:= fOwner.fHostName;
   fPath:= fOwner.fPath;
   fPort:= fOwner.fPort;
   fUserName:= fOwner.fUserName;
   fPassword:= fOwner.fPassword;
   fPreConfigProxy:= fOwner.fPreConfigProxy;
   if ( not fPreConfigProxy ) then
   begin
    fProxySrv:= fOwner.fProxySrv;
    fProxyPort:= fOwner.fProxyPort;
   end;
  end // 'if Assigned( fOwner ) then'
  else Exit;
 finally
  LeaveCriticalSection( fCritSection );
 end;

 // initializing Wininet, settings some connection parameters
 if ( fPreConfigProxy ) then
  hSession:= InternetOpen( strUserAgent, INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0 )
 else
  begin
   if ( fProxySrv <> '' ) then
    hSession:= InternetOpen( strUserAgent, INTERNET_OPEN_TYPE_PROXY,
      PChar( 'http=' + fProxySrv + ':' + Int2Str( fProxyPort) ), nil, 0 )
   else
    hSession:= InternetOpen( strUserAgent, INTERNET_OPEN_TYPE_DIRECT, nil, nil, 0 );
  end;
  if ( hSession = nil ) then
  begin
   with ( fOwner^ ) do
    if Assigned( fOnError ) then fOnError( fOwner, GetLastError );
   Exit;
  end;

 // checking if thread must be terminated
 EnterCriticalSection( fCritSection );
 try
  if ( not Assigned( fOwner ) ) then Exit;
 finally
  LeaveCriticalSection( fCritSection );
 end;

 // connecting to http-server
 hConnect:= InternetConnect( hSession, PChar( fHostName ), fPort,
  PChar( fUserName ), PChar( fPassword ), INTERNET_SERVICE_HTTP, 0, 0 );
 if ( hConnect = nil ) then
 begin
  with ( fOwner^ ) do
   if Assigned( fOnError ) then fOnError( fOwner, GetLastError );
  CloseHandles;
  Exit;
 end;

 // checking if thread must be terminated
 EnterCriticalSection( fCritSection );
 try
  if ( not Assigned( fOwner ) ) then
  begin
   CloseHandles;
   Exit;
  end;
 finally
  LeaveCriticalSection( fCritSection );
 end;

 // prepare resource request to http-server
 // we're prefer HTTP/1.0 version but this parameter can be ignored by Wininet
 // see KB258425 (http://support.microsoft.com/kb/258425) for more details.
 hRequest:= HttpOpenRequest( hConnect, nil, PChar( fPath ), nil,
  nil, nil, INTERNET_FLAG_NO_UI + INTERNET_FLAG_PRAGMA_NOCACHE, 0);
 if ( hRequest = nil ) then
 begin
  with ( fOwner^ ) do
   if Assigned( fOnError ) then fOnError( fOwner, GetLastError );
  CloseHandles;
  Exit;
 end;
 // adding custom http headers to request
 {$IFDEF USE_CUSTOMHEADERS}
 with ( fOwner^ ) do
 if Assigned( fCHeaderList ) then
  with ( fCHeaderList^ ) do
  if ( Count > 0 ) then
   for i:= 1 to Count do
    HttpAddRequestHeaders( hRequest, PChar( Items[i-1] ), Length( Items[i-1] ), HTTP_ADDREQ_FLAG_ADD );
 {$ENDIF}
 // setting http headers 'connection type' field (don't allow persistent connection)
 if ( fPreConfigProxy or ( fProxySrv <> '' ) ) then
  HttpAddRequestHeaders( hRequest, strProxyConnectType, Length( strProxyConnectType ), HTTP_ADDREQ_FLAG_ADD )
 else
  HttpAddRequestHeaders( hRequest, strConnectType, Length( strConnectType ), HTTP_ADDREQ_FLAG_ADD );

 // checking if thread must be terminated
 EnterCriticalSection( fCritSection );
 try
  if ( not Assigned( fOwner ) ) then
  begin
   CloseHandles;
   Exit;
  end;
 finally
  LeaveCriticalSection( fCritSection );
 end;

 // send http request to server
 if ( not HttpSendRequest( hRequest, nil, 0, nil, 0 ) ) then
 begin
  with ( fOwner^ ) do
   if Assigned( fOnError ) then fOnError( fOwner, GetLastError );
  CloseHandles;
  Exit;
 end;

 // checking if thread must be terminated
 EnterCriticalSection( fCritSection );
 try
  if ( not Assigned( fOwner ) ) then
  begin
   CloseHandles;
   Exit;
  end;
 finally
  LeaveCriticalSection( fCritSection );
 end;

 // receiving headers (if event assigned)
 if Assigned( fOwner.fOnHeaderReceived ) then
 begin
   lpdwIndex:= 0; Buf:= nil;
   HttpQueryInfo( hRequest, HTTP_QUERY_RAW_HEADERS_CRLF, Buf, iBufSize, lpdwIndex );
   // NB: it's ok when 'unsufficient buffer' message received now
   iErrorCode:= GetLastError;

   if ( iErrorCode = ERROR_INSUFFICIENT_BUFFER ) then
   begin
    GetMem( Buf, iBufSize );
    lpdwIndex:= 0;
    try
     if ( HttpQueryInfo( hRequest, HTTP_QUERY_RAW_HEADERS_CRLF, Buf, iBufSize, lpdwIndex ) ) then
     with ( fOwner^ ) do
     begin
       fHeaderList.SetText( Buf, false );
      with ( fHeaderList^ ) do
       if ( Items[Count-1] = '' ) then Delete( Count-1 );
      fOnHeaderReceived( fOwner, fHeaderList );
     end  // 'if ( HttpQueryInfo( hRequest, HTTP_QUERY_RAW_HEADERS_CRLF, Buf, iBufSize, iReserved ) )'
     else
      with ( fOwner^ ) do
       if Assigned( fOnError ) then fOnError( fOwner, GetLastError );

    finally
     FreeMem( Buf );
    end;
   end  // 'if ( iErrorCode = ERROR_INSUFFICIENT_BUFFER )'
   else
    with ( fOwner^ ) do
     if Assigned( fOnError ) then fOnError( fOwner, iErrorCode );

   // checking if thread must be terminated
   EnterCriticalSection( fCritSection );
   try
    if ( not Assigned( fOwner ) ) then
    begin
     CloseHandles;
     Exit;
    end;
   finally
    LeaveCriticalSection( fCritSection );
   end;

 end; // 'if Assigned( fOnHeaderReceived )'

 // checking if thread must be terminated
 EnterCriticalSection( fCritSection );
 try
  if ( not Assigned( fOwner ) ) then
  begin
   CloseHandles;
   Exit;
  end;
 finally
  LeaveCriticalSection( fCritSection );
 end;

 // getting http status code
 iBufSize:= 16;
 iErrorCode:= 0;
 lpdwIndex:= 0;
 GetMem( Buf, iBufSize );
 try
  if ( HttpQueryInfo( hRequest, HTTP_QUERY_STATUS_CODE, Buf, iBufSize, lpdwIndex ) ) then
   iErrorCode:= Str2Int( StrPas( Buf ) )
  else
   with ( fOwner^ ) do
    if Assigned( fOnError ) then fOnError( fOwner, GetLastError );
 finally
  FreeMem( Buf );
 end;

 // checking if thread must be terminated
 EnterCriticalSection( fCritSection );
 try
  if ( not Assigned( fOwner ) ) then
  begin
   CloseHandles;
   Exit;
  end;
 finally
  LeaveCriticalSection( fCritSection );
 end;

 // checking if resource is available
 if ( ( Assigned( fOwner.fOnDownload ) and ( iErrorCode = HTTP_STATUS_OK {HTTP/1.1 200 OK} ) ) ) then
 begin
  iBufSize:= 16;
  lpdwIndex:= 0;
  iContentLen:= 0;
  GetMem( Buf, iBufSize );
  try
   if ( HttpQueryInfo( hRequest, HTTP_QUERY_CONTENT_LENGTH, Buf, iBufSize, lpdwIndex ) ) then
    // getting http content length
    iContentLen:= Str2Int( StrPas( Buf ) );
   // set iContentLen value to '-1' if not present or invalid
   if ( iContentLen <= 0 ) then  iContentLen:= -1;

   iReadCount:= 0;
   GetMem( fDataBuf, iDataBufSize );
   try
    // downloading resource
    with ( fOwner^ ) do
    while ( InternetReadFile( hRequest, fDataBuf, iDataBufSize, iNumRead ) ) do
     if ( iNumRead > 0 ) then
     begin
      // checking if thread must be terminated
      EnterCriticalSection( fCritSection );
      try
       if ( not Assigned( fOwner ) ) then Break;
      finally
       LeaveCriticalSection( fCritSection );
      end;

      // write received data to stream
      fDataStream.Write( fDataBuf^, iNumRead );
      Inc( iReadCount, iNumRead );
      // update download progress
      if Assigned( fOnProgress ) then fDLThread.Synchronize( On_UpdateProgress );
     end
     //  'if ( iNumRead > 0 )'
    else Break;

    // checking if thread must be terminated
    EnterCriticalSection( fCritSection );
    try
     if ( not Assigned( fOwner ) ) then
     begin
      CloseHandles;
      Exit;
     end;
    finally
     LeaveCriticalSection( fCritSection );
    end;

    // download complete
    with ( fOwner^ ) do
    begin
     fDataStream.Position:= 0;
     // call assigned event handler
     fOnDownload( @Self, fDataStream );
    end;

   finally
    FreeMem( fDataBuf );
   end;

  finally
   FreeMem( Buf );
  end;

 end  // 'if ( ( Assigned( fOnDownload ) and ( iErrorCode = HTTP_STATUS_OK {HTTP/1.1 200 OK} ) ) )'
 else
  if ( iErrorCode <> HTTP_STATUS_OK { HTTP/1.1 OK } ) then
   with ( fOwner^ ) do
    if Assigned( fOnError ) then fOnError( fOwner, ERROR_INTERNET_EXTENDED_ERROR );

 CloseHandles;
end;

//-----------------------------------------------------
destructor TDownloadWorker.Destroy;
begin
 fDLThread.Free;
 fHostName:= '';
 fPath:= '';
 fUserName:= '';
 fPassword:= '';
 fProxySrv:= '';
 EnterCriticalSection( fCritSection );
 try
  if Assigned( fOwner ) then
   fOwner.fBusy:= false;
 finally
  LeaveCriticalSection( fCritSection );
 end;
 DeleteCriticalSection( fCritSection );
 inherited;
end;

//-----------------------------------------------------


end.
