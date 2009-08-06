{$IFDEF FPC}
 {$mode delphi}
{$ENDIF}
unit mckHTTPDownload;
{

  ("`-''-/").___..--''"`-._
 `6_ 6  )   `-.  (     ).`-.__.`)
 (_Y_.)'  ._   )  `._ `. ``-..-'
  _..`--'_..-_/  /--'_.' ,'
(il).-''  (li).'  ((!.-'

 Download with HTTP-protocol (MCK Classes)

 Copyright © 2007-2008 Denis Fateyev (Danger)
 Website: <http://fateyev.com>
 E-Mail: <denis@fateyev.com>

}
interface

// ----------------------------------------------------------
uses
  Windows, Classes, Messages, Forms, SysUtils, mirror,
  KOL, KOLHTTPDownload {$IFDEF FPC}, LResources {$ENDIF};

// ----------------------------------------------------------
type
  PKOLHttpDownload =^TKOLHttpDownload;
  TKOLHttpDownload = class( TKOLObj )

  private
    fUserName: string;
    fUserPass: string;
    fProxyAddr: string;
    fProxyPort: Integer;
    fPreconfProxy: Boolean;

    fOnError: THTTPErrorEvent;
    fOnDownload: THTTPDownloadEvent;
    fOnProgress: THTTPProgressEvent;
    fOnHeaderReceived: THTTPHdrRecvEvent;

  public
    constructor Create( Owner: TComponent ); override;

  protected
    function  AdditionalUnits: string; override;
    procedure SetupFirst( SL: TStringList; const AName, AParent, Prefix: string ); override;
    procedure SetupLast( SL: TStringList; const AName, AParent, Prefix: string ); override;
    procedure AssignEvents( SL: TStringList; const AName: string ); override;

    procedure SetUserName( Value: string );
    procedure SetUserPass( Value: string );
    procedure SetProxyAddr( Value: string );
    procedure SetProxyPort( Value: Integer );
    procedure SetPreconfProxy( Value: Boolean );

    procedure SetOnDownload( Value: THTTPDownloadEvent );
    procedure SetOnError( Value: THTTPErrorEvent );
    procedure SetOnProgress( Value: THTTPProgressEvent );
    procedure SetOnHeaderReceived( Value: THTTPHdrRecvEvent );

  published
    property  authUserName : string read fUserName write SetUserName;
    property  authPassword : string read fUserPass write SetUserPass;

    property  ProxyServer : string read fProxyAddr write SetProxyAddr;
    property  ProxyPort   : Integer read fProxyPort write SetProxyPort;
    property  PreconfigProxy: Boolean read fPreconfProxy write SetPreconfProxy;

    property  OnDownload  : THTTPDownloadEvent read fOnDownload write SetOnDownload;
    property  OnProgress  : THTTPProgressEvent read fOnProgress write SetOnProgress;
    property  OnHeaderReceived  : THTTPHdrRecvEvent read fOnHeaderReceived write SetOnHeaderReceived;
    property  OnError  : THTTPErrorEvent read fOnError write SetOnError;

  end;

// ----------------------------------------------------------
procedure Register;

implementation

// ----------------------------------------------------------
procedure Register;
begin
  RegisterComponents('KOLAddons', [TKOLHttpDownload]);
end;

// ----------------------------------------------------------
{ TKOLHttpDownload }

constructor TKOLHttpDownload.Create;
begin
 inherited Create( Owner );
 fPreconfProxy:= false;
end;

// ----------------------------------------------------------
procedure TKOLHttpDownload.SetUserName;
begin
   fUserName:= Value;
   Change;
end;

// ----------------------------------------------------------
procedure TKOLHttpDownload.SetUserPass;
begin
   fUserPass:= Value;
   Change;
end;

// ----------------------------------------------------------
procedure TKOLHttpDownload.SetProxyAddr;
begin
   fProxyAddr:= Value;
   Change;
end;

// ----------------------------------------------------------
procedure TKOLHttpDownload.SetProxyPort;
begin
 if fProxyAddr = '' then fProxyPort:= 0
 else fProxyPort := Value;
   Change;
end;

// ----------------------------------------------------------
procedure TKOLHttpDownload.SetPreconfProxy;
begin
   fPreconfProxy:= Value;
   Change;
end;

// ----------------------------------------------------------
procedure TKOLHttpDownload.SetOnDownload;
begin
   fOnDownload:= Value;
   Change;
end;

// ----------------------------------------------------------
procedure TKOLHttpDownload.SetOnError;
begin
   fOnError:= Value;
   Change;
end;

// ----------------------------------------------------------
procedure TKOLHttpDownload.SetOnProgress;
begin
   fOnProgress:= Value;
   Change;
end;

// ----------------------------------------------------------
procedure TKOLHttpDownload.SetOnHeaderReceived;
begin
   fOnHeaderReceived := Value;
   Change;
end;

// ----------------------------------------------------------
function  TKOLHttpDownload.AdditionalUnits;
begin
   Result := ', KOLHTTPDownload';
end;

// ----------------------------------------------------------
procedure TKOLHttpDownload.SetupFirst(SL: TStringList; const AName,
  AParent, Prefix: String);
begin
  SL.Add( Prefix + AName + ' := NewHTTPDownload;' );
  if ( fPreconfProxy ) then
   SL.Add( Prefix + AName  + '.UsePreconfigProxy:= true; ')
  else
  begin
    if ( fProxyAddr <> '' ) then
    begin
      SL.Add( Prefix + AName + '.ProxyServer := ''' + fProxyAddr + ''';');
      if ( fProxyPort <> 0 ) then
      SL.Add( Prefix + AName + '.ProxyPort := ' + IntToStr( fProxyPort ) + ';');
    end;
  end;

  if ( fUserName <> '' ) or ( fUserPass <> '' ) then
   SL.Add( Prefix + AName + '.SetAuthInfo( ''' + fUserName + ''', ''' + fUserPass +''' );');
end;

// ----------------------------------------------------------
procedure TKOLHttpDownload.SetupLast(SL: TStringList; const AName,
  AParent, Prefix: String);
begin
   //
end;

// ----------------------------------------------------------
procedure TKOLHttpDownload.AssignEvents(SL: TStringList; const AName: String);
begin
  inherited;
  DoAssignEvents( SL, AName, [ 'OnDownload' ], [ @OnDownload ]);
  DoAssignEvents( SL, AName, [ 'OnProgress' ], [ @OnProgress ]);
  DoAssignEvents( SL, AName, [ 'OnHeaderReceived' ], [ @OnHeaderReceived ]);
  DoAssignEvents( SL, AName, [ 'OnError' ], [ @OnError ]);
end;

// ----------------------------------------------------------
{$IFDEF FPC}
initialization
 {$I mckHTTPDownload.lrs}
{$ENDIF}

// ----------------------------------------------------------

end.

