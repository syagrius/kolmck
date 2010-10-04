//////////////////////////////////////////////////////////////////////////////////
//										//
//										//
//		TKOLWebBrowser v1.0						//
//										//
//	Author: Dimaxx (dimaxx@atnet.ru)					//
//										//
//										//
//////////////////////////////////////////////////////////////////////////////////

unit KOLWebBrowser;

interface

uses KOL, KOLSHDocVw;

type
  TKOLWebBrowser = PWebBrowser;
  PKOLWebBrowser = PWebBrowser;

function NewKOLWebBrowser(AOwner: PControl): PKOLWebBrowser;

implementation

function NewKOLWebBrowser;
begin
  New(Result,CreateParented(AOwner));
end;

end.

