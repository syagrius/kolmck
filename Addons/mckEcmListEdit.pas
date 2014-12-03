unit mckEcmListEdit;

interface
{$I KOLDEF.INC}

uses
  Windows, KOL, Classes, Messages, Forms, SysUtils, mirror,
  mckCtrls, Graphics, KOLEcmListEdit,
//////////////////////////////////////////////////
     {$IFDEF _D6orHigher}                       //
     DesignIntf, DesignEditors, DesignConst,    //
     Variants,                                  //
     {$ELSE}                                    //
//////////////////////////////////////////////////
     DsgnIntf,
//////////////////////////////////////////////////////////
     {$ENDIF}                                           //

  mckLVColumnsEditor;

type
//  TOnEditText = procedure (Sender: PControl; ACol, ARow: Integer; var Value: String) of object;

  TKOLEcmListEdit = class(TKOLListView)
  private
    fDrawForbidden: TOnDrawItem;
    fListData: boolean;
    fOnGetText: TOnEditText;
    fOnPutText: TOnEditText;
    fOnEndEdit: TOnEndEdit;
    fOnColAdjust: TOnColAdjust;
    fOnEditChar: TOnEditChar;
    fOnCreateEdit: TOnCreateEdit;
    fLimStyle: TKOLListViewStyle;
    fOnDrawCell: TOnDrawCell;
    procedure SetOnGetText(const Value: TOnEditText);
    procedure SetOnPutText(const Value: TOnEditText);
    procedure SetOnEndEdit(const Value: TOnEndEdit);
    procedure SetOnColAdjust(const Value: TOnColAdjust);
    procedure SetOnEditChar(const Value: TOnEditChar);
    procedure SetOnCreateEdit(const Value: TOnCreateEdit);
    procedure SetLimStyle(const Value: TKOLListViewStyle);
    procedure SetOnDrawCell(const Value: TOnDrawCell);
  protected
    function  AdditionalUnits: string; override;
    procedure SetupFirst( SL: TStringList; const AName, AParent, Prefix: String ); override;
    procedure SetupLast( SL: TStringList; const AName, AParent, Prefix: String ); override;
    procedure AssignEvents( SL: TStringList; const AName: String ); override;
    function  SetupParams( const AName, AParent: TDelphiString ): TDelphiString; override;
   function  GetCaption: string;
    function  GetStyle: TKOLListViewStyle;
    function  GetOptions: TKOLListViewOptions;
    procedure SetOptions(v: TKOLListViewOptions);
  public
    constructor Create(Owner: TComponent); override;
    property IsListData: boolean read fListData write fListData;
    procedure UpdateColumns; virtual;
  published
    property Caption: string Read GetCaption;
    property Style: TKOLListViewStyle Read fLimStyle write SetLimStyle;
    property Options: TKOLListViewOptions read GetOptions write SetOptions;
    property OnGetEditText: TOnEditText read fOnGetText write SetOnGetText;
    property OnPutEditText: TOnEditText read fOnPutText write SetOnPutText;
    property OnStopEdit:  TOnEndEdit  read fOnEndEdit write SetOnEndEdit;
    property OnColAdjust: TOnColAdjust read fOnColAdjust write SetOnColAdjust;
    property OnEditChar: TOnEditChar read fOnEditChar write SetOnEditChar;
    property OnCreateEdit: TOnCreateEdit read fOnCreateEdit write SetOnCreateEdit;
    property OnDrawCell: TOnDrawCell read FOnDrawCell write SetOnDrawCell;
    // Hide in Object Inspector property OnDrawItem (made read only)
    property OnDrawItem: TOnDrawItem read fDrawForbidden;
  end;
  procedure Register;

implementation

//{$R EcmListEdit.dcr}

constructor TKOLEcmListEdit.Create;
begin
   inherited;
   inherited Style   := lvsDetail;
   inherited Options := [{lvoRowSelect,}lvoHideSel,lvoGridLines,lvoOwnerDrawFixed];
//   Font.FontCharset  := 204;
end;


function  TKOLEcmListEdit.AdditionalUnits;
begin
   Result := ', KOLEcmListEdit';
end;

procedure TKOLEcmListEdit.SetupFirst;
begin
//   if @fOnGetText <> nil then
//     SL.Add( '      PEcmListEdit(' + AName + '.CustomObj).OnGetEditText := Result.' +
  inherited;
end;

procedure TKOLEcmListEdit.SetupLast;
begin
   inherited AssignEvents(SL, AName);
   if @fOnGetText <> nil then
     SL.Add( '      PEcmListEdit(' + AName + '.CustomObj).OnGetEditText := Result.' +
       ParentForm.MethodName( @OnGetEditText ) + ';' );
   if @fOnPutText <> nil then
     SL.Add( '      PEcmListEdit(' + AName + '.CustomObj).OnPutEditText := Result.' +
       ParentForm.MethodName( @OnPutEditText ) + ';' );
   if @fOnEndEdit <> nil then
     SL.Add( '      PEcmListEdit(' + AName + '.CustomObj).OnStopEdit := Result.' +
       ParentForm.MethodName( @OnStopEdit ) + ';' );
   if @fOnColAdjust <> nil then
     SL.Add( '      PEcmListEdit(' + AName + '.CustomObj).OnColAdjust := Result.' +
       ParentForm.MethodName( @OnColAdjust ) + ';' );
   if @fOnEditChar <> nil then
     SL.Add( '      PEcmListEdit(' + AName + '.CustomObj).OnEditChar := Result.' +
       ParentForm.MethodName( @OnEditChar ) + ';' );
   if @fOnCreateEdit <> nil then
     SL.Add( '      PEcmListEdit(' + AName + '.CustomObj).OnCreateEdit := Result.' +
       ParentForm.MethodName( @OnCreateEdit ) + ';' );
   if @fOnDrawCell <> nil then
     SL.Add( '      PEcmListEdit(' + AName + '.CustomObj).OnDrawCell := Result.' +
       ParentForm.MethodName( @OnDrawCell ) + ';' );
end;

procedure TKOLEcmListEdit.AssignEvents;
begin
   inherited;
end;

function TKOLEcmListEdit.GetCaption;
begin
   Result := inherited Caption;
end;

function TKOLEcmListEdit.GetStyle;
begin
//  Result := lvsDetail;
  Result := fLimStyle;
end;

function TKOLEcmListEdit.GetOptions;
begin
   Result := inherited Options;
end;

procedure TKOLEcmListEdit.SetOptions;
begin
   inherited Options := v + [{lvoRowSelect,}lvoHideSel,lvoOwnerDrawFixed];
end;

procedure Register;
begin
  RegisterComponents('KOLAddons', [TKOLEcmListEdit]);
  RegisterComponentEditor( TKOLEcmListEdit, TKOLLVColumnsEditor );
end;

procedure TKOLEcmListEdit.UpdateColumns;
begin
  Change;
end;

procedure TKOLEcmListEdit.SetOnGetText(const Value: TOnEditText);
begin
  if @fOnGetText <> @Value then begin
    fOnGetText := Value;
    Change();
  end;
end;

procedure TKOLEcmListEdit.SetOnPutText(const Value: TOnEditText);
begin
  if @fOnPutText <> @Value then begin
    fOnPutText := Value;
    Change();
  end;
end;

procedure TKOLEcmListEdit.SetOnEndEdit(const Value: TOnEndEdit);
begin
  if @fOnEndEdit <> @Value then begin
    fOnEndEdit := Value;
    Change();
  end;
end;

procedure TKOLEcmListEdit.SetOnColAdjust(const Value: TOnColAdjust);
begin
  if @fOnColAdjust <> @Value then begin
    fOnColAdjust := Value;
    Change;
  end;
end;

procedure TKOLEcmListEdit.SetOnEditChar(const Value: TOnEditChar);
begin
  if @fOnEditChar <> @Value then begin
    fOnEditChar := Value;
    Change();
  end;
end;

procedure TKOLEcmListEdit.SetOnDrawCell(const Value: TOnDrawCell);
begin
  if @FOnDrawCell <> @Value then begin
    FOnDrawCell:= Value;
    Change();
  end;
end;

function TKOLEcmListEdit.SetupParams(const AName, AParent: TDelphiString): TDelphiString;
begin
  Result := inherited SetupParams(AName,AParent)
end;

procedure TKOLEcmListEdit.SetOnCreateEdit(const Value: TOnCreateEdit);
begin
  if @fOnCreateEdit <> @Value then begin
    fOnCreateEdit := Value;
    Change();
  end;
end;


procedure TKOLEcmListEdit.SetLimStyle(const Value: TKOLListViewStyle);
begin
  if (Value <> fLimStyle) and ((Value = lvsDetail) or (Value = lvsDetailNoHeader)) then begin
    fLimStyle := Value;
    inherited Style := fLimStyle;
  end;
end;

end.

