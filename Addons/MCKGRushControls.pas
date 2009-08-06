unit MCKGRushControls;

//  file: MCKGRushControls.pas
//  file version: 0.35
//  last modified: 14.02.06
//  package: GRushControls
//  author: Karpinskyj Alexandr aka homm
//      mailto: homm86@mail.ru
//      My humble Web-Page: http://www.homm86.narod.ru

{$I KOLDEF.INC}

interface

uses    Windows,
        Messages,
        Classes,
        Controls,
        KOL,
        KOLGRushControls,
        mirror,
        mckCtrls,
        Graphics,
        Forms,
        SysUtils,
        mckObjs,
        dialogs,
        {$IFDEF _D6orHigher}                      
        DesignIntf,
        DesignEditors,
        DesignConst,   
        Variants                                  
        {$ELSE}                                   
        DsgnIntf
        {$ENDIF};

{$R *.res}

const
    clMoneyGreen = TColor($C0DCC0);
    clSkyBlue = TColor($F0CAA6);
    clCream = TColor($F0FBFF);
    clMedGray = TColor($A4A0A0);

type
    TKOLCustomControl_ = class ( TKOLCustomControl ) end;
    TKOLPaintState = packed class ( TPersistent )
    protected
        fOwner:             TComponent;
        fColorFrom:         TColor;
        fColorTo:           TColor;
        fColorOuter:        TColor;
        fColorText:         TColor;
        fColorShadow:       TColor;
        fBorderColor:       TColor;
        fBorderRoundWidth:  DWORD;
        fBorderRoundHeight: DWORD;
        fBorderWidth:       DWORD;
        fGradientStyle:     TGRushGradientStyle;
        fShadowOffset:      Integer;
        fGlyphItemX:        DWORD;
        fGlyphItemY:        DWORD;
    public
        procedure Assign(Source: TPersistent); override;
        constructor Create (aOwner: TComponent);
        procedure Change;
        procedure SetColorFrom( Value: TColor );
        procedure SetColorTo( Value: TColor );
        procedure SetColorOuter( Value: TColor );
        procedure SetColorText( Value: TColor );
        procedure SetColorShadow( Value: TColor );
        procedure SetBorderColor( Value: TColor );
        procedure SetBorderRoundWidth( Value: DWORD );
        procedure SetBorderRoundHeight( Value: DWORD );
        procedure SetBorderWidth( Value: DWORD );
        procedure SetGradientStyle( Value: TGRushGradientStyle );
        procedure SetShadowOffset( Value: Integer );
        procedure SetGlyphItemX( Value: DWORD );
        procedure SetGlyphItemY( Value: DWORD );
    published
        property ColorFrom: TColor read fColorFrom write SetColorFrom;
        property ColorTo: TColor read fColorTo write SetColorTo;
        property ColorOuter: TColor read fColorOuter write SetColorOuter;
        property ColorText: TColor read fColorText write SetColorText;
        property ColorShadow: TColor read fColorShadow write SetColorShadow;
        property BorderColor: TColor read fBorderColor write SetBorderColor;
        property BorderRoundWidth: DWORD read fBorderRoundWidth write SetBorderRoundWidth;
        property BorderRoundHeight: DWORD read fBorderRoundHeight write SetBorderRoundHeight;
        property BorderWidth: DWORD read fBorderWidth write SetBorderWidth;
        property GradientStyle: TGRushGradientStyle read fGradientStyle write SetGradientStyle;
        property ShadowOffset: Integer read fShadowOffset write SetShadowOffset;
        property GlyphItemX: DWORD read fGlyphItemX write SetGlyphItemX default 0;
        property GlyphItemY: DWORD read fGlyphItemY write SetGlyphItemY default 0;
    end;

//************************************************************

    TKOLRect = class ( TPersistent )
    protected
        fOwner:             TComponent;
        fLeft:              Integer;
        fTop:               Integer;
        fRight:             Integer;
        fBottom:            Integer;
    public
        procedure Assign(Source: TPersistent); override;
        constructor Create (aOwner: TComponent; const aRect: TRect);
        procedure Change;
        procedure SetLeft( Value: Integer );
        procedure SetTop( Value: Integer );
        procedure SetRight( Value: Integer );
        procedure SetBottom( Value: Integer );
    published
        property Left: Integer read fLeft write SetLeft;
        property Top: Integer read fTop write SetTop;
        property Right: Integer read fRight write SetRight;
        property Bottom: Integer read fBottom write SetBottom;
    end;

//************************************************************

    TKOLGRushStyles = class ( TPersistent )
    protected
        fOwner:             TComponent;
        fPSDef:             TKOLPaintState;
        fPSOver:            TKOLPaintState;
        fPSDown:            TKOLPaintState;
        fPSDis:             TKOLPaintState;
        fContentOffsets:    TKOLRect;
        fGlyphWidth:        DWORD;
        fGlyphHeight:       DWORD;
        fSplitterDotsCount: DWORD;
        fCheckMetric:       DWORD;
        fColorCheck:        TColor;
        fGlyphVAlign:       KOL.TVerticalAlign;
        fGlyphHAlign:       TGRushHAlign;
        fTextVAlign:        KOL.TVerticalAlign;
        fTextHAlign:        TGRushHAlign;
        fDrawGlyph:         Boolean;
        fDrawText:          Boolean;
        fDrawFocusRect:     Boolean;
        fDrawProgress:      Boolean;
        fDrawProgressRect:  Boolean;
        fGlyphAttached:     Boolean;
        fCropTopFirst:      Boolean;
        fAntiAliasing:      Boolean;
        fProgressVertical:  Boolean;
        fUpdateSpeed:       TGRushSpeed;
        fSpacing:           DWORD;
    public
        procedure Assign(Source: TPersistent); override;
        constructor Create (aOwner: TComponent);
        destructor Destroy; override;
        property Owner: TComponent read fOwner;
        procedure Change;
        procedure SetGlyphWidth ( Value: DWORD );
        procedure SetGlyphHeight ( Value: DWORD );
        procedure SetSplitterDotsCount ( Value: DWORD );
        procedure SetCheckMetric ( Value: DWORD );
        procedure SetColorCheck ( Value: TColor );
        procedure SetGlyphVAlign ( Value: KOL.TVerticalAlign );
        procedure SetGlyphHAlign ( Value: TGRushHAlign );
        procedure SetTextVAlign ( Value: KOL.TVerticalAlign );
        procedure SetTextHAlign ( Value: TGRushHAlign );
        procedure SetDrawGlyph ( Value: Boolean );
        procedure SetDrawText ( Value: Boolean );
        procedure SetDrawFocusRect ( Value: Boolean );
        procedure SetDrawProgress ( Value: Boolean );
        procedure SetDrawProgressRect ( Value: Boolean );
        procedure SetGlyphAttached ( Value: Boolean );
        procedure SetCropTopFirst ( Value: Boolean );
        procedure SetAntiAliasing ( Value: Boolean );
        procedure SetProgressVertical ( Value: Boolean );
        procedure SetUpdateSpeed ( Value: TGRushSpeed );
        procedure SetSpacing ( Value: DWORD );

        procedure SetUpProgressVertical ( Value: Boolean );
        procedure SetUpSplitterAlign ( Value: Boolean );
    end;

//************************************************************

    TKOLGRushButtonStyles = class (TKOLGRushStyles)
    published
    {-} property DefPaintState: TKOLPaintState read fPSDef write fPSDef;
    {-} property OverPaintState: TKOLPaintState read fPSOver write fPSOver;
    {-} property DownPaintState: TKOLPaintState read fPSDown write fPSDown;
    {-} property DisPaintState: TKOLPaintState read fPSDis write fPSDis;
    {-} property ContentOffsets: TKOLRect read fContentOffsets write fContentOffsets;
    {-} property GlyphWidth: DWORD read fGlyphWidth write SetGlyphWidth default 0;
    {-} property GlyphHeight: DWORD read fGlyphHeight write SetGlyphHeight default 0;
        //property CheckMetric: DWORD read fCheckMetric write SetCheckMetric;
        //property ColorCheck: TColor read fColorCheck write SetColorCheck;
    {-} property GlyphVAlign: KOL.TVerticalAlign read fGlyphVAlign write SetGlyphVAlign default KOL.vaCenter;
    {-} property GlyphHAlign: TGRushHAlign read fGlyphHAlign write SetGlyphHAlign default haLeft;
    {-} property TextVAlign: KOL.TVerticalAlign read fTextVAlign write SetTextVAlign default KOL.vaCenter;
    {-} property TextHAlign: TGRushHAlign read fTextHAlign write SetTextHAlign default haCenter;
    {-} property DrawGlyph: Boolean read fDrawGlyph write SetDrawGlyph default TRUE;
    {-} property DrawText: Boolean read fDrawText write SetDrawText default TRUE;
        property DrawFocusRect: Boolean read fDrawFocusRect write SetDrawFocusRect default TRUE;
        //property DrawProgress: Boolean read fDrawProgress write SetDrawProgress;
        //property DrawProgressRect: Boolean read fDrawProgressRect write SetDrawProgressRect;
    {-} property GlyphAttached: Boolean read fGlyphAttached write SetGlyphAttached default FALSE;
    {-} property CropTopFirst: Boolean read fCropTopFirst write SetCropTopFirst default TRUE;
    {-} property AntiAliasing: Boolean read fAntiAliasing write SetAntiAliasing default TRUE;
        //property ProgressVertical: Boolean read fProgressVertical write SetProgressVertical;
        property UpdateSpeed: TGRushSpeed read fUpdateSpeed write SetUpdateSpeed default usFast;
    {-} property Spacing: DWORD read fSpacing write SetSpacing default 5;
    end;

//************************************************************

    TKOLGRushPanelStyles = class (TKOLGRushStyles)
    public
        constructor Create (aOwner: TComponent);
    published
    {-} property DefPaintState: TKOLPaintState read fPSDef write fPSDef;
    {-} //property OverPaintState: TKOLPaintState read fPSOver;
    {-} //property DownPaintState: TKOLPaintState read fPSDown;
    {-} property DisPaintState: TKOLPaintState read fPSDis write fPSDis;
    {-} property ContentOffsets: TKOLRect read fContentOffsets write fContentOffsets;
    {-} property GlyphWidth: DWORD read fGlyphWidth write SetGlyphWidth default 0;
    {-} property GlyphHeight: DWORD read fGlyphHeight write SetGlyphHeight default 0;
        //property CheckMetric: DWORD read fCheckMetric write SetCheckMetric;
        //property ColorCheck: TColor read fColorCheck write SetColorCheck;
    {-} property GlyphVAlign: KOL.TVerticalAlign read fGlyphVAlign write SetGlyphVAlign default KOL.vaCenter;
    {-} property GlyphHAlign: TGRushHAlign read fGlyphHAlign write SetGlyphHAlign default haLeft;
    {-} property TextVAlign: KOL.TVerticalAlign read fTextVAlign write SetTextVAlign default KOL.vaTop;
    {-} property TextHAlign: TGRushHAlign read fTextHAlign write SetTextHAlign default haCenter;
    {-} property DrawGlyph: Boolean read fDrawGlyph write SetDrawGlyph default TRUE;
    {-} property DrawText: Boolean read fDrawText write SetDrawText default TRUE;
        //property DrawFocusRect: Boolean read fDrawFocusRect write SetDrawFocusRect;
        //property DrawProgress: Boolean read fDrawProgress write SetDrawProgress;
        //property DrawProgressRect: Boolean read fDrawProgressRect write SetDrawProgressRect;
    {-} property GlyphAttached: Boolean read fGlyphAttached write SetGlyphAttached default FALSE;
    {-} property CropTopFirst: Boolean read fCropTopFirst write SetCropTopFirst default TRUE;
    {-} property AntiAliasing: Boolean read fAntiAliasing write SetAntiAliasing default TRUE;
        //property ProgressVertical: Boolean read fProgressVertical write SetProgressVertical;
        //property UpdateSpeed: TGRushSpeed read fUpdateSpeed write SetUpdateSpeed;
    {-} property Spacing: DWORD read fSpacing write SetSpacing default 5;
    end;

//************************************************************

    TKOLGRushCheckBoxStyles = class (TKOLGRushStyles)
    public
        constructor Create (aOwner: TComponent);
    published
    {-} property DefPaintState: TKOLPaintState read fPSDef write fPSDef;
    {-} property OverPaintState: TKOLPaintState read fPSOver write fPSOver;
    {-} property DownPaintState: TKOLPaintState read fPSDown write fPSDown;
    {-} property DisPaintState: TKOLPaintState read fPSDis write fPSDis;
    {-} property ContentOffsets: TKOLRect read fContentOffsets write fContentOffsets;
    {-} property GlyphWidth: DWORD read fGlyphWidth write SetGlyphWidth default 0;
    {-} property GlyphHeight: DWORD read fGlyphHeight write SetGlyphHeight default 0;
        property CheckMetric: DWORD read fCheckMetric write SetCheckMetric default 13;
        property ColorCheck: TColor read fColorCheck write SetColorCheck default integer($F3706C);
    {-} property GlyphVAlign: KOL.TVerticalAlign read fGlyphVAlign write SetGlyphVAlign default KOL.vaCenter;
    {-} property GlyphHAlign: TGRushHAlign read fGlyphHAlign write SetGlyphHAlign default haLeft;
    {-} property TextVAlign: KOL.TVerticalAlign read fTextVAlign write SetTextVAlign default KOL.vaCenter;
    {-} property TextHAlign: TGRushHAlign read fTextHAlign write SetTextHAlign default haLeft;
    {-} property DrawGlyph: Boolean read fDrawGlyph write SetDrawGlyph default TRUE;
    {-} property DrawText: Boolean read fDrawText write SetDrawText default TRUE;
        property DrawFocusRect: Boolean read fDrawFocusRect write SetDrawFocusRect default TRUE;
        //property DrawProgress: Boolean read fDrawProgress write SetDrawProgress;
        //property DrawProgressRect: Boolean read fDrawProgressRect write SetDrawProgressRect;
    {-} property GlyphAttached: Boolean read fGlyphAttached write SetGlyphAttached default FALSE;
    {-} property CropTopFirst: Boolean read fCropTopFirst write SetCropTopFirst default TRUE;
    {-} property AntiAliasing: Boolean read fAntiAliasing write SetAntiAliasing default TRUE;
        //property ProgressVertical: Boolean read fProgressVertical write SetProgressVertical;
        property UpdateSpeed: TGRushSpeed read fUpdateSpeed write SetUpdateSpeed default usFast;
    {-} property Spacing: DWORD read fSpacing write SetSpacing default 5;
    end;

//************************************************************

    TKOLGRushRadioBoxStyles = class (TKOLGRushStyles)
    public
        constructor Create (aOwner: TComponent);
    published
    {-} property DefPaintState: TKOLPaintState read fPSDef write fPSDef;
    {-} property OverPaintState: TKOLPaintState read fPSOver write fPSOver;
    {-} property DownPaintState: TKOLPaintState read fPSDown write fPSDown;
    {-} property DisPaintState: TKOLPaintState read fPSDis write fPSDis;
    {-} property ContentOffsets: TKOLRect read fContentOffsets write fContentOffsets;
    {-} property GlyphWidth: DWORD read fGlyphWidth write SetGlyphWidth default 0;
    {-} property GlyphHeight: DWORD read fGlyphHeight write SetGlyphHeight default 0;
        property CheckMetric: DWORD read fCheckMetric write SetCheckMetric default 13;
        property ColorCheck: TColor read fColorCheck write SetColorCheck default integer($F3706C);
    {-} property GlyphVAlign: KOL.TVerticalAlign read fGlyphVAlign write SetGlyphVAlign default KOL.vaCenter;
    {-} property GlyphHAlign: TGRushHAlign read fGlyphHAlign write SetGlyphHAlign default haLeft;
    {-} property TextVAlign: KOL.TVerticalAlign read fTextVAlign write SetTextVAlign default KOL.vaCenter;
    {-} property TextHAlign: TGRushHAlign read fTextHAlign write SetTextHAlign default haLeft;
    {-} property DrawGlyph: Boolean read fDrawGlyph write SetDrawGlyph default TRUE;
    {-} property DrawText: Boolean read fDrawText write SetDrawText default TRUE;
        property DrawFocusRect: Boolean read fDrawFocusRect write SetDrawFocusRect default TRUE;
        //property DrawProgress: Boolean read fDrawProgress write SetDrawProgress;
        //property DrawProgressRect: Boolean read fDrawProgressRect write SetDrawProgressRect;
    {-} property GlyphAttached: Boolean read fGlyphAttached write SetGlyphAttached default FALSE;
    {-} property CropTopFirst: Boolean read fCropTopFirst write SetCropTopFirst default TRUE;
    {-} property AntiAliasing: Boolean read fAntiAliasing write SetAntiAliasing default TRUE;
        //property ProgressVertical: Boolean read fProgressVertical write SetProgressVertical;
        property UpdateSpeed: TGRushSpeed read fUpdateSpeed write SetUpdateSpeed default usFast;
    {-} property Spacing: DWORD read fSpacing write SetSpacing default 5;
    end;

//************************************************************

    TKOLGRushSplitterStyles = class (TKOLGRushStyles)
    public
        constructor Create (aOwner: TComponent);
    published
    {-} property DefPaintState: TKOLPaintState read fPSDef write fPSDef;
    {-} property OverPaintState: TKOLPaintState read fPSOver write fPSOver;
    {-} property DownPaintState: TKOLPaintState read fPSDown write fPSDown;
    {-} property DisPaintState: TKOLPaintState read fPSDis write fPSDis;
    {-} property ContentOffsets: TKOLRect read fContentOffsets write fContentOffsets;
    {-} property GlyphWidth: DWORD read fGlyphWidth write SetGlyphWidth default 0;
    {-} property GlyphHeight: DWORD read fGlyphHeight write SetGlyphHeight default 0;
        //property CheckMetric: DWORD read fCheckMetric write SetCheckMetric;
        //property ColorCheck: TColor read fColorCheck write SetColorCheck;
    {-} property GlyphVAlign: KOL.TVerticalAlign read fGlyphVAlign write SetGlyphVAlign default KOL.vaCenter;
    {-} property GlyphHAlign: TGRushHAlign read fGlyphHAlign write SetGlyphHAlign default haLeft;
    {-} property TextVAlign: KOL.TVerticalAlign read fTextVAlign write SetTextVAlign default KOL.vaCenter;
    {-} property TextHAlign: TGRushHAlign read fTextHAlign write SetTextHAlign default haCenter;
    {-} property DrawGlyph: Boolean read fDrawGlyph write SetDrawGlyph default TRUE;
    {-} property DrawText: Boolean read fDrawText write SetDrawText default TRUE;
        //property DrawFocusRect: Boolean read fDrawFocusRect write SetDrawFocusRect;
        //property DrawProgress: Boolean read fDrawProgress write SetDrawProgress;
        //property DrawProgressRect: Boolean read fDrawProgressRect write SetDrawProgressRect;
    {-} property GlyphAttached: Boolean read fGlyphAttached write SetGlyphAttached default FALSE;
    {-} property CropTopFirst: Boolean read fCropTopFirst write SetCropTopFirst default TRUE;
    {-} property AntiAliasing: Boolean read fAntiAliasing write SetAntiAliasing default TRUE;
        //property ProgressVertical: Boolean read fProgressVertical write SetProgressVertical;
        property UpdateSpeed: TGRushSpeed read fUpdateSpeed write SetUpdateSpeed default usVeryFast;
    {-} property Spacing: DWORD read fSpacing write SetSpacing default 5;
    {-} property SplitterDotsCount: DWORD read fSplitterDotsCount write SetSplitterDotsCount default 16;
    end;

//************************************************************

    TKOLGRushProgressBarStyles = class (TKOLGRushStyles)
    public
        constructor Create (aOwner: TComponent);
    published
    {-} property DefPaintState: TKOLPaintState read fPSDef write fPSDef;
    {-} //property OverPaintState: TKOLPaintState read fPSOver;
    {-} //property DownPaintState: TKOLPaintState read fPSDown;
    {-} property DisPaintState: TKOLPaintState read fPSDis write fPSDis;
    {-} property ContentOffsets: TKOLRect read fContentOffsets write fContentOffsets;
    {-} property GlyphWidth: DWORD read fGlyphWidth write SetGlyphWidth default 0;
    {-} property GlyphHeight: DWORD read fGlyphHeight write SetGlyphHeight default 0;
        //property CheckMetric: DWORD read fCheckMetric write SetCheckMetric;
        //property ColorCheck: TColor read fColorCheck write SetColorCheck;
    {-} property GlyphVAlign: KOL.TVerticalAlign read fGlyphVAlign write SetGlyphVAlign default KOL.vaCenter;
    {-} property GlyphHAlign: TGRushHAlign read fGlyphHAlign write SetGlyphHAlign default haLeft;
    {-} property TextVAlign: KOL.TVerticalAlign read fTextVAlign write SetTextVAlign default KOL.vaCenter;
    {-} property TextHAlign: TGRushHAlign read fTextHAlign write SetTextHAlign default haCenter;
    {-} property DrawGlyph: Boolean read fDrawGlyph write SetDrawGlyph default TRUE;
    {-} property DrawText: Boolean read fDrawText write SetDrawText default TRUE;
        //property DrawFocusRect: Boolean read fDrawFocusRect write SetDrawFocusRect;
        property DrawProgress: Boolean read fDrawProgress write SetDrawProgress default TRUE;
        property DrawProgressRect: Boolean read fDrawProgressRect write SetDrawProgressRect default TRUE;
    {-} property GlyphAttached: Boolean read fGlyphAttached write SetGlyphAttached default FALSE;
    {-} property CropTopFirst: Boolean read fCropTopFirst write SetCropTopFirst default TRUE;
    {-} property AntiAliasing: Boolean read fAntiAliasing write SetAntiAliasing default TRUE;
        property ProgressVertical: Boolean read fProgressVertical write SetProgressVertical default FALSE;
        //property UpdateSpeed: TGRushSpeed read fUpdateSpeed write SetUpdateSpeed;
    {-} property Spacing: DWORD read fSpacing write SetSpacing default 5;
    end;

//************************************************************

    TKOLGRushImageCollectionImageType = (None, BMP_GIF_JPG, PNG);
    TKOLGRushImageCollection = class (TKOLObj)
    protected
        fImageType: TKOLGRushImageCollectionImageType;
        fItemWidth: DWORD;
        fItemHeight: DWORD;
        fDataStream: TMemoryStream;
        function GetResourceName: String;
        function GetResourceFileName: String;
        procedure SetImageType(Value: TKOLGRushImageCollectionImageType);
        procedure SetItemWidth(Value: DWORD);
        procedure SetItemHeight(Value: DWORD);

        procedure DefineProperties(Filer: TFiler); override;
        procedure ReadData(Stream: Classes.TStream);
        procedure WriteData(Stream: Classes.TStream);

        function AdditionalUnits: String; override;
        procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: String); override;
        procedure P_SetupFirst(SL: TStringList; const AName, AParent, Prefix: String); override;
        procedure SetupLast( SL: TStringList; const AName, AParent, Prefix: String ); override;
        procedure P_SetupLast( SL: TStringList; const AName, AParent, Prefix: String );override;

        procedure AssignEvents( SL: TStringList; const AName: String ); override;
        function P_AssignEvents( SL: TStringList; const AName: String; CheckOnly: Boolean ): Boolean; override;
    public
        property DataStream: TMemoryStream read fDataStream write fDataStream;
        function Pcode_Generate: Boolean; override;
        function TypeName: String; override;
        constructor Create( AOwner: TComponent ); override;
        destructor Destroy; override;
        function LoadBitmap: PBitmap;
    published
        property ItemWidth: DWORD read fItemWidth write SetItemWidth default 0;
        property ItemHeight: DWORD read fItemHeight write SetItemHeight default 0;
        property ImageType: TKOLGRushImageCollectionImageType read fImageType write SetImageType default None;
    end;

//************************************************************

    TKOLGRushButton = class (TKOLButton)
    protected
        fStyles: TKOLGRushButtonStyles;
        fOnRecalcRects: TOnRecalcRects;
        fImageCollection: TKOLGRushImageCollection;

        fDummyProperty: Integer;
        function CanNotChangeFontColor: Boolean; override;
        function DefaultParentColor: Boolean; override;
        function CanChangeColor: Boolean; override;
        procedure SetOnRecalcRects(const Value: TOnRecalcRects);
        procedure SetImageCollection(const Value: TKOLGRushImageCollection);

        function TypeName: String; override;
        function AdditionalUnits: String; override;
        procedure SetupConstruct(SL: TStringList; const AName, AParent, Prefix: String); override;
        procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: String); override;
        procedure P_SetupFirst(SL: TStringList; const AName, AParent, Prefix: String); override;
        procedure AssignEvents( SL: TStringList; const AName: String ); override;
        function P_AssignEvents( SL: TStringList; const AName: String;
            CheckOnly: Boolean ): Boolean; override;
        procedure SetStyles (Val: TKOLGRushButtonStyles);
    public
        constructor Create( AOwner: TComponent ); override;
        destructor Destroy; override;
        function Pcode_Generate: Boolean; override;
        procedure NotifyLinkedComponent( Sender: TObject; Operation: TNotifyOperation ); override;
    published
        property GRushStyles: TKOLGRushButtonStyles read fStyles write SetStyles;
        property OnRecalcRects: TOnRecalcRects read fOnRecalcRects write SetOnRecalcRects;
        property imagecollection: TKOLGRushImageCollection read fImageCollection write SetImageCollection;
        property Transparent;

        property ParentColor: Integer read fDummyProperty;
        property VerticalAlign: Integer read fDummyProperty;
        property TextAlign: Integer read fDummyProperty;
        property Ctl3D: Integer read fDummyProperty;
        property Flat: Integer read fDummyProperty;
        property EraseBackGround: Integer read fDummyProperty;
        property LikeSpeedButton: Integer read fDummyProperty;
        property Windowed: Integer read fDummyProperty;
        property Color: Integer read fDummyProperty;
        property Image: Integer read fDummyProperty;
        property WordWrap: Integer read fDummyProperty;
    end;

//************************************************************

    TKOLGRushPanel = class (TKOLPanel)
    protected
        fStyles: TKOLGRushPanelStyles;
        fOnRecalcRects: TOnRecalcRects;
        fImageCollection: TKOLGRushImageCollection;

        fDummyProperty: Integer;
        procedure SetOnRecalcRects(const Value: TOnRecalcRects);
        procedure SetImageCollection(const Value: TKOLGRushImageCollection);

        function ClientMargins: TRect; override;
        function TypeName: String; override;
        function AdditionalUnits: String; override;
        function SetupParams(const AName, AParent: String): String; override;
        function P_SetupParams( const AName, AParent: String; var nparams: Integer ): String; override;
        procedure SetupConstruct(SL: TStringList; const AName, AParent, Prefix: String); override;
        procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: String); override;
        procedure P_SetupFirst(SL: TStringList; const AName, AParent, Prefix: String); override;
        procedure AssignEvents( SL: TStringList; const AName: String ); override;
        function P_AssignEvents( SL: TStringList; const AName: String;
            CheckOnly: Boolean ): Boolean; override;
        procedure SetStyles (Val: TKOLGRushPanelStyles);
    public
        constructor Create( AOwner: TComponent ); override;
        destructor Destroy; override;
        function Pcode_Generate: Boolean; override;
        procedure NotifyLinkedComponent( Sender: TObject; Operation: TNotifyOperation ); override;
    published
        property GRushStyles: TKOLGRushPanelStyles read fStyles write SetStyles;
        property OnRecalcRects: TOnRecalcRects read fOnRecalcRects write SetOnRecalcRects;
        property imagecollection: TKOLGRushImageCollection read fImageCollection write SetImageCollection;

        property ParentColor: Integer read fDummyProperty;
        property VerticalAlign: Integer read fDummyProperty;
        property TextAlign: Integer read fDummyProperty;
        property Ctl3D: Integer read fDummyProperty;
        property EdgeStyle: Integer read fDummyProperty;
        property EraseBackGround: Integer read fDummyProperty;
        property ShowAccelChar: Integer read fDummyProperty;
        property Color: Integer read fDummyProperty;
        property Brush: Integer read fDummyProperty;
    end;

//************************************************************

    TKOLGRushCheckBox = class (TKOLCheckBox)
    protected
        fStyles: TKOLGRushCheckBoxStyles;
        fOnRecalcRects: TOnRecalcRects;
        fImageCollection: TKOLGRushImageCollection;

        fDummyProperty: Integer;
        procedure SetOnRecalcRects(const Value: TOnRecalcRects);
        procedure SetImageCollection(const Value: TKOLGRushImageCollection);

        function TypeName: String; override;
        function AdditionalUnits: String; override;
        procedure SetupConstruct(SL: TStringList; const AName, AParent, Prefix: String); override;
        procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: String); override;
        procedure P_SetupFirst(SL: TStringList; const AName, AParent, Prefix: String); override;
        procedure AssignEvents( SL: TStringList; const AName: String ); override;
        function P_AssignEvents( SL: TStringList; const AName: String;
            CheckOnly: Boolean ): Boolean; override;
        procedure SetStyles (Val: TKOLGRushCheckBoxStyles);
    public
        constructor Create( AOwner: TComponent ); override;
        destructor Destroy; override;
        function Pcode_Generate: Boolean; override;
        procedure NotifyLinkedComponent( Sender: TObject; Operation: TNotifyOperation ); override;
    published
        property GRushStyles: TKOLGRushCheckBoxStyles read fStyles write SetStyles;
        property OnRecalcRects: TOnRecalcRects read fOnRecalcRects write SetOnRecalcRects;
        property imagecollection: TKOLGRushImageCollection read fImageCollection write SetImageCollection;

        property Auto3State: Integer read fDummyProperty;
        property Border: Integer read fDummyProperty;
        property ParentColor: Integer read fDummyProperty;
        property Ctl3D: Integer read fDummyProperty;
        property Color: Integer read fDummyProperty;
        property EraseBackGround: Integer read fDummyProperty;
        property HasBorder: Integer read fDummyProperty;
        property Brush: Integer read fDummyProperty;
        property Windowed: Integer read fDummyProperty;
        property WordWrap: Integer read fDummyProperty;
    end;

//************************************************************

    TKOLGRushRadioBox = class (TKOLRadioBox)
    protected
        fStyles: TKOLGRushRadioBoxStyles;
        fOnRecalcRects: TOnRecalcRects;
        fImageCollection: TKOLGRushImageCollection;

        fDummyProperty: Integer;
        procedure SetOnRecalcRects(const Value: TOnRecalcRects);
        procedure SetImageCollection(const Value: TKOLGRushImageCollection);

        function TypeName: String; override;
        function AdditionalUnits: String; override;
        procedure SetupConstruct(SL: TStringList; const AName, AParent, Prefix: String); override;
        procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: String); override;
        procedure P_SetupFirst(SL: TStringList; const AName, AParent, Prefix: String); override;
        procedure AssignEvents( SL: TStringList; const AName: String ); override;
        function P_AssignEvents( SL: TStringList; const AName: String;
            CheckOnly: Boolean ): Boolean; override;
        procedure SetStyles (Val: TKOLGRushRadioBoxStyles);
    public
        constructor Create( AOwner: TComponent ); override;
        destructor Destroy; override;
        function Pcode_Generate: Boolean; override;
        procedure NotifyLinkedComponent( Sender: TObject; Operation: TNotifyOperation ); override;
    published
        property GRushStyles: TKOLGRushRadioBoxStyles read fStyles write SetStyles;
        property OnRecalcRects: TOnRecalcRects read fOnRecalcRects write SetOnRecalcRects;
        property imagecollection: TKOLGRushImageCollection read fImageCollection write SetImageCollection;

        property Border: Integer read fDummyProperty;
        property ParentColor: Integer read fDummyProperty;
        property Ctl3D: Integer read fDummyProperty;
        property Color: Integer read fDummyProperty;
        property EraseBackGround: Integer read fDummyProperty;
        property HasBorder: Integer read fDummyProperty;
        property Brush: Integer read fDummyProperty;
        property Windowed: Integer read fDummyProperty;
        property WordWrap: Integer read fDummyProperty;
    end;

//************************************************************

    TKOLGRushSplitter = class (TKOLSplitter)
    protected
        fStyles: TKOLGRushSplitterStyles;
        fCaption: String;
        fLastAlign: TKOLAlign;
        fOnRecalcRects: TOnRecalcRects;
        fImageCollection: TKOLGRushImageCollection;

        fDummyProperty: Integer;
        procedure SetCaption(const Value: String); override;
        procedure SetOnRecalcRects(const Value: TOnRecalcRects);
        procedure SetImageCollection(const Value: TKOLGRushImageCollection);

        function TypeName: String; override;
        function AdditionalUnits: String; override;
        function SetupParams(const AName, AParent: String): String; override;
        function P_SetupParams( const AName, AParent: String; var nparams: Integer ): String; override;
        procedure SetupConstruct(SL: TStringList; const AName, AParent, Prefix: String); override;
        procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: String); override;
        procedure P_SetupFirst(SL: TStringList; const AName, AParent, Prefix: String); override;
        procedure AssignEvents( SL: TStringList; const AName: String ); override;
        function P_AssignEvents( SL: TStringList; const AName: String;
            CheckOnly: Boolean ): Boolean; override;
        procedure SetStyles (Val: TKOLGRushSplitterStyles);
    public
        constructor Create( AOwner: TComponent ); override;
        destructor Destroy; override;
        procedure Change; override;
        function Pcode_Generate: Boolean; override;
        procedure NotifyLinkedComponent( Sender: TObject; Operation: TNotifyOperation ); override;
    published
        property GRushStyles: TKOLGRushSplitterStyles read fStyles write SetStyles;
        property Caption: String read fCaption write SetCaption;
        property OnRecalcRects: TOnRecalcRects read fOnRecalcRects write SetOnRecalcRects;
        property imagecollection: TKOLGRushImageCollection read fImageCollection write SetImageCollection;

        property Brush: Integer read fDummyProperty;
        property ParentColor: Integer read fDummyProperty;
        property Color: Integer read fDummyProperty;
        property EraseBackGround: Integer read fDummyProperty;
        property EdgeStyle: Integer read fDummyProperty;
        property Ctl3D: Integer read fDummyProperty;
    end;

//************************************************************

    TKOLGRushProgressBar = class (TKOLProgressBar)
    protected
        fStyles: TKOLGRushProgressBarStyles;
        fCaption: String;
        fOnRecalcRects: TOnRecalcRects;
        fOnProgressChange: TOnProgressChange;
        fImageCollection: TKOLGRushImageCollection;

        fDummyProperty: Integer;
        procedure SetCaption(const Value: String); override;
        procedure SetOnRecalcRects(const Value: TOnRecalcRects);
        procedure SetOnProgressChange(const Value: TOnProgressChange);
        procedure SetImageCollection(const Value: TKOLGRushImageCollection);

        function TypeName: String; override;
        function AdditionalUnits: String; override;
        function SetupParams(const AName, AParent: String): String; override;
        function P_SetupParams( const AName, AParent: String; var nparams: Integer ): String; override;
        procedure SetupConstruct(SL: TStringList; const AName, AParent, Prefix: String); override;
        procedure SetupFirst(SL: TStringList; const AName, AParent, Prefix: String); override;
        procedure P_SetupFirst(SL: TStringList; const AName, AParent, Prefix: String); override;
        procedure AssignEvents( SL: TStringList; const AName: String ); override;
        function P_AssignEvents( SL: TStringList; const AName: String;
            CheckOnly: Boolean ): Boolean; override;
        procedure SetStyles (Val: TKOLGRushProgressBarStyles);
    public
        constructor Create( AOwner: TComponent ); override;
        destructor Destroy; override;
        function Pcode_Generate: Boolean; override;
        procedure NotifyLinkedComponent( Sender: TObject; Operation: TNotifyOperation ); override;
    published
        property GRushStyles: TKOLGRushProgressBarStyles read fStyles write SetStyles;
        property Caption: String read fCaption write SetCaption;
        property OnRecalcRects: TOnRecalcRects read fOnRecalcRects write SetOnRecalcRects;
        property OnProgressChange: TOnProgressChange read fOnProgressChange write SetOnProgressChange;
        property imagecollection: TKOLGRushImageCollection read fImageCollection write SetImageCollection;

        property Brush: Integer read fDummyProperty;
        property ParentColor: Integer read fDummyProperty;
        property Color: Integer read fDummyProperty;
        property EraseBackGround: Integer read fDummyProperty;
        property ProgressColor: Integer read fDummyProperty;
        property ProgressBKColor: Integer read fDummyProperty;
        property Smooth: Integer read fDummyProperty;
        property Vertical: Integer read fDummyProperty;
    end;

procedure Register;
procedure tinyLoadJPGGIFBMPStream(var TargetBitmap: KOL.PBitMap; Stream: Classes.TStream);
procedure TryResize(Control: KOL.PControl; W, H: Integer);

implementation

uses    tinyPNG, tinyJPGGIFBMP;

procedure Register;
begin
    RegisterComponents( 'KOLGRushControls', [TKOLGRushButton, TKOLGRushPanel
        , TKOLGRushCheckBox, TKOLGRushRadioBox, TKOLGRushSplitter, TKOLGRushProgressBar
        , TKOLGRushImageCollection]);
end;

procedure TryResize(Control: KOL.PControl; W, H: Integer);
begin
    if W < Control.Width then begin
        Control.Left := Control.Left + ((Control.Width-W) div 2);
        Control.Width := W;
    end;
    if H < Control.Height then begin
        Control.Top := Control.Top + ((Control.Height-H) div 2);
        Control.Height := H;
    end;
end;

//************************************************************

constructor TKOLPaintState.Create;
begin
    inherited Create;
    fOwner := aOwner;
end;

procedure TKOLPaintState.Assign( Source: TPersistent );
var     Val: TKOLPaintState;
begin
    if Source is TKOLPaintState then begin
        Val := Source as TKOLPaintState;
        //fOwner :=            Val.fOwner;
        fColorFrom :=        Val.fColorFrom;
        fColorTo :=          Val.fColorTo;
        fColorOuter :=       Val.fColorOuter;
        fColorText :=        Val.fColorText;
        fColorShadow :=      Val.fColorShadow;
        fBorderColor :=      Val.fBorderColor;
        fBorderRoundWidth := Val.fBorderRoundWidth;
        fBorderRoundHeight := Val.fBorderRoundHeight;
        fBorderWidth :=      Val.fBorderWidth;
        fGradientStyle :=    Val.fGradientStyle;
        fShadowOffset :=     Val.fShadowOffset;
        fGlyphItemX :=       Val.fGlyphItemX;
        fGlyphItemY :=       Val.fGlyphItemY;

        change;
    end;
end;

procedure TKOLPaintState.Change;
begin
    if fOwner = nil then Exit;
    if csLoading in fOwner.ComponentState then Exit;
    (fOwner as TKOLControl).Change;
end;

procedure TKOLPaintState.SetColorFrom;
begin fColorFrom := Value; Change; end;
procedure TKOLPaintState.SetColorTo;
begin fColorTo := Value; Change; end;
procedure TKOLPaintState.SetColorOuter;
begin fColorOuter := Value; Change; end;
procedure TKOLPaintState.SetColorText;
begin fColorText := Value; Change; end;
procedure TKOLPaintState.SetColorShadow;
begin fColorShadow := Value; Change; end;
procedure TKOLPaintState.SetBorderColor;
begin fBorderColor := Value; Change; end;
procedure TKOLPaintState.SetBorderRoundWidth;
begin fBorderRoundWidth := Value; Change; end;
procedure TKOLPaintState.SetBorderRoundHeight;
begin fBorderRoundHeight := Value; Change; end;
procedure TKOLPaintState.SetBorderWidth;
begin fBorderWidth := Value; Change; end;
procedure TKOLPaintState.SetGradientStyle;
begin fGradientStyle := Value; Change; end;
procedure TKOLPaintState.SetShadowOffset;
begin fShadowOffset := Value; Change; end;
procedure TKOLPaintState.SetGlyphItemX;
begin fGlyphItemX := Value; Change; end;
procedure TKOLPaintState.SetGlyphItemY;
begin fGlyphItemY := Value; Change; end;

//************************************************************

constructor TKOLRect.Create;
begin
    inherited Create;
    fOwner := aOwner;
    fLeft := aRect.Left;
    fTop := aRect.Top;
    fRight := aRect.Right;
    fBottom := aRect.Bottom;
end;

procedure TKOLRect.Assign( Source: TPersistent );
var     Val: TKOLRect;
begin
    if Source is TKOLRect then begin
        Val := Source as TKOLRect;
        //fOwner :=   Val.fOwner;
        fLeft :=    Val.fLeft;
        fTop :=     Val.fTop;
        fRight :=   Val.fRight;
        fBottom :=  Val.fBottom;
        Change;
    end;
end;

procedure TKOLRect.Change;
begin
    if fOwner = nil then Exit;
    if csLoading in fOwner.ComponentState then Exit;
    (fOwner as TKOLControl).Change;
end;

procedure TKOLRect.SetLeft;
begin fLeft := Value; Change; end;
procedure TKOLRect.SetTop;
begin fTop := Value; Change; end;
procedure TKOLRect.SetRight;
begin fRight := Value; Change; end;
procedure TKOLRect.SetBottom;
begin fBottom := Value; Change; end;

//************************************************************

constructor TKOLGRushStyles.Create;
begin
    inherited Create;
    fOwner := aOwner;
    fPSDef := TKOLPaintState.Create(aOwner);
    fPSOver := TKOLPaintState.Create(aOwner);
    fPSDown := TKOLPaintState.Create(aOwner);
    fPSDis := TKOLPaintState.Create(aOwner);
    fContentOffsets := TKOLRect.Create(aOwner, DefGRushData.fContentOffsets);
    Move(DefGRushData.fPSDef, (@fPSDef.fColorFrom)^, sizeof (TGRushPaintState));
    Move(DefGRushData.fPSOver, (@fPSOver.fColorFrom)^, sizeof (TGRushPaintState));
    Move(DefGRushData.fPSDown, (@fPSDown.fColorFrom)^, sizeof (TGRushPaintState));
    Move(DefGRushData.fPSDis, (@fPSDis.fColorFrom)^, sizeof (TGRushPaintState));
    fGlyphWidth :=       DefGRushData.fGlyphWidth;
    fGlyphHeight :=      DefGRushData.fGlyphHeight;
    fCheckMetric :=      DefGRushData.fCheckMetric;
    fColorCheck :=       DefGRushData.fColorCheck;
    fGlyphVAlign :=      DefGRushData.fGlyphVAlign;
    fGlyphHAlign :=      DefGRushData.fGlyphHAlign;
    fTextVAlign :=       DefGRushData.fTextVAlign;
    fTextHAlign :=       DefGRushData.fTextHAlign;
    fDrawGlyph :=        DefGRushData.fDrawGlyph;
    fDrawText :=         DefGRushData.fDrawText;
    fDrawFocusRect :=    DefGRushData.fDrawFocusRect;
    fDrawProgress :=     DefGRushData.fDrawProgress;
    fDrawProgressRect := DefGRushData.fDrawProgressRect;
    fGlyphAttached :=    DefGRushData.fGlyphAttached;
    fCropTopFirst :=     DefGRushData.fCropTopFirst;
    fAntiAliasing :=     DefGRushData.fAntiAliasing;
    fProgressVertical := DefGRushData.fProgressVertical;
    fUpdateSpeed :=      DefGRushData.fUpdateSpeed;
    fSpacing :=          DefGRushData.fSpacing;
    fSplitterDotsCount := DefGRushData.fSplitterDotsCount;
end;

destructor TKOLGRushStyles.Destroy;
begin
    fPSDef.Free;
    fPSOver.Free;
    fPSDown.Free;
    fPSDis.Free;
    fContentOffsets.Free;
    inherited;
end;

procedure TKOLGRushStyles.Assign (Source: TPersistent);
var     Val: TKOLGRushStyles;
begin
    if Source is TKOLGRushStyles then begin
        Val := Source as TKOLGRushStyles;
        //fOwner := Val.fOwner;
        fPSDef.Assign( Val.fPSDef );
        fPSOver.Assign( Val.fPSOver );
        fPSDown.Assign( Val.fPSDown );
        fPSDis.Assign( Val.fPSDis );
        fContentOffsets.Assign( Val.fContentOffsets );
        fGlyphWidth :=          Val.fGlyphWidth;
        fGlyphHeight :=         Val.fGlyphHeight;
        fCheckMetric :=         Val.fCheckMetric;
        fColorCheck :=          Val.fColorCheck;
        fGlyphVAlign :=         Val.fGlyphVAlign;
        fGlyphHAlign :=         Val.fGlyphHAlign;
        fTextVAlign :=          Val.fTextVAlign;
        fTextHAlign :=          Val.fTextHAlign;
        fDrawGlyph :=           Val.fDrawGlyph;
        fDrawText :=            Val.fDrawText;
        fDrawFocusRect :=       Val.fDrawFocusRect;
        fDrawProgress :=        Val.fDrawProgress;
        fDrawProgressRect :=    Val.fDrawProgressRect;
        fGlyphAttached :=       Val.fGlyphAttached;
        fCropTopFirst :=        Val.fCropTopFirst;
        fAntiAliasing :=        Val.fAntiAliasing;
        fProgressVertical :=    Val.fProgressVertical;
        fUpdateSpeed :=         Val.fUpdateSpeed;
        fSpacing :=             Val.fSpacing;
        fSplitterDotsCount :=   Val.fSplitterDotsCount;
        Change;
    end;
end;

procedure TKOLGRushStyles.Change;
begin
    if fOwner = nil then Exit;
    if csLoading in fOwner.ComponentState then Exit;
    (fOwner as TKOLControl).Change;
end;

procedure TKOLGRushStyles.SetGlyphWidth;
begin fGlyphWidth := Value; Change; end;
procedure TKOLGRushStyles.SetGlyphHeight;
begin fGlyphHeight := Value; Change; end;
procedure TKOLGRushStyles.SetCheckMetric;
begin fCheckMetric := Value; Change; end;
procedure TKOLGRushStyles.SetColorCheck;
begin fColorCheck := Value; Change; end;
procedure TKOLGRushStyles.SetGlyphVAlign;
begin fGlyphVAlign := Value; Change; end;
procedure TKOLGRushStyles.SetGlyphHAlign;
begin fGlyphHAlign := Value; Change; end;
procedure TKOLGRushStyles.SetTextVAlign;
begin fTextVAlign := Value; Change; end;
procedure TKOLGRushStyles.SetTextHAlign;
begin fTextHAlign := Value; Change; end;
procedure TKOLGRushStyles.SetDrawGlyph;
begin fDrawGlyph := Value; Change; end;
procedure TKOLGRushStyles.SetDrawText;
begin fDrawText := Value; Change; end;
procedure TKOLGRushStyles.SetDrawFocusRect;
begin fDrawFocusRect := Value; Change; end;
procedure TKOLGRushStyles.SetDrawProgress;
begin fDrawProgress := Value; Change; end;
procedure TKOLGRushStyles.SetDrawProgressRect;
begin fDrawProgressRect := Value; Change; end;
procedure TKOLGRushStyles.SetGlyphAttached;
begin fGlyphAttached := Value; Change; end;
procedure TKOLGRushStyles.SetCropTopFirst;
begin fCropTopFirst := Value; Change; end;
procedure TKOLGRushStyles.SetAntiAliasing;
begin fAntiAliasing := Value; Change; end;
procedure TKOLGRushStyles.SetProgressVertical;
begin fProgressVertical := Value; SetUpProgressVertical(Value); Change; end;
procedure TKOLGRushStyles.SetUpdateSpeed;
begin fUpdateSpeed := Value; Change; end;
procedure TKOLGRushStyles.SetSpacing;
begin fSpacing := Value; Change; end;
procedure TKOLGRushStyles.SetSplitterDotsCount;
begin fSplitterDotsCount := Value; Change; end;

procedure TKOLGRushStyles.SetUpProgressVertical;
begin
    if Value then begin
        fPSDef.fBorderRoundWidth := 25;
        fPSDef.fBorderRoundHeight := 4;
        fPSOver.fBorderRoundWidth := 25;
        fPSOver.fBorderRoundHeight := 4;
        fPSDown.fBorderRoundWidth := 25;
        fPSDown.fBorderRoundHeight := 4;
        fPSDis.fBorderRoundWidth := 25;
        fPSDis.fBorderRoundHeight := 4;
        fPSDef.fGradientStyle := gsDoubleHorz;
        fPSOver.fGradientStyle := gsDoubleHorz;
        fPSDown.fGradientStyle := gsDoubleHorz;
        fPSDis.fGradientStyle := gsDoubleHorz;
    end else begin
        fPSDef.fBorderRoundWidth := 4;
        fPSDef.fBorderRoundHeight := 25;
        fPSOver.fBorderRoundWidth := 4;
        fPSOver.fBorderRoundHeight := 25;
        fPSDown.fBorderRoundWidth := 4;
        fPSDown.fBorderRoundHeight := 25;
        fPSDis.fBorderRoundWidth := 4;
        fPSDis.fBorderRoundHeight := 25;
        fPSDef.fGradientStyle := gsDoubleVert;
        fPSOver.fGradientStyle := gsDoubleVert;
        fPSDown.fGradientStyle := gsDoubleVert;
        fPSDis.fGradientStyle := gsDoubleVert;
    end;
end;
procedure TKOLGRushStyles.SetUpSplitterAlign;
begin
    if (fPSOver = nil) or (fPSDown = nil) or (fPSDis = nil) then exit;
    if Value then begin
        fPSOver.fGradientStyle := gsHorizontal;
        fPSDown.fGradientStyle := gsHorizontal;
        fPSDis.fGradientStyle := gsHorizontal;
    end else begin
        fPSOver.fGradientStyle := gsVertical;
        fPSDown.fGradientStyle := gsVertical;
        fPSDis.fGradientStyle := gsVertical;
    end;
end;

//************************************************************

constructor TKOLGRushPanelStyles.Create;
begin
    inherited;
    fTextVAlign := KOL.vaTop;
    fPSDef.fBorderRoundWidth := 6;
    fPSDef.fBorderRoundHeight := 6;
    fPSDis.fBorderRoundWidth := 6;
    fPSDis.fBorderRoundHeight := 6;
end;

//************************************************************

constructor TKOLGRushCheckBoxStyles.Create;
begin
    inherited;
    fTextHAlign := haLeft;
    fContentOffsets.Free;
    fContentOffsets := TKOLRect.Create(aOwner, CheckContentRect);

    fPSDef.fBorderRoundWidth := 0;
    fPSDef.fBorderRoundHeight := 0;
    fPSOver.fBorderRoundWidth := 0;
    fPSOver.fBorderRoundHeight := 0;
    fPSDown.fBorderRoundWidth := 0;
    fPSDown.fBorderRoundHeight := 0;
    fPSDis.fBorderRoundWidth := 0;
    fPSDis.fBorderRoundHeight := 0;

    fPSDef.fBorderColor := clGray;
    fPSOver.fBorderColor := $404040;
    fPSDown.fBorderColor := clGray;
    fPSDis.fBorderColor := clGray;

    fPSDef.fGradientStyle := gsFromTopLeft;
    fPSOver.fGradientStyle := gsFromTopLeft;
    fPSDown.fGradientStyle := gsFromTopLeft;
    fPSDis.fGradientStyle := gsFromTopLeft;
end;

//************************************************************

constructor TKOLGRushRadioBoxStyles.Create;
begin
    inherited;
    fTextHAlign := haLeft;
    fContentOffsets.Free;
    fContentOffsets := TKOLRect.Create(aOwner, CheckContentRect);

    fPSDef.fBorderRoundWidth := 50;
    fPSDef.fBorderRoundHeight := 50;
    fPSOver.fBorderRoundWidth := 50;
    fPSOver.fBorderRoundHeight := 50;
    fPSDown.fBorderRoundWidth := 50;
    fPSDown.fBorderRoundHeight := 50;
    fPSDis.fBorderRoundWidth := 50;
    fPSDis.fBorderRoundHeight := 50;

    fPSDef.fBorderColor := clGray;
    fPSOver.fBorderColor := $404040;
    fPSDown.fBorderColor := clGray;
    fPSDis.fBorderColor := clGray;

    fPSDef.fGradientStyle := gsFromTopLeft;
    fPSOver.fGradientStyle := gsFromTopLeft;
    fPSDown.fGradientStyle := gsFromTopLeft;
    fPSDis.fGradientStyle := gsFromTopLeft;
end;

//************************************************************

constructor TKOLGRushSplitterStyles.Create;
begin
    inherited;
    fPSOver.fColorTo := $D0AD95;
    fPSDown.fColorTo := $C39475;
    fUpdateSpeed := usVeryFast;
    fSplitterDotsCount := 16;

    SetUpSplitterAlign((fOwner as TKOLCustomControl).Align in [mirror.caLeft, mirror.caRight]);
    fPSDef.fGradientStyle := gsSolid;

    fPSDef.fColorFrom := clBtnFace;
    fPSOver.fColorFrom := clWhite;
    fPSDown.fColorFrom := clWhite;
    fPSDis.fColorFrom := clWhite;

    fPSDef.fBorderWidth := 0;
    fPSOver.fBorderWidth := 0;
    fPSDown.fBorderWidth := 0;
    fPSDis.fBorderWidth := 0;
    fPSDef.fBorderRoundWidth := 0;
    fPSDef.fBorderRoundHeight := 0;
    fPSOver.fBorderRoundWidth := 0;
    fPSOver.fBorderRoundHeight := 0;
    fPSDown.fBorderRoundWidth := 0;
    fPSDown.fBorderRoundHeight := 0;
    fPSDis.fBorderRoundWidth := 0;
    fPSDis.fBorderRoundHeight := 0;
end;

//************************************************************

constructor TKOLGRushProgressBarStyles.Create;
begin
    inherited;
    fContentOffsets.Free;
    fContentOffsets := TKOLRect.Create(aOwner, ProgressBarContentRect);
    fDrawProgress := TRUE;
    fDrawProgressRect := TRUE;
    fPSDef.fColorTo := $B6977E;
    fPSDef.fColorFrom := $E0D2C9;
    fPSDef.fShadowOffset := 1;
    fPSOver.fShadowOffset := 1;
    fPSDown.fShadowOffset := 1;
    fPSDis.fShadowOffset := 1;

    fPSDef.fBorderWidth := 1;
    fPSOver.fBorderWidth := 1;
    fPSDown.fBorderWidth := 1;
    fPSDis.fBorderWidth := 1;

    SetUpProgressVertical(FALSE);
end;

//************************************************************

procedure SetUpCommon (aOwner: TComponent; Styles: TKOLGrushStyles;
    SL: TStringList; const AName, Prefix: String; DefStyles: TKOLGrushStyles;
    ImageCollection: TKOLGRushImageCollection);
const   TVAligns: array [KOL.TVerticalAlign] of String = ('KOL.vaTop', 'KOL.vaCenter', 'vaBottom');
        THAligns: array [TGRushHAlign] of String = ('haLeft', 'haCenter', 'haRight');
        Booleans: array [Boolean] of String = ('FALSE', 'TRUE');
begin
    if (Styles.fContentOffsets.fLeft <> DefStyles.fContentOffsets.fLeft)
        or (Styles.fContentOffsets.fTop <> DefStyles.fContentOffsets.fTop)
        or (Styles.fContentOffsets.fRight <> DefStyles.fContentOffsets.fRight)
        or (Styles.fContentOffsets.fBottom <> DefStyles.fContentOffsets.fBottom) then
        begin
        SL.Add( Prefix + AName + '.All_ContentOffsets := MakeRect( '+Int2str(Styles.fContentOffsets.fLeft)+', '
            +Int2str(Styles.fContentOffsets.fTop)+', '+Int2str(Styles.fContentOffsets.fRight)+', '
            +Int2str(Styles.fContentOffsets.fBottom)+');')
    end;
    {if Styles.fGlyphWidth <> DefStyles.fGlyphWidth then
        SL.Add( Prefix + AName + '.All_GlyphWidth := '+int2str(Styles.fGlyphWidth)+';');
    if Styles.fGlyphHeight <> DefStyles.fGlyphHeight then
        SL.Add( Prefix + AName + '.All_GlyphHeight := '+int2str(Styles.fGlyphHeight)+';');}
    if Assigned(ImageCollection) and (ImageCollection.fImageType <> None) then begin
        SL.Add( Prefix + AName + '.All_GlyphBitmap := Result.' + ImageCollection.Name + ';');
        if Styles.fGlyphWidth <> 0 then
            SL.Add( Prefix + AName + '.All_GlyphWidth := ' + inttostr(Styles.fGlyphWidth) + ';')
        else if ImageCollection.fItemWidth <> 0 then
            SL.Add( Prefix + AName + '.All_GlyphWidth := ' + inttostr(ImageCollection.fItemWidth) + ';');
        if Styles.fGlyphHeight <> 0 then
            SL.Add( Prefix + AName + '.All_GlyphHeight := ' + inttostr(Styles.fGlyphHeight) + ';')
        else if ImageCollection.fItemHeight <> 0 then
            SL.Add( Prefix + AName + '.All_GlyphHeight := ' + inttostr(ImageCollection.fItemHeight) + ';');
    end;
    if Styles.fGlyphVAlign <> DefStyles.fGlyphVAlign then
        SL.Add( Prefix + AName + '.All_GlyphVAlign := '+TVAligns[Styles.fGlyphVAlign]+';');
    if Styles.fGlyphHAlign <> DefStyles.fGlyphHAlign then
        SL.Add( Prefix + AName + '.All_GlyphHAlign := '+THAligns[Styles.fGlyphHAlign]+';');
    if Styles.fTextVAlign <> DefStyles.fTextVAlign then
        SL.Add( Prefix + AName + '.All_TextVAlign := '+TVAligns[Styles.fTextVAlign]+';');
    if Styles.fTextHAlign <> DefStyles.fTextHAlign then
        SL.Add( Prefix + AName + '.All_TextHAlign := '+THAligns[Styles.fTextHAlign]+';');
    if Styles.fDrawGlyph <> DefStyles.fDrawGlyph then
        SL.Add( Prefix + AName + '.All_DrawGlyph := '+Booleans[Styles.fDrawGlyph]+';');
    if Styles.fDrawText <> DefStyles.fDrawText then
        SL.Add( Prefix + AName + '.All_DrawText := '+Booleans[Styles.fDrawText]+';');
    if Styles.fGlyphAttached <> DefStyles.fGlyphAttached then
        SL.Add( Prefix + AName + '.All_GlyphAttached := '+Booleans[Styles.fGlyphAttached]+';');
    if Styles.fCropTopFirst <> DefStyles.fCropTopFirst then
        SL.Add( Prefix + AName + '.All_CropTopFirst := '+Booleans[Styles.fCropTopFirst]+';');
    if Styles.fAntiAliasing <> DefStyles.fAntiAliasing then
        SL.Add( Prefix + AName + '.All_AntiAliasing := '+Booleans[Styles.fAntiAliasing]+';');
    if Styles.fSpacing <> DefStyles.fSpacing then
        SL.Add( Prefix + AName + '.All_Spacing := '+int2str(Styles.fSpacing)+';');
end;

procedure P_SetUpCommon (aOwner: TComponent; Styles: TKOLGrushStyles;
    SL: TStringList; DefStyles: TKOLGrushStyles);
begin
    if (Styles.fContentOffsets.fLeft <> DefStyles.fContentOffsets.fLeft)
        or (Styles.fContentOffsets.fTop <> DefStyles.fContentOffsets.fTop)
        or (Styles.fContentOffsets.fRight <> DefStyles.fContentOffsets.fRight)
        or (Styles.fContentOffsets.fBottom <> DefStyles.fContentOffsets.fBottom) then
        begin
        SL.Add('L('+Int2Str(Styles.fContentOffsets.fBottom)
            +') L('+Int2Str(Styles.fContentOffsets.fRight)
            +') L('+Int2Str(Styles.fContentOffsets.fTop)
            +') L('+Int2Str(Styles.fContentOffsets.fLeft)
            +') LoadStack C5 GR0O_.SetAll_ContentOffsets<2> L(4) DelN');
    end;

    if Styles.fGlyphWidth <> DefStyles.fGlyphWidth then
        SL.Add( ' L(' + int2str(Styles.fGlyphWidth) + ') C1 GR0O_.SetAll_GlyphWidth<2>' );
    if Styles.fGlyphHeight <> DefStyles.fGlyphHeight then
        SL.Add( ' L(' + int2str(Styles.fGlyphHeight) + ') C1 GR0O_.SetAll_GlyphHeight<2>' );
    if Styles.fGlyphVAlign <> DefStyles.fGlyphVAlign then
        SL.Add( ' L(' + int2str( Byte ( Styles.fGlyphVAlign ) ) + ') C1 GR0O_.SetAll_GlyphVAlign<2>' );
    if Styles.fGlyphHAlign <> DefStyles.fGlyphHAlign then
        SL.Add( ' L(' + int2str( Byte ( Styles.fGlyphHAlign ) ) + ') C1 GR0O_.SetAll_GlyphHAlign<2>' );
    if Styles.fTextVAlign <> DefStyles.fTextVAlign then
        SL.Add( ' L(' + int2str( Byte ( Styles.fTextVAlign ) ) + ') C1 GR0O_.SetAll_TextVAlign<2>' );
    if Styles.fTextHAlign <> DefStyles.fTextHAlign then
        SL.Add( ' L(' + int2str( Byte ( Styles.fTextHAlign ) ) + ') C1 GR0O_.SetAll_TextHAlign<2>' );
    if Styles.fDrawGlyph <> DefStyles.fDrawGlyph then
        SL.Add( ' L(0) C1 GR0O_.SetAll_DrawGlyph<2>' );
    if Styles.fDrawText <> DefStyles.fDrawText then
        SL.Add( ' L(0) C1 GR0O_.SetAll_DrawText<2>' );
    if Styles.fGlyphAttached <> DefStyles.fGlyphAttached then
        SL.Add( ' L(1) C1 GR0O_.SetAll_GlyphAttached<2>' );
    if Styles.fCropTopFirst <> DefStyles.fCropTopFirst then
        SL.Add( ' L(0) C1 GR0O_.SetAll_CropTopFirst<2>' );
    if Styles.fAntialiasing <> DefStyles.fAntialiasing then
        SL.Add( ' L(0) C1 GR0O_.SetAll_Antialiasing<2>' );
    if Styles.fSpacing <> DefStyles.fSpacing then
        SL.Add( ' L(' + int2str( Styles.fSpacing ) + ') C1 GR0O_.SetAll_Spacing<2>' );
end;

//************************************************************

procedure SetUpState2States (aOwner: TComponent; Styles: TKOLGrushStyles;
    SL: TStringList; const AName, Prefix: String; DefStyles: TKOLGrushStyles);
const   GradientStyles: array [TGRushGradientStyle] of String = ('gsSolid', 'gsVertical'
    , 'gsHorizontal', 'gsDoubleVert', 'gsDoubleHorz', 'gsFromTopLeft', 'gsFromTopRight');
begin
    if (Styles.fPSDef.fColorFrom = Styles.fPSDis.fColorFrom)
        and (Styles.fPSDef.fColorFrom <> DefStyles.fPSDef.fColorFrom) then
        SL.Add( Prefix + AName + '.All_ColorFrom := '+Int2str(Styles.fPSDef.fColorFrom)+';')
    else begin
        if Styles.fPSDef.fColorFrom <> DefStyles.fPSDef.fColorFrom then
            SL.Add( Prefix + AName + '.Def_ColorFrom := '+Int2str(Styles.fPSDef.fColorFrom)+';');
        if Styles.fPSDis.fColorFrom <> DefStyles.fPSDis.fColorFrom then
            SL.Add( Prefix + AName + '.Dis_ColorFrom := '+Int2str(Styles.fPSDis.fColorFrom)+';');
    end;

    if (Styles.fPSDef.fColorTo = Styles.fPSDis.fColorTo)
        and (Styles.fPSDef.fColorTo <> DefStyles.fPSDef.fColorTo) then
        SL.Add( Prefix + AName + '.All_ColorTo := '+Int2str(Styles.fPSDef.fColorTo)+';')
    else begin
        if Styles.fPSDef.fColorTo <> DefStyles.fPSDef.fColorTo then
            SL.Add( Prefix + AName + '.Def_ColorTo := '+Int2str(Styles.fPSDef.fColorTo)+';');
        if Styles.fPSDis.fColorTo <> DefStyles.fPSDis.fColorTo then
            SL.Add( Prefix + AName + '.Dis_ColorTo := '+Int2str(Styles.fPSDis.fColorTo)+';');
    end;

    if (Styles.fPSDef.fColorOuter = Styles.fPSDis.fColorOuter)
        and (Styles.fPSDef.fColorOuter <> DefStyles.fPSDef.fColorOuter) then
        SL.Add( Prefix + AName + '.All_ColorOuter := '+Int2str(Styles.fPSDef.fColorOuter)+';')
    else begin
        if Styles.fPSDef.fColorOuter <> DefStyles.fPSDef.fColorOuter then
            SL.Add( Prefix + AName + '.Def_ColorOuter := '+Int2str(Styles.fPSDef.fColorOuter)+';');
        if Styles.fPSDis.fColorOuter <> DefStyles.fPSDis.fColorOuter then
            SL.Add( Prefix + AName + '.Dis_ColorOuter := '+Int2str(Styles.fPSDis.fColorOuter)+';');
    end;

    if (Styles.fPSDef.fColorText = Styles.fPSDis.fColorText)
        and (Styles.fPSDef.fColorText <> DefStyles.fPSDef.fColorText) then
        SL.Add( Prefix + AName + '.All_ColorText := '+Int2str(Styles.fPSDef.fColorText)+';')
    else begin
        if Styles.fPSDef.fColorText <> DefStyles.fPSDef.fColorText then
            SL.Add( Prefix + AName + '.Def_ColorText := '+Int2str(Styles.fPSDef.fColorText)+';');
        if Styles.fPSDis.fColorText <> DefStyles.fPSDis.fColorText then
            SL.Add( Prefix + AName + '.Dis_ColorText := '+Int2str(Styles.fPSDis.fColorText)+';');
    end;

    if (Styles.fPSDef.fColorShadow = Styles.fPSDis.fColorShadow)
        and (Styles.fPSDef.fColorShadow <> DefStyles.fPSDef.fColorShadow) then
        SL.Add( Prefix + AName + '.All_ColorShadow := '+Int2str(Styles.fPSDef.fColorShadow)+';')
    else begin
        if Styles.fPSDef.fColorShadow <> DefStyles.fPSDef.fColorShadow then
            SL.Add( Prefix + AName + '.Def_ColorShadow := '+Int2str(Styles.fPSDef.fColorShadow)+';');
        if Styles.fPSDis.fColorShadow <> DefStyles.fPSDis.fColorShadow then
            SL.Add( Prefix + AName + '.Dis_ColorShadow := '+Int2str(Styles.fPSDis.fColorShadow)+';');
    end;

    if (Styles.fPSDef.fBorderColor = Styles.fPSDis.fBorderColor)
        and (Styles.fPSDef.fBorderColor <> DefStyles.fPSDef.fBorderColor) then
        SL.Add( Prefix + AName + '.All_BorderColor := '+Int2str(Styles.fPSDef.fBorderColor)+';')
    else begin
        if Styles.fPSDef.fBorderColor <> DefStyles.fPSDef.fBorderColor then
            SL.Add( Prefix + AName + '.Def_BorderColor := '+Int2str(Styles.fPSDef.fBorderColor)+';');
        if Styles.fPSDis.fBorderColor <> DefStyles.fPSDis.fBorderColor then
            SL.Add( Prefix + AName + '.Dis_BorderColor := '+Int2str(Styles.fPSDis.fBorderColor)+';');
    end;

    if (Styles.fPSDef.fBorderRoundWidth = Styles.fPSDis.fBorderRoundWidth)
        and (Styles.fPSDef.fBorderRoundWidth <> DefStyles.fPSDef.fBorderRoundWidth) then
        SL.Add( Prefix + AName + '.All_BorderRoundWidth := '+Int2Str(Styles.fPSDef.fBorderRoundWidth)+';')
    else begin
        if Styles.fPSDef.fBorderRoundWidth <> DefStyles.fPSDef.fBorderRoundWidth then
            SL.Add( Prefix + AName + '.Def_BorderRoundWidth := '+Int2Str(Styles.fPSDef.fBorderRoundWidth)+';');
        if Styles.fPSDis.fBorderRoundWidth <> DefStyles.fPSDis.fBorderRoundWidth then
            SL.Add( Prefix + AName + '.Dis_BorderRoundWidth := '+Int2Str(Styles.fPSDis.fBorderRoundWidth)+';');
    end;

    if (Styles.fPSDef.fBorderRoundHeight = Styles.fPSDis.fBorderRoundHeight)
        and (Styles.fPSDef.fBorderRoundHeight <> DefStyles.fPSDef.fBorderRoundHeight) then
        SL.Add( Prefix + AName + '.All_BorderRoundHeight := '+Int2Str(Styles.fPSDef.fBorderRoundHeight)+';')
    else begin
        if Styles.fPSDef.fBorderRoundHeight <> DefStyles.fPSDef.fBorderRoundHeight then
            SL.Add( Prefix + AName + '.Def_BorderRoundHeight := '+Int2Str(Styles.fPSDef.fBorderRoundHeight)+';');
        if Styles.fPSDis.fBorderRoundHeight <> DefStyles.fPSDis.fBorderRoundHeight then
            SL.Add( Prefix + AName + '.Dis_BorderRoundHeight := '+Int2Str(Styles.fPSDis.fBorderRoundHeight)+';');
    end;

    if (Styles.fPSDef.fBorderWidth = Styles.fPSDis.fBorderWidth)
        and (Styles.fPSDef.fBorderWidth <> DefStyles.fPSDef.fBorderWidth) then
        SL.Add( Prefix + AName + '.All_BorderWidth := '+Int2Str(Styles.fPSDef.fBorderWidth)+';')
    else begin
        if Styles.fPSDef.fBorderWidth <> DefStyles.fPSDef.fBorderWidth then
            SL.Add( Prefix + AName + '.Def_BorderWidth := '+Int2Str(Styles.fPSDef.fBorderWidth)+';');
        if Styles.fPSDis.fBorderWidth <> DefStyles.fPSDis.fBorderWidth then
            SL.Add( Prefix + AName + '.Dis_BorderWidth := '+Int2Str(Styles.fPSDis.fBorderWidth)+';');
    end;

    if (Styles.fPSDef.fGradientStyle = Styles.fPSDis.fGradientStyle)
        and (Styles.fPSDef.fGradientStyle <> DefStyles.fPSDef.fGradientStyle) then
        SL.Add( Prefix + AName + '.All_GradientStyle := '+GradientStyles[Styles.fPSDef.fGradientStyle]+';')
    else begin
        if Styles.fPSDef.fGradientStyle <> DefStyles.fPSDef.fGradientStyle then
            SL.Add( Prefix + AName + '.Def_GradientStyle := '+GradientStyles[Styles.fPSDef.fGradientStyle]+';');
        if Styles.fPSDis.fGradientStyle <> DefStyles.fPSDis.fGradientStyle then
            SL.Add( Prefix + AName + '.Dis_GradientStyle := '+GradientStyles[Styles.fPSDis.fGradientStyle]+';');
    end;

    if (Styles.fPSDef.fShadowOffset = Styles.fPSDis.fShadowOffset)
        and (Styles.fPSDef.fShadowOffset <> DefStyles.fPSDef.fShadowOffset) then
        SL.Add( Prefix + AName + '.All_ShadowOffset := '+Int2Str(Styles.fPSDef.fShadowOffset)+';')
    else begin
        if Styles.fPSDef.fShadowOffset <> DefStyles.fPSDef.fShadowOffset then
            SL.Add( Prefix + AName + '.Def_ShadowOffset := '+Int2Str(Styles.fPSDef.fShadowOffset)+';');
        if Styles.fPSDis.fShadowOffset <> DefStyles.fPSDis.fShadowOffset then
            SL.Add( Prefix + AName + '.Dis_ShadowOffset := '+Int2Str(Styles.fPSDis.fShadowOffset)+';');
    end;

    if (Styles.fPSDef.fGlyphItemX = Styles.fPSDis.fGlyphItemX)
        and (Styles.fPSDef.fGlyphItemX <> DefStyles.fPSDef.fGlyphItemX) then
        SL.Add( Prefix + AName + '.All_GlyphItemX := '+Int2Str(Styles.fPSDef.fGlyphItemX)+';')
    else begin
        if Styles.fPSDef.fGlyphItemX <> DefStyles.fPSDef.fGlyphItemX then
            SL.Add( Prefix + AName + '.Def_GlyphItemX := '+Int2Str(Styles.fPSDef.fGlyphItemX)+';');
        if Styles.fPSDis.fGlyphItemX <> DefStyles.fPSDis.fGlyphItemX then
            SL.Add( Prefix + AName + '.Dis_GlyphItemX := '+Int2Str(Styles.fPSDis.fGlyphItemX)+';');
    end;

    if (Styles.fPSDef.fGlyphItemY = Styles.fPSDis.fGlyphItemY)
        and (Styles.fPSDef.fGlyphItemY <> DefStyles.fPSDef.fGlyphItemY) then
        SL.Add( Prefix + AName + '.All_GlyphItemY := '+Int2Str(Styles.fPSDef.fGlyphItemY)+';')
    else begin
        if Styles.fPSDef.fGlyphItemY <> DefStyles.fPSDef.fGlyphItemY then
            SL.Add( Prefix + AName + '.Def_GlyphItemY := '+Int2Str(Styles.fPSDef.fGlyphItemY)+';');
        if Styles.fPSDis.fGlyphItemY <> DefStyles.fPSDis.fGlyphItemY then
            SL.Add( Prefix + AName + '.Dis_GlyphItemY := '+Int2Str(Styles.fPSDis.fGlyphItemY)+';');
    end;
end;

procedure P_SetUpState2States (aOwner: TComponent; Styles: TKOLGrushStyles;
    SL: TStringList; DefStyles: TKOLGrushStyles);
begin
    if (Styles.fPSDef.fColorFrom = Styles.fPSOver.fColorFrom)
        and (Styles.fPSDef.fColorFrom = Styles.fPSDis.fColorFrom) then
        SL.Add( ' L(' + IntToStr( Styles.fPSDef.fColorFrom ) + ') C1 GR0O_.SetAll_ColorFrom<2>' )
    else begin
        if Styles.fPSDef.fColorFrom <> DefStyles.fPSDef.fColorFrom then
            SL.Add( ' L(' + IntToStr( Styles.fPSDef.fColorFrom ) + ') C1 GR0O_.SetDef_ColorFrom<2>' );
        if Styles.fPSDown.fColorFrom <> DefStyles.fPSDown.fColorFrom then
            SL.Add( ' L(' + IntToStr( Styles.fPSDown.fColorFrom ) + ') C1 GR0O_.SetDown_ColorFrom<2>' );
    end;

    if (Styles.fPSDef.fColorTo = Styles.fPSOver.fColorTo)
        and (Styles.fPSDef.fColorTo = Styles.fPSDis.fColorTo)then
        SL.Add( ' L(' + IntToStr( Styles.fPSDef.fColorTo ) + ') C1 GR0O_.SetAll_ColorTo<2>' )
    else begin
        if Styles.fPSDef.fColorTo <> DefStyles.fPSDef.fColorTo then
            SL.Add( ' L(' + IntToStr( Styles.fPSDef.fColorTo ) + ') C1 GR0O_.SetDef_ColorTo<2>' );
        if Styles.fPSDown.fColorTo <> DefStyles.fPSDown.fColorTo then
            SL.Add( ' L(' + IntToStr( Styles.fPSDown.fColorTo ) + ') C1 GR0O_.SetDown_ColorTo<2>' );
    end;

    if (Styles.fPSDef.fColorOuter = Styles.fPSOver.fColorOuter)
        and (Styles.fPSDef.fColorOuter = Styles.fPSDis.fColorOuter) then
        SL.Add( ' L(' + IntToStr( Styles.fPSDef.fColorOuter ) + ') C1 GR0O_.SetAll_ColorOuter<2>' )
    else begin
        if Styles.fPSDef.fColorOuter <> DefStyles.fPSDef.fColorOuter then
            SL.Add( ' L(' + IntToStr( Styles.fPSDef.fColorOuter ) + ') C1 GR0O_.SetDef_ColorOuter<2>' );
        if Styles.fPSDown.fColorOuter <> DefStyles.fPSDown.fColorOuter then
            SL.Add( ' L(' + IntToStr( Styles.fPSDown.fColorOuter ) + ') C1 GR0O_.SetDown_ColorOuter<2>' );
    end;

    if (Styles.fPSDef.fColorText = Styles.fPSOver.fColorText)
        and (Styles.fPSDef.fColorText = Styles.fPSDis.fColorText) then
        SL.Add( ' L(' + IntToStr( Styles.fPSDef.fColorText ) + ') C1 GR0O_.SetAll_ColorText<2>' )
    else begin
        if Styles.fPSDef.fColorText <> DefStyles.fPSDef.fColorText then
            SL.Add( ' L(' + IntToStr( Styles.fPSDef.fColorText ) + ') C1 GR0O_.SetDef_ColorText<2>' );
        if Styles.fPSDown.fColorText <> DefStyles.fPSDown.fColorText then
            SL.Add( ' L(' + IntToStr( Styles.fPSDown.fColorText ) + ') C1 GR0O_.SetDown_ColorText<2>' );
    end;

    if (Styles.fPSDef.fColorShadow = Styles.fPSOver.fColorShadow)
        and (Styles.fPSDef.fColorShadow = Styles.fPSDis.fColorShadow) then
        SL.Add( ' L(' + IntToStr( Styles.fPSDef.fColorShadow ) + ') C1 GR0O_.SetAll_ColorShadow<2>' )
    else begin
        if Styles.fPSDef.fColorShadow <> DefStyles.fPSDef.fColorShadow then
            SL.Add( ' L(' + IntToStr( Styles.fPSDef.fColorShadow ) + ') C1 GR0O_.SetDef_ColorShadow<2>' );
        if Styles.fPSDown.fColorShadow <> DefStyles.fPSDown.fColorShadow then
            SL.Add( ' L(' + IntToStr( Styles.fPSDown.fColorShadow ) + ') C1 GR0O_.SetDown_ColorShadow<2>' );
    end;

    if (Styles.fPSDef.fBorderColor = Styles.fPSOver.fBorderColor)
        and (Styles.fPSDef.fBorderColor = Styles.fPSDis.fBorderColor) then
        SL.Add( ' L(' + IntToStr( Styles.fPSDef.fBorderColor ) + ') C1 GR0O_.SetAll_BorderColor<2>' )
    else begin
        if Styles.fPSDef.fBorderColor <> DefStyles.fPSDef.fBorderColor then
            SL.Add( ' L(' + IntToStr( Styles.fPSDef.fBorderColor ) + ') C1 GR0O_.SetDef_BorderColor<2>' );
        if Styles.fPSDown.fBorderColor <> DefStyles.fPSDown.fBorderColor then
            SL.Add( ' L(' + IntToStr( Styles.fPSDown.fBorderColor ) + ') C1 GR0O_.SetDown_BorderColor<2>' );
    end;

    if (Styles.fPSDef.fBorderRoundWidth = Styles.fPSOver.fBorderRoundWidth)
        and (Styles.fPSDef.fBorderRoundWidth = Styles.fPSDis.fBorderRoundWidth) then
        SL.Add( ' L(' + IntToStr( Styles.fPSDef.fBorderRoundWidth ) + ') C1 GR0O_.SetAll_BorderRoundWidth<2>' )
    else begin
        if Styles.fPSDef.fBorderRoundWidth <> DefStyles.fPSDef.fBorderRoundWidth then
            SL.Add( ' L(' + IntToStr( Styles.fPSDef.fBorderRoundWidth ) + ') C1 GR0O_.SetDef_BorderRoundWidth<2>' );
        if Styles.fPSDown.fBorderRoundWidth <> DefStyles.fPSDown.fBorderRoundWidth then
            SL.Add( ' L(' + IntToStr( Styles.fPSDown.fBorderRoundWidth ) + ') C1 GR0O_.SetDown_BorderRoundWidth<2>' );
    end;

    if (Styles.fPSDef.fBorderRoundHeight = Styles.fPSOver.fBorderRoundHeight)
        and (Styles.fPSDef.fBorderRoundHeight = Styles.fPSDis.fBorderRoundHeight) then
        SL.Add( ' L(' + IntToStr( Styles.fPSDef.fBorderRoundHeight ) + ') C1 GR0O_.SetAll_BorderRoundHeight<2>' )
    else begin
        if Styles.fPSDef.fBorderRoundHeight <> DefStyles.fPSDef.fBorderRoundHeight then
            SL.Add( ' L(' + IntToStr( Styles.fPSDef.fBorderRoundHeight ) + ') C1 GR0O_.SetDef_BorderRoundHeight<2>' );
        if Styles.fPSDown.fBorderRoundHeight <> DefStyles.fPSDown.fBorderRoundHeight then
            SL.Add( ' L(' + IntToStr( Styles.fPSDown.fBorderRoundHeight ) + ') C1 GR0O_.SetDown_BorderRoundHeight<2>' );
    end;

    if (Styles.fPSDef.fBorderWidth = Styles.fPSOver.fBorderWidth)
        and (Styles.fPSDef.fBorderWidth = Styles.fPSDis.fBorderWidth)then
        SL.Add( ' L(' + IntToStr( Styles.fPSDef.fBorderWidth ) + ') C1 GR0O_.SetAll_BorderWidth<2>' )
    else begin
        if Styles.fPSDef.fBorderWidth <> DefStyles.fPSDef.fBorderWidth then
            SL.Add( ' L(' + IntToStr( Styles.fPSDef.fBorderWidth ) + ') C1 GR0O_.SetDef_BorderWidth<2>' );
        if Styles.fPSDown.fBorderWidth <> DefStyles.fPSDown.fBorderWidth then
            SL.Add( ' L(' + IntToStr( Styles.fPSDown.fBorderWidth ) + ') C1 GR0O_.SetDown_BorderWidth<2>' );
    end;

    if (Styles.fPSDef.fGradientStyle = Styles.fPSOver.fGradientStyle)
        and (Styles.fPSDef.fGradientStyle = Styles.fPSDis.fGradientStyle) then
        SL.Add( ' L(' + IntToStr( Byte (Styles.fPSDef.fGradientStyle ) ) + ') C1 GR0O_.SetAll_GradientStyle<2>' )
    else begin
        if Styles.fPSDef.fGradientStyle <> DefStyles.fPSDef.fGradientStyle then
            SL.Add( ' L(' + IntToStr( Byte (Styles.fPSDef.fGradientStyle ) ) + ') C1 GR0O_.SetDef_GradientStyle<2>' );
        if Styles.fPSDown.fGradientStyle <> DefStyles.fPSDown.fGradientStyle then
            SL.Add( ' L(' + IntToStr( Byte (Styles.fPSDown.fGradientStyle ) ) + ') C1 GR0O_.SetDown_GradientStyle<2>' );
    end;

    if (Styles.fPSDef.fShadowOffset = Styles.fPSOver.fShadowOffset)
        and (Styles.fPSDef.fShadowOffset = Styles.fPSDis.fShadowOffset) then
        SL.Add( ' L(' + IntToStr( Styles.fPSDef.fShadowOffset ) + ') C1 GR0O_.SetAll_ShadowOffset<2>' )
    else begin
        if Styles.fPSDef.fShadowOffset <> DefStyles.fPSDef.fShadowOffset then
            SL.Add( ' L(' + IntToStr( Styles.fPSDef.fShadowOffset ) + ') C1 GR0O_.SetDef_ShadowOffset<2>' );
        if Styles.fPSDown.fShadowOffset <> DefStyles.fPSDown.fShadowOffset then
            SL.Add( ' L(' + IntToStr( Styles.fPSDown.fShadowOffset ) + ') C1 GR0O_.SetDown_ShadowOffset<2>' );
    end;

    if (Styles.fPSDef.fGlyphItemX = Styles.fPSOver.fGlyphItemX)
        and (Styles.fPSDef.fGlyphItemX = Styles.fPSDis.fGlyphItemX) then
        SL.Add( ' L(' + IntToStr( Styles.fPSDef.fGlyphItemX ) + ') C1 GR0O_.SetAll_GlyphItemX<2>' )
    else begin
        if Styles.fPSDef.fGlyphItemX <> DefStyles.fPSDef.fGlyphItemX then
            SL.Add( ' L(' + IntToStr( Styles.fPSDef.fGlyphItemX ) + ') C1 GR0O_.SetDef_GlyphItemX<2>' );
        if Styles.fPSDown.fGlyphItemX <> DefStyles.fPSDown.fGlyphItemX then
            SL.Add( ' L(' + IntToStr( Styles.fPSDown.fGlyphItemX ) + ') C1 GR0O_.SetDown_GlyphItemX<2>' );
    end;

    if (Styles.fPSDef.fGlyphItemY = Styles.fPSOver.fGlyphItemY)
        and (Styles.fPSDef.fGlyphItemY = Styles.fPSDis.fGlyphItemY) then
        SL.Add( ' L(' + IntToStr( Styles.fPSDef.fGlyphItemY ) + ') C1 GR0O_.SetAll_GlyphItemY<2>' )
    else begin
        if Styles.fPSDef.fGlyphItemY <> DefStyles.fPSDef.fGlyphItemY then
            SL.Add( ' L(' + IntToStr( Styles.fPSDef.fGlyphItemY ) + ') C1 GR0O_.SetDef_GlyphItemY<2>' );
        if Styles.fPSDown.fGlyphItemY <> DefStyles.fPSDown.fGlyphItemY then
            SL.Add( ' L(' + IntToStr( Styles.fPSDown.fGlyphItemY ) + ') C1 GR0O_.SetDown_GlyphItemY<2>' );
    end;
end;

//************************************************************

procedure SetUpState4States (aOwner: TComponent; Styles: TKOLGrushStyles;
    SL: TStringList; const AName, Prefix: String; DefStyles: TKOLGrushStyles);
const   GradientStyles: array [TGRushGradientStyle] of String = ('gsSolid', 'gsVertical'
    , 'gsHorizontal', 'gsDoubleVert', 'gsDoubleHorz', 'gsFromTopLeft', 'gsFromTopRight');
begin
    if (Styles.fPSDef.fColorFrom = Styles.fPSOver.fColorFrom)
        and (Styles.fPSDef.fColorFrom = Styles.fPSDown.fColorFrom)
        and (Styles.fPSDef.fColorFrom = Styles.fPSDis.fColorFrom)
        and (Styles.fPSDef.fColorFrom <> DefStyles.fPSDef.fColorFrom) then
        SL.Add( Prefix + AName + '.All_ColorFrom := '+Int2str(Styles.fPSDef.fColorFrom)+';')
    else begin
        if Styles.fPSDef.fColorFrom <> DefStyles.fPSDef.fColorFrom then
            SL.Add( Prefix + AName + '.Def_ColorFrom := '+Int2str(Styles.fPSDef.fColorFrom)+';');
        if Styles.fPSOver.fColorFrom <> DefStyles.fPSOver.fColorFrom then
            SL.Add( Prefix + AName + '.Over_ColorFrom := '+Int2str(Styles.fPSOver.fColorFrom)+';');
        if Styles.fPSDown.fColorFrom <> DefStyles.fPSDown.fColorFrom then
            SL.Add( Prefix + AName + '.Down_ColorFrom := '+Int2str(Styles.fPSDown.fColorFrom)+';');
        if Styles.fPSDis.fColorFrom <> DefStyles.fPSDis.fColorFrom then
            SL.Add( Prefix + AName + '.Dis_ColorFrom := '+Int2str(Styles.fPSDis.fColorFrom)+';');
    end;

    if (Styles.fPSDef.fColorTo = Styles.fPSOver.fColorTo)
        and (Styles.fPSDef.fColorTo = Styles.fPSDown.fColorTo)
        and (Styles.fPSDef.fColorTo = Styles.fPSDis.fColorTo)
        and (Styles.fPSDef.fColorTo <> DefStyles.fPSDef.fColorTo) then
        SL.Add( Prefix + AName + '.All_ColorTo := '+Int2str(Styles.fPSDef.fColorTo)+';')
    else begin
        if Styles.fPSDef.fColorTo <> DefStyles.fPSDef.fColorTo then
            SL.Add( Prefix + AName + '.Def_ColorTo := '+Int2str(Styles.fPSDef.fColorTo)+';');
        if Styles.fPSOver.fColorTo <> DefStyles.fPSOver.fColorTo then
            SL.Add( Prefix + AName + '.Over_ColorTo := '+Int2str(Styles.fPSOver.fColorTo)+';');
        if Styles.fPSDown.fColorTo <> DefStyles.fPSDown.fColorTo then
            SL.Add( Prefix + AName + '.Down_ColorTo := '+Int2str(Styles.fPSDown.fColorTo)+';');
        if Styles.fPSDis.fColorTo <> DefStyles.fPSDis.fColorTo then
            SL.Add( Prefix + AName + '.Dis_ColorTo := '+Int2str(Styles.fPSDis.fColorTo)+';');
    end;

    if (Styles.fPSDef.fColorOuter = Styles.fPSOver.fColorOuter)
        and (Styles.fPSDef.fColorOuter = Styles.fPSDown.fColorOuter)
        and (Styles.fPSDef.fColorOuter = Styles.fPSDis.fColorOuter)
        and (Styles.fPSDef.fColorOuter <> DefStyles.fPSDef.fColorOuter) then
        SL.Add( Prefix + AName + '.All_ColorOuter := '+Int2str(Styles.fPSDef.fColorOuter)+';')
    else begin
        if Styles.fPSDef.fColorOuter <> DefStyles.fPSDef.fColorOuter then
            SL.Add( Prefix + AName + '.Def_ColorOuter := '+Int2str(Styles.fPSDef.fColorOuter)+';');
        if Styles.fPSOver.fColorOuter <> DefStyles.fPSOver.fColorOuter then
            SL.Add( Prefix + AName + '.Over_ColorOuter := '+Int2str(Styles.fPSOver.fColorOuter)+';');
        if Styles.fPSDown.fColorOuter <> DefStyles.fPSDown.fColorOuter then
            SL.Add( Prefix + AName + '.Down_ColorOuter := '+Int2str(Styles.fPSDown.fColorOuter)+';');
        if Styles.fPSDis.fColorOuter <> DefStyles.fPSDis.fColorOuter then
            SL.Add( Prefix + AName + '.Dis_ColorOuter := '+Int2str(Styles.fPSDis.fColorOuter)+';');
    end;

    if (Styles.fPSDef.fColorText = Styles.fPSOver.fColorText)
        and (Styles.fPSDef.fColorText = Styles.fPSDown.fColorText)
        and (Styles.fPSDef.fColorText = Styles.fPSDis.fColorText)
        and (Styles.fPSDef.fColorText <> DefStyles.fPSDef.fColorText) then
        SL.Add( Prefix + AName + '.All_ColorText := '+Int2str(Styles.fPSDef.fColorText)+';')
    else begin
        if Styles.fPSDef.fColorText <> DefStyles.fPSDef.fColorText then
            SL.Add( Prefix + AName + '.Def_ColorText := '+Int2str(Styles.fPSDef.fColorText)+';');
        if Styles.fPSOver.fColorText <> DefStyles.fPSOver.fColorText then
            SL.Add( Prefix + AName + '.Over_ColorText := '+Int2str(Styles.fPSOver.fColorText)+';');
        if Styles.fPSDown.fColorText <> DefStyles.fPSDown.fColorText then
            SL.Add( Prefix + AName + '.Down_ColorText := '+Int2str(Styles.fPSDown.fColorText)+';');
        if Styles.fPSDis.fColorText <> DefStyles.fPSDis.fColorText then
            SL.Add( Prefix + AName + '.Dis_ColorText := '+Int2str(Styles.fPSDis.fColorText)+';');
    end;

    if (Styles.fPSDef.fColorShadow = Styles.fPSOver.fColorShadow)
        and (Styles.fPSDef.fColorShadow = Styles.fPSDown.fColorShadow)
        and (Styles.fPSDef.fColorShadow = Styles.fPSDis.fColorShadow)
        and (Styles.fPSDef.fColorShadow <> DefStyles.fPSDef.fColorShadow) then
        SL.Add( Prefix + AName + '.All_ColorShadow := '+Int2str(Styles.fPSDef.fColorShadow)+';')
    else begin
        if Styles.fPSDef.fColorShadow <> DefStyles.fPSDef.fColorShadow then
            SL.Add( Prefix + AName + '.Def_ColorShadow := '+Int2str(Styles.fPSDef.fColorShadow)+';');
        if Styles.fPSOver.fColorShadow <> DefStyles.fPSOver.fColorShadow then
            SL.Add( Prefix + AName + '.Over_ColorShadow := '+Int2str(Styles.fPSOver.fColorShadow)+';');
        if Styles.fPSDown.fColorShadow <> DefStyles.fPSDown.fColorShadow then
            SL.Add( Prefix + AName + '.Down_ColorShadow := '+Int2str(Styles.fPSDown.fColorShadow)+';');
        if Styles.fPSDis.fColorShadow <> DefStyles.fPSDis.fColorShadow then
            SL.Add( Prefix + AName + '.Dis_ColorShadow := '+Int2str(Styles.fPSDis.fColorShadow)+';');
    end;

    if (Styles.fPSDef.fBorderColor = Styles.fPSOver.fBorderColor)
        and (Styles.fPSDef.fBorderColor = Styles.fPSDown.fBorderColor)
        and (Styles.fPSDef.fBorderColor = Styles.fPSDis.fBorderColor)
        and (Styles.fPSDef.fBorderColor <> DefStyles.fPSDef.fBorderColor) then
        SL.Add( Prefix + AName + '.All_BorderColor := '+Int2str(Styles.fPSDef.fBorderColor)+';')
    else begin
        if Styles.fPSDef.fBorderColor <> DefStyles.fPSDef.fBorderColor then
            SL.Add( Prefix + AName + '.Def_BorderColor := '+Int2str(Styles.fPSDef.fBorderColor)+';');
        if Styles.fPSOver.fBorderColor <> DefStyles.fPSOver.fBorderColor then
            SL.Add( Prefix + AName + '.Over_BorderColor := '+Int2str(Styles.fPSOver.fBorderColor)+';');
        if Styles.fPSDown.fBorderColor <> DefStyles.fPSDown.fBorderColor then
            SL.Add( Prefix + AName + '.Down_BorderColor := '+Int2str(Styles.fPSDown.fBorderColor)+';');
        if Styles.fPSDis.fBorderColor <> DefStyles.fPSDis.fBorderColor then
            SL.Add( Prefix + AName + '.Dis_BorderColor := '+Int2str(Styles.fPSDis.fBorderColor)+';');
    end;

    if (Styles.fPSDef.fBorderRoundWidth = Styles.fPSOver.fBorderRoundWidth)
        and (Styles.fPSDef.fBorderRoundWidth = Styles.fPSDown.fBorderRoundWidth)
        and (Styles.fPSDef.fBorderRoundWidth = Styles.fPSDis.fBorderRoundWidth)
        and (Styles.fPSDef.fBorderRoundWidth <> DefStyles.fPSDef.fBorderRoundWidth) then
        SL.Add( Prefix + AName + '.All_BorderRoundWidth := '+Int2Str(Styles.fPSDef.fBorderRoundWidth)+';')
    else begin
        if Styles.fPSDef.fBorderRoundWidth <> DefStyles.fPSDef.fBorderRoundWidth then
            SL.Add( Prefix + AName + '.Def_BorderRoundWidth := '+Int2Str(Styles.fPSDef.fBorderRoundWidth)+';');
        if Styles.fPSOver.fBorderRoundWidth <> DefStyles.fPSOver.fBorderRoundWidth then
            SL.Add( Prefix + AName + '.Over_BorderRoundWidth := '+Int2Str(Styles.fPSOver.fBorderRoundWidth)+';');
        if Styles.fPSDown.fBorderRoundWidth <> DefStyles.fPSDown.fBorderRoundWidth then
            SL.Add( Prefix + AName + '.Down_BorderRoundWidth := '+Int2Str(Styles.fPSDown.fBorderRoundWidth)+';');
        if Styles.fPSDis.fBorderRoundWidth <> DefStyles.fPSDis.fBorderRoundWidth then
            SL.Add( Prefix + AName + '.Dis_BorderRoundWidth := '+Int2Str(Styles.fPSDis.fBorderRoundWidth)+';');
    end;

    if (Styles.fPSDef.fBorderRoundHeight = Styles.fPSOver.fBorderRoundHeight)
        and (Styles.fPSDef.fBorderRoundHeight = Styles.fPSDown.fBorderRoundHeight)
        and (Styles.fPSDef.fBorderRoundHeight = Styles.fPSDis.fBorderRoundHeight)
        and (Styles.fPSDef.fBorderRoundHeight <> DefStyles.fPSDef.fBorderRoundHeight) then
        SL.Add( Prefix + AName + '.All_BorderRoundHeight := '+Int2Str(Styles.fPSDef.fBorderRoundHeight)+';')
    else begin
        if Styles.fPSDef.fBorderRoundHeight <> DefStyles.fPSDef.fBorderRoundHeight then
            SL.Add( Prefix + AName + '.Def_BorderRoundHeight := '+Int2Str(Styles.fPSDef.fBorderRoundHeight)+';');
        if Styles.fPSOver.fBorderRoundHeight <> DefStyles.fPSOver.fBorderRoundHeight then
            SL.Add( Prefix + AName + '.Over_BorderRoundHeight := '+Int2Str(Styles.fPSOver.fBorderRoundHeight)+';');
        if Styles.fPSDown.fBorderRoundHeight <> DefStyles.fPSDown.fBorderRoundHeight then
            SL.Add( Prefix + AName + '.Down_BorderRoundHeight := '+Int2Str(Styles.fPSDown.fBorderRoundHeight)+';');
        if Styles.fPSDis.fBorderRoundHeight <> DefStyles.fPSDis.fBorderRoundHeight then
            SL.Add( Prefix + AName + '.Dis_BorderRoundHeight := '+Int2Str(Styles.fPSDis.fBorderRoundHeight)+';');
    end;

    if (Styles.fPSDef.fBorderWidth = Styles.fPSOver.fBorderWidth)
        and (Styles.fPSDef.fBorderWidth = Styles.fPSDown.fBorderWidth)
        and (Styles.fPSDef.fBorderWidth = Styles.fPSDis.fBorderWidth)
        and (Styles.fPSDef.fBorderWidth <> DefStyles.fPSDef.fBorderWidth) then
        SL.Add( Prefix + AName + '.All_BorderWidth := '+Int2Str(Styles.fPSDef.fBorderWidth)+';')
    else begin
        if Styles.fPSDef.fBorderWidth <> DefStyles.fPSDef.fBorderWidth then
            SL.Add( Prefix + AName + '.Def_BorderWidth := '+Int2Str(Styles.fPSDef.fBorderWidth)+';');
        if Styles.fPSOver.fBorderWidth <> DefStyles.fPSOver.fBorderWidth then
            SL.Add( Prefix + AName + '.Over_BorderWidth := '+Int2Str(Styles.fPSOver.fBorderWidth)+';');
        if Styles.fPSDown.fBorderWidth <> DefStyles.fPSDown.fBorderWidth then
            SL.Add( Prefix + AName + '.Down_BorderWidth := '+Int2Str(Styles.fPSDown.fBorderWidth)+';');
        if Styles.fPSDis.fBorderWidth <> DefStyles.fPSDis.fBorderWidth then
            SL.Add( Prefix + AName + '.Dis_BorderWidth := '+Int2Str(Styles.fPSDis.fBorderWidth)+';');
    end;

    if (Styles.fPSDef.fGradientStyle = Styles.fPSOver.fGradientStyle)
        and (Styles.fPSDef.fGradientStyle = Styles.fPSDown.fGradientStyle)
        and (Styles.fPSDef.fGradientStyle = Styles.fPSDis.fGradientStyle)
        and (Styles.fPSDef.fGradientStyle <> DefStyles.fPSDef.fGradientStyle) then
        SL.Add( Prefix + AName + '.All_GradientStyle := '+GradientStyles[Styles.fPSDef.fGradientStyle]+';')
    else begin
        if Styles.fPSDef.fGradientStyle <> DefStyles.fPSDef.fGradientStyle then
            SL.Add( Prefix + AName + '.Def_GradientStyle := '+GradientStyles[Styles.fPSDef.fGradientStyle]+';');
        if Styles.fPSOver.fGradientStyle <> DefStyles.fPSOver.fGradientStyle then
            SL.Add( Prefix + AName + '.Over_GradientStyle := '+GradientStyles[Styles.fPSOver.fGradientStyle]+';');
        if Styles.fPSDown.fGradientStyle <> DefStyles.fPSDown.fGradientStyle then
            SL.Add( Prefix + AName + '.Down_GradientStyle := '+GradientStyles[Styles.fPSDown.fGradientStyle]+';');
        if Styles.fPSDis.fGradientStyle <> DefStyles.fPSDis.fGradientStyle then
            SL.Add( Prefix + AName + '.Dis_GradientStyle := '+GradientStyles[Styles.fPSDis.fGradientStyle]+';');
    end;

    if (Styles.fPSDef.fShadowOffset = Styles.fPSOver.fShadowOffset)
        and (Styles.fPSDef.fShadowOffset = Styles.fPSDown.fShadowOffset)
        and (Styles.fPSDef.fShadowOffset = Styles.fPSDis.fShadowOffset)
        and (Styles.fPSDef.fShadowOffset <> DefStyles.fPSDef.fShadowOffset) then
        SL.Add( Prefix + AName + '.All_ShadowOffset := '+Int2Str(Styles.fPSDef.fShadowOffset)+';')
    else begin
        if Styles.fPSDef.fShadowOffset <> DefStyles.fPSDef.fShadowOffset then
            SL.Add( Prefix + AName + '.Def_ShadowOffset := '+Int2Str(Styles.fPSDef.fShadowOffset)+';');
        if Styles.fPSOver.fShadowOffset <> DefStyles.fPSOver.fShadowOffset then
            SL.Add( Prefix + AName + '.Over_ShadowOffset := '+Int2Str(Styles.fPSOver.fShadowOffset)+';');
        if Styles.fPSDown.fShadowOffset <> DefStyles.fPSDown.fShadowOffset then
            SL.Add( Prefix + AName + '.Down_ShadowOffset := '+Int2Str(Styles.fPSDown.fShadowOffset)+';');
        if Styles.fPSDis.fShadowOffset <> DefStyles.fPSDis.fShadowOffset then
            SL.Add( Prefix + AName + '.Dis_ShadowOffset := '+Int2Str(Styles.fPSDis.fShadowOffset)+';');
    end;

    if (Styles.fPSDef.fGlyphItemX = Styles.fPSOver.fGlyphItemX)
        and (Styles.fPSDef.fGlyphItemX = Styles.fPSDown.fGlyphItemX)
        and (Styles.fPSDef.fGlyphItemX = Styles.fPSDis.fGlyphItemX)
        and (Styles.fPSDef.fGlyphItemX <> DefStyles.fPSDef.fGlyphItemX) then
        SL.Add( Prefix + AName + '.All_GlyphItemX := '+Int2Str(Styles.fPSDef.fGlyphItemX)+';')
    else begin
        if Styles.fPSDef.fGlyphItemX <> DefStyles.fPSDef.fGlyphItemX then
            SL.Add( Prefix + AName + '.Def_GlyphItemX := '+Int2Str(Styles.fPSDef.fGlyphItemX)+';');
        if Styles.fPSOver.fGlyphItemX <> DefStyles.fPSOver.fGlyphItemX then
            SL.Add( Prefix + AName + '.Over_GlyphItemX := '+Int2Str(Styles.fPSOver.fGlyphItemX)+';');
        if Styles.fPSDown.fGlyphItemX <> DefStyles.fPSDown.fGlyphItemX then
            SL.Add( Prefix + AName + '.Down_GlyphItemX := '+Int2Str(Styles.fPSDown.fGlyphItemX)+';');
        if Styles.fPSDis.fGlyphItemX <> DefStyles.fPSDis.fGlyphItemX then
            SL.Add( Prefix + AName + '.Dis_GlyphItemX := '+Int2Str(Styles.fPSDis.fGlyphItemX)+';');
    end;

    if (Styles.fPSDef.fGlyphItemY = Styles.fPSOver.fGlyphItemY)
        and (Styles.fPSDef.fGlyphItemY = Styles.fPSDown.fGlyphItemY)
        and (Styles.fPSDef.fGlyphItemY = Styles.fPSDis.fGlyphItemY)
        and (Styles.fPSDef.fGlyphItemY <> DefStyles.fPSDef.fGlyphItemY) then
        SL.Add( Prefix + AName + '.All_GlyphItemY := '+Int2Str(Styles.fPSDef.fGlyphItemY)+';')
    else begin
        if Styles.fPSDef.fGlyphItemY <> DefStyles.fPSDef.fGlyphItemY then
            SL.Add( Prefix + AName + '.Def_GlyphItemY := '+Int2Str(Styles.fPSDef.fGlyphItemY)+';');
        if Styles.fPSOver.fGlyphItemY <> DefStyles.fPSOver.fGlyphItemY then
            SL.Add( Prefix + AName + '.Over_GlyphItemY := '+Int2Str(Styles.fPSOver.fGlyphItemY)+';');
        if Styles.fPSDown.fGlyphItemY <> DefStyles.fPSDown.fGlyphItemY then
            SL.Add( Prefix + AName + '.Down_GlyphItemY := '+Int2Str(Styles.fPSDown.fGlyphItemY)+';');
        if Styles.fPSDis.fGlyphItemY <> DefStyles.fPSDis.fGlyphItemY then
            SL.Add( Prefix + AName + '.Dis_GlyphItemY := '+Int2Str(Styles.fPSDis.fGlyphItemY)+';');
    end;
end;

procedure P_SetUpState4States (aOwner: TComponent; Styles: TKOLGrushStyles;
    SL: TStringList; DefStyles: TKOLGrushStyles);
begin
    if (Styles.fPSDef.fColorFrom = Styles.fPSOver.fColorFrom)
        and (Styles.fPSDef.fColorFrom = Styles.fPSDown.fColorFrom)
        and (Styles.fPSDef.fColorFrom = Styles.fPSDis.fColorFrom)
        and (Styles.fPSDef.fColorFrom <> DefStyles.fPSDef.fColorFrom) then
        SL.Add( ' L(' + IntToStr( Styles.fPSDef.fColorFrom ) + ') C1 GR0O_.SetAll_ColorFrom<2>' )
    else begin
        if Styles.fPSDef.fColorFrom <> DefStyles.fPSDef.fColorFrom then
            SL.Add( ' L(' + IntToStr( Styles.fPSDef.fColorFrom ) + ') C1 GR0O_.SetDef_ColorFrom<2>' );
        if Styles.fPSOver.fColorFrom <> DefStyles.fPSOver.fColorFrom then
            SL.Add( ' L(' + IntToStr( Styles.fPSOver.fColorFrom ) + ') C1 GR0O_.SetOver_ColorFrom<2>' );
        if Styles.fPSDown.fColorFrom <> DefStyles.fPSDown.fColorFrom then
            SL.Add( ' L(' + IntToStr( Styles.fPSDown.fColorFrom ) + ') C1 GR0O_.SetDown_ColorFrom<2>' );
        if Styles.fPSDis.fColorFrom <> DefStyles.fPSDis.fColorFrom then
            SL.Add( ' L(' + IntToStr( Styles.fPSDis.fColorFrom ) + ') C1 GR0O_.SetDis_ColorFrom<2>' );
    end;

    if (Styles.fPSDef.fColorTo = Styles.fPSOver.fColorTo)
        and (Styles.fPSDef.fColorTo = Styles.fPSDown.fColorTo)
        and (Styles.fPSDef.fColorTo = Styles.fPSDis.fColorTo)
        and (Styles.fPSDef.fColorTo <> DefStyles.fPSDef.fColorTo) then
        SL.Add( ' L(' + IntToStr( Styles.fPSDef.fColorTo ) + ') C1 GR0O_.SetAll_ColorTo<2>' )
    else begin
        if Styles.fPSDef.fColorTo <> DefStyles.fPSDef.fColorTo then
            SL.Add( ' L(' + IntToStr( Styles.fPSDef.fColorTo ) + ') C1 GR0O_.SetDef_ColorTo<2>' );
        if Styles.fPSOver.fColorTo <> DefStyles.fPSOver.fColorTo then
            SL.Add( ' L(' + IntToStr( Styles.fPSOver.fColorTo ) + ') C1 GR0O_.SetOver_ColorTo<2>' );
        if Styles.fPSDown.fColorTo <> DefStyles.fPSDown.fColorTo then
            SL.Add( ' L(' + IntToStr( Styles.fPSDown.fColorTo ) + ') C1 GR0O_.SetDown_ColorTo<2>' );
        if Styles.fPSDis.fColorTo <> DefStyles.fPSDis.fColorTo then
            SL.Add( ' L(' + IntToStr( Styles.fPSDis.fColorTo ) + ') C1 GR0O_.SetDis_ColorTo<2>' );
    end;

    if (Styles.fPSDef.fColorOuter = Styles.fPSOver.fColorOuter)
        and (Styles.fPSDef.fColorOuter = Styles.fPSDown.fColorOuter)
        and (Styles.fPSDef.fColorOuter = Styles.fPSDis.fColorOuter)
        and (Styles.fPSDef.fColorOuter <> DefStyles.fPSDef.fColorOuter) then
        SL.Add( ' L(' + IntToStr( Styles.fPSDef.fColorOuter ) + ') C1 GR0O_.SetAll_ColorOuter<2>' )
    else begin
        if Styles.fPSDef.fColorOuter <> DefStyles.fPSDef.fColorOuter then
            SL.Add( ' L(' + IntToStr( Styles.fPSDef.fColorOuter ) + ') C1 GR0O_.SetDef_ColorOuter<2>' );
        if Styles.fPSOver.fColorOuter <> DefStyles.fPSOver.fColorOuter then
            SL.Add( ' L(' + IntToStr( Styles.fPSOver.fColorOuter ) + ') C1 GR0O_.SetOver_ColorOuter<2>' );
        if Styles.fPSDown.fColorOuter <> DefStyles.fPSDown.fColorOuter then
            SL.Add( ' L(' + IntToStr( Styles.fPSDown.fColorOuter ) + ') C1 GR0O_.SetDown_ColorOuter<2>' );
        if Styles.fPSDis.fColorOuter <> DefStyles.fPSDis.fColorOuter then
            SL.Add( ' L(' + IntToStr( Styles.fPSDis.fColorOuter ) + ') C1 GR0O_.SetDis_ColorOuter<2>' );
    end;

    if (Styles.fPSDef.fColorText = Styles.fPSOver.fColorText)
        and (Styles.fPSDef.fColorText = Styles.fPSDown.fColorText)
        and (Styles.fPSDef.fColorText = Styles.fPSDis.fColorText)
        and (Styles.fPSDef.fColorText <> DefStyles.fPSDef.fColorText) then
        SL.Add( ' L(' + IntToStr( Styles.fPSDef.fColorText ) + ') C1 GR0O_.SetAll_ColorText<2>' )
    else begin
        if Styles.fPSDef.fColorText <> DefStyles.fPSDef.fColorText then
            SL.Add( ' L(' + IntToStr( Styles.fPSDef.fColorText ) + ') C1 GR0O_.SetDef_ColorText<2>' );
        if Styles.fPSOver.fColorText <> DefStyles.fPSOver.fColorText then
            SL.Add( ' L(' + IntToStr( Styles.fPSOver.fColorText ) + ') C1 GR0O_.SetOver_ColorText<2>' );
        if Styles.fPSDown.fColorText <> DefStyles.fPSDown.fColorText then
            SL.Add( ' L(' + IntToStr( Styles.fPSDown.fColorText ) + ') C1 GR0O_.SetDown_ColorText<2>' );
        if Styles.fPSDis.fColorText <> DefStyles.fPSDis.fColorText then
            SL.Add( ' L(' + IntToStr( Styles.fPSDis.fColorText ) + ') C1 GR0O_.SetDis_ColorText<2>' );
    end;

    if (Styles.fPSDef.fColorShadow = Styles.fPSOver.fColorShadow)
        and (Styles.fPSDef.fColorShadow = Styles.fPSDown.fColorShadow)
        and (Styles.fPSDef.fColorShadow = Styles.fPSDis.fColorShadow)
        and (Styles.fPSDef.fColorShadow <> DefStyles.fPSDef.fColorShadow) then
        SL.Add( ' L(' + IntToStr( Styles.fPSDef.fColorShadow ) + ') C1 GR0O_.SetAll_ColorShadow<2>' )
    else begin
        if Styles.fPSDef.fColorShadow <> DefStyles.fPSDef.fColorShadow then
            SL.Add( ' L(' + IntToStr( Styles.fPSDef.fColorShadow ) + ') C1 GR0O_.SetDef_ColorShadow<2>' );
        if Styles.fPSOver.fColorShadow <> DefStyles.fPSOver.fColorShadow then
            SL.Add( ' L(' + IntToStr( Styles.fPSOver.fColorShadow ) + ') C1 GR0O_.SetOver_ColorShadow<2>' );
        if Styles.fPSDown.fColorShadow <> DefStyles.fPSDown.fColorShadow then
            SL.Add( ' L(' + IntToStr( Styles.fPSDown.fColorShadow ) + ') C1 GR0O_.SetDown_ColorShadow<2>' );
        if Styles.fPSDis.fColorShadow <> DefStyles.fPSDis.fColorShadow then
            SL.Add( ' L(' + IntToStr( Styles.fPSDis.fColorShadow ) + ') C1 GR0O_.SetDis_ColorShadow<2>' );
    end;

    if (Styles.fPSDef.fBorderColor = Styles.fPSOver.fBorderColor)
        and (Styles.fPSDef.fBorderColor = Styles.fPSDown.fBorderColor)
        and (Styles.fPSDef.fBorderColor = Styles.fPSDis.fBorderColor)
        and (Styles.fPSDef.fBorderColor <> DefStyles.fPSDef.fBorderColor) then
        SL.Add( ' L(' + IntToStr( Styles.fPSDef.fBorderColor ) + ') C1 GR0O_.SetAll_BorderColor<2>' )
    else begin
        if Styles.fPSDef.fBorderColor <> DefStyles.fPSDef.fBorderColor then
            SL.Add( ' L(' + IntToStr( Styles.fPSDef.fBorderColor ) + ') C1 GR0O_.SetDef_BorderColor<2>' );
        if Styles.fPSOver.fBorderColor <> DefStyles.fPSOver.fBorderColor then
            SL.Add( ' L(' + IntToStr( Styles.fPSOver.fBorderColor ) + ') C1 GR0O_.SetOver_BorderColor<2>' );
        if Styles.fPSDown.fBorderColor <> DefStyles.fPSDown.fBorderColor then
            SL.Add( ' L(' + IntToStr( Styles.fPSDown.fBorderColor ) + ') C1 GR0O_.SetDown_BorderColor<2>' );
        if Styles.fPSDis.fBorderColor <> DefStyles.fPSDis.fBorderColor then
            SL.Add( ' L(' + IntToStr( Styles.fPSDis.fBorderColor ) + ') C1 GR0O_.SetDis_BorderColor<2>' );
    end;

    if (Styles.fPSDef.fBorderRoundWidth = Styles.fPSOver.fBorderRoundWidth)
        and (Styles.fPSDef.fBorderRoundWidth = Styles.fPSDown.fBorderRoundWidth)
        and (Styles.fPSDef.fBorderRoundWidth = Styles.fPSDis.fBorderRoundWidth)
        and (Styles.fPSDef.fBorderRoundWidth <> DefStyles.fPSDef.fBorderRoundWidth) then
        SL.Add( ' L(' + IntToStr( Styles.fPSDef.fBorderRoundWidth ) + ') C1 GR0O_.SetAll_BorderRoundWidth<2>' )
    else begin
        if Styles.fPSDef.fBorderRoundWidth <> DefStyles.fPSDef.fBorderRoundWidth then
            SL.Add( ' L(' + IntToStr( Styles.fPSDef.fBorderRoundWidth ) + ') C1 GR0O_.SetDef_BorderRoundWidth<2>' );
        if Styles.fPSOver.fBorderRoundWidth <> DefStyles.fPSOver.fBorderRoundWidth then
            SL.Add( ' L(' + IntToStr( Styles.fPSOver.fBorderRoundWidth ) + ') C1 GR0O_.SetOver_BorderRoundWidth<2>' );
        if Styles.fPSDown.fBorderRoundWidth <> DefStyles.fPSDown.fBorderRoundWidth then
            SL.Add( ' L(' + IntToStr( Styles.fPSDown.fBorderRoundWidth ) + ') C1 GR0O_.SetDown_BorderRoundWidth<2>' );
        if Styles.fPSDis.fBorderRoundWidth <> DefStyles.fPSDis.fBorderRoundWidth then
            SL.Add( ' L(' + IntToStr( Styles.fPSDis.fBorderRoundWidth ) + ') C1 GR0O_.SetDis_BorderRoundWidth<2>' );
    end;

    if (Styles.fPSDef.fBorderRoundHeight = Styles.fPSOver.fBorderRoundHeight)
        and (Styles.fPSDef.fBorderRoundHeight = Styles.fPSDown.fBorderRoundHeight)
        and (Styles.fPSDef.fBorderRoundHeight = Styles.fPSDis.fBorderRoundHeight)
        and (Styles.fPSDef.fBorderRoundHeight <> DefStyles.fPSDef.fBorderRoundHeight) then
        SL.Add( ' L(' + IntToStr( Styles.fPSDef.fBorderRoundHeight ) + ') C1 GR0O_.SetAll_BorderRoundHeight<2>' )
    else begin
        if Styles.fPSDef.fBorderRoundHeight <> DefStyles.fPSDef.fBorderRoundHeight then
            SL.Add( ' L(' + IntToStr( Styles.fPSDef.fBorderRoundHeight ) + ') C1 GR0O_.SetDef_BorderRoundHeight<2>' );
        if Styles.fPSOver.fBorderRoundHeight <> DefStyles.fPSOver.fBorderRoundHeight then
            SL.Add( ' L(' + IntToStr( Styles.fPSOver.fBorderRoundHeight ) + ') C1 GR0O_.SetOver_BorderRoundHeight<2>' );
        if Styles.fPSDown.fBorderRoundHeight <> DefStyles.fPSDown.fBorderRoundHeight then
            SL.Add( ' L(' + IntToStr( Styles.fPSDown.fBorderRoundHeight ) + ') C1 GR0O_.SetDown_BorderRoundHeight<2>' );
        if Styles.fPSDis.fBorderRoundHeight <> DefStyles.fPSDis.fBorderRoundHeight then
            SL.Add( ' L(' + IntToStr( Styles.fPSDis.fBorderRoundHeight ) + ') C1 GR0O_.SetDis_BorderRoundHeight<2>' );
    end;

    if (Styles.fPSDef.fBorderWidth = Styles.fPSOver.fBorderWidth)
        and (Styles.fPSDef.fBorderWidth = Styles.fPSDown.fBorderWidth)
        and (Styles.fPSDef.fBorderWidth = Styles.fPSDis.fBorderWidth)
        and (Styles.fPSDef.fBorderWidth <> DefStyles.fPSDef.fBorderWidth) then
        SL.Add( ' L(' + IntToStr( Styles.fPSDef.fBorderWidth ) + ') C1 GR0O_.SetAll_BorderWidth<2>' )
    else begin
        if Styles.fPSDef.fBorderWidth <> DefStyles.fPSDef.fBorderWidth then
            SL.Add( ' L(' + IntToStr( Styles.fPSDef.fBorderWidth ) + ') C1 GR0O_.SetDef_BorderWidth<2>' );
        if Styles.fPSOver.fBorderWidth <> DefStyles.fPSOver.fBorderWidth then
            SL.Add( ' L(' + IntToStr( Styles.fPSOver.fBorderWidth ) + ') C1 GR0O_.SetOver_BorderWidth<2>' );
        if Styles.fPSDown.fBorderWidth <> DefStyles.fPSDown.fBorderWidth then
            SL.Add( ' L(' + IntToStr( Styles.fPSDown.fBorderWidth ) + ') C1 GR0O_.SetDown_BorderWidth<2>' );
        if Styles.fPSDis.fBorderWidth <> DefStyles.fPSDis.fBorderWidth then
            SL.Add( ' L(' + IntToStr( Styles.fPSDis.fBorderWidth ) + ') C1 GR0O_.SetDis_BorderWidth<2>' );
    end;

    if (Styles.fPSDef.fGradientStyle = Styles.fPSOver.fGradientStyle)
        and (Styles.fPSDef.fGradientStyle = Styles.fPSDown.fGradientStyle)
        and (Styles.fPSDef.fGradientStyle = Styles.fPSDis.fGradientStyle)
        and (Styles.fPSDef.fGradientStyle <> DefStyles.fPSDef.fGradientStyle) then
        SL.Add( ' L(' + IntToStr( Byte (Styles.fPSDef.fGradientStyle ) ) + ') C1 GR0O_.SetAll_GradientStyle<2>' )
    else begin
        if Styles.fPSDef.fGradientStyle <> DefStyles.fPSDef.fGradientStyle then
            SL.Add( ' L(' + IntToStr( Byte (Styles.fPSDef.fGradientStyle ) ) + ') C1 GR0O_.SetDef_GradientStyle<2>' );
        if Styles.fPSOver.fGradientStyle <> DefStyles.fPSOver.fGradientStyle then
            SL.Add( ' L(' + IntToStr( Byte (Styles.fPSOver.fGradientStyle ) ) + ') C1 GR0O_.SetOver_GradientStyle<2>' );
        if Styles.fPSDown.fGradientStyle <> DefStyles.fPSDown.fGradientStyle then
            SL.Add( ' L(' + IntToStr( Byte (Styles.fPSDown.fGradientStyle ) ) + ') C1 GR0O_.SetDown_GradientStyle<2>' );
        if Styles.fPSDis.fGradientStyle <> DefStyles.fPSDis.fGradientStyle then
            SL.Add( ' L(' + IntToStr( Byte (Styles.fPSDis.fGradientStyle ) ) + ') C1 GR0O_.SetDis_GradientStyle<2>' );
    end;

    if (Styles.fPSDef.fShadowOffset = Styles.fPSOver.fShadowOffset)
        and (Styles.fPSDef.fShadowOffset = Styles.fPSDown.fShadowOffset)
        and (Styles.fPSDef.fShadowOffset = Styles.fPSDis.fShadowOffset)
        and (Styles.fPSDef.fShadowOffset <> DefStyles.fPSDef.fShadowOffset) then
        SL.Add( ' L(' + IntToStr( Styles.fPSDef.fShadowOffset ) + ') C1 GR0O_.SetAll_ShadowOffset<2>' )
    else begin
        if Styles.fPSDef.fShadowOffset <> DefStyles.fPSDef.fShadowOffset then
            SL.Add( ' L(' + IntToStr( Styles.fPSDef.fShadowOffset ) + ') C1 GR0O_.SetDef_ShadowOffset<2>' );
        if Styles.fPSOver.fShadowOffset <> DefStyles.fPSOver.fShadowOffset then
            SL.Add( ' L(' + IntToStr( Styles.fPSOver.fShadowOffset ) + ') C1 GR0O_.SetOver_ShadowOffset<2>' );
        if Styles.fPSDown.fShadowOffset <> DefStyles.fPSDown.fShadowOffset then
            SL.Add( ' L(' + IntToStr( Styles.fPSDown.fShadowOffset ) + ') C1 GR0O_.SetDown_ShadowOffset<2>' );
        if Styles.fPSDis.fShadowOffset <> DefStyles.fPSDis.fShadowOffset then
            SL.Add( ' L(' + IntToStr( Styles.fPSDis.fShadowOffset ) + ') C1 GR0O_.SetDis_ShadowOffset<2>' );
    end;

    if (Styles.fPSDef.fGlyphItemX = Styles.fPSOver.fGlyphItemX)
        and (Styles.fPSDef.fGlyphItemX = Styles.fPSDown.fGlyphItemX)
        and (Styles.fPSDef.fGlyphItemX = Styles.fPSDis.fGlyphItemX)
        and (Styles.fPSDef.fGlyphItemX <> DefStyles.fPSDef.fGlyphItemX) then
        SL.Add( ' L(' + IntToStr( Styles.fPSDef.fGlyphItemX ) + ') C1 GR0O_.SetAll_GlyphItemX<2>' )
    else begin
        if Styles.fPSDef.fGlyphItemX <> DefStyles.fPSDef.fGlyphItemX then
            SL.Add( ' L(' + IntToStr( Styles.fPSDef.fGlyphItemX ) + ') C1 GR0O_.SetDef_GlyphItemX<2>' );
        if Styles.fPSOver.fGlyphItemX <> DefStyles.fPSOver.fGlyphItemX then
            SL.Add( ' L(' + IntToStr( Styles.fPSOver.fGlyphItemX ) + ') C1 GR0O_.SetOver_GlyphItemX<2>' );
        if Styles.fPSDown.fGlyphItemX <> DefStyles.fPSDown.fGlyphItemX then
            SL.Add( ' L(' + IntToStr( Styles.fPSDown.fGlyphItemX ) + ') C1 GR0O_.SetDown_GlyphItemX<2>' );
        if Styles.fPSDis.fGlyphItemX <> DefStyles.fPSDis.fGlyphItemX then
            SL.Add( ' L(' + IntToStr( Styles.fPSDis.fGlyphItemX ) + ') C1 GR0O_.SetDis_GlyphItemX<2>' );
    end;

    if (Styles.fPSDef.fGlyphItemY = Styles.fPSOver.fGlyphItemY)
        and (Styles.fPSDef.fGlyphItemY = Styles.fPSDown.fGlyphItemY)
        and (Styles.fPSDef.fGlyphItemY = Styles.fPSDis.fGlyphItemY)
        and (Styles.fPSDef.fGlyphItemY <> DefStyles.fPSDef.fGlyphItemY) then
        SL.Add( ' L(' + IntToStr( Styles.fPSDef.fGlyphItemY ) + ') C1 GR0O_.SetAll_GlyphItemY<2>' )
    else begin
        if Styles.fPSDef.fGlyphItemY <> DefStyles.fPSDef.fGlyphItemY then
            SL.Add( ' L(' + IntToStr( Styles.fPSDef.fGlyphItemY ) + ') C1 GR0O_.SetDef_GlyphItemY<2>' );
        if Styles.fPSOver.fGlyphItemY <> DefStyles.fPSOver.fGlyphItemY then
            SL.Add( ' L(' + IntToStr( Styles.fPSOver.fGlyphItemY ) + ') C1 GR0O_.SetOver_GlyphItemY<2>' );
        if Styles.fPSDown.fGlyphItemY <> DefStyles.fPSDown.fGlyphItemY then
            SL.Add( ' L(' + IntToStr( Styles.fPSDown.fGlyphItemY ) + ') C1 GR0O_.SetDown_GlyphItemY<2>' );
        if Styles.fPSDis.fGlyphItemY <> DefStyles.fPSDis.fGlyphItemY then
            SL.Add( ' L(' + IntToStr( Styles.fPSDis.fGlyphItemY ) + ') C1 GR0O_.SetDis_GlyphItemY<2>' );
    end;
end;


    {if fStyles.fGlyphWidth <> CtlStyles.fGlyphWidth then
        SL.Add( Prefix + AName + '.All_GlyphWidth := '+int2str(fStyles.GlyphWidth)+';');
    if fStyles.fGlyphHeight <> CtlStyles.fGlyphHeight then
        SL.Add( Prefix + AName + '.All_GlyphHeight := '+int2str(fStyles.GlyphHeight)+';');
    if fStyles.fCheckMetric <> CtlStyles.fCheckMetric then
        SL.Add( Prefix + AName + '.All_CheckMetric := '+int2str(fStyles.CheckMetric)+';');
    if fStyles.fColorCheck <> CtlStyles.fColorCheck then
        SL.Add( Prefix + AName + '.All_ColorCheck := $'+Int2Hex(fStyles.ColorCheck, 6)+';');
    if fStyles.GlyphVAlign <> CtlStyles.fGlyphVAlign then
        SL.Add( Prefix + AName + '.All_GlyphVAlign := '+TVAligns[fStyles.GlyphVAlign]+';');
    if fStyles.GlyphHAlign <> CtlStyles.fGlyphHAlign then
        SL.Add( Prefix + AName + '.All_GlyphHAlign := '+THAligns[fStyles.GlyphHAlign]+';');
    if fStyles.TextVAlign <> CtlStyles.TextVAlign then
        SL.Add( Prefix + AName + '.All_TextVAlign := '+TVAligns[fStyles.TextVAlign]+';');
    if fStyles.TextHAlign <> CtlStyles.TextHAlign then
        SL.Add( Prefix + AName + '.All_TextHAlign := '+THAligns[fStyles.TextHAlign]+';');
    if fStyles.DrawGlyph <> CtlStyles.DrawGlyph then
        SL.Add( Prefix + AName + '.All_DrawGlyph := '+Booleans[fStyles.DrawGlyph]+';');
    if fStyles.DrawText <> CtlStyles.DrawText then
        SL.Add( Prefix + AName + '.All_DrawText := '+Booleans[fStyles.DrawText]+';');
    if fStyles.DrawFocusRect <> CtlStyles.DrawFocusRect then
        SL.Add( Prefix + AName + '.All_DrawFocusRect := '+Booleans[fStyles.DrawFocusRect]+';');
    if fStyles.DrawProgress <> CtlStyles.DrawProgress then
        SL.Add( Prefix + AName + '.All_DrawProgress := '+Booleans[fStyles.DrawProgress]+';');
    if fStyles.DrawProgressRect <> CtlStyles.DrawProgressRect then
        SL.Add( Prefix + AName + '.All_DrawProgressRect := '+Booleans[fStyles.DrawProgressRect]+';');
    if fStyles.GlyphAttached <> CtlStyles.GlyphAttached then
        SL.Add( Prefix + AName + '.All_GlyphAttached := '+Booleans[fStyles.GlyphAttached]+';');
    if fStyles.CropTopFirst <> CtlStyles.CropTopFirst then
        SL.Add( Prefix + AName + '.All_CropTopFirst := '+Booleans[fStyles.CropTopFirst]+';');
    if fStyles.AntiAliasing <> CtlStyles.AntiAliasing then
        SL.Add( Prefix + AName + '.All_AntiAliasing := '+Booleans[fStyles.AntiAliasing]+';');
    if fStyles.ProgressVertical <> CtlStyles.ProgressVertical then
        SL.Add( Prefix + AName + '.All_ProgressVertical := '+Booleans[fStyles.ProgressVertical]+';');
    if fStyles.UpdateSpeed <> CtlStyles.UpdateSpeed then
        SL.Add( Prefix + AName + '.All_UpdateSpeed := '+TGRushSpeeds[fStyles.UpdateSpeed]+';');
    if fStyles.Spacing <> CtlStyles.Spacing then
        SL.Add( Prefix + AName + '.All_Spacing := '+int2str(fStyles.Spacing)+';');}

procedure GenerateCustomResource( Resource: Classes.TStream; const FileName: String;
    const ResName: string; ResType: PChar );
const   header: array [0..31] of char = #0#0#0#0#32#0#0#0#255#255#0#0#255#255#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0;
        postheader: array [0..17] of char = #0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0;
var     TempStr: WideString;
        OutFile: TFileStream;
        I: integer;
begin
    DeleteFile(FileName);
    Resource.Position := 0;
    OutFile := TFileStream.Create(FileName, fmCreate);
    OutFile.Write(header, 32);
    I := Resource.Size;
    OutFile.Write(I, 4);
    I := (length(ResName) + 10 + Length(ResType))*2+8;
    OutFile.Write(I, 4);
    TempStr := ResType;
    OutFile.Write(Pointer(TempStr)^, 2*length(TempStr));
    I := 0;
    OutFile.Write(I, 2);
    TempStr := ResName;
    OutFile.Write(Pointer(TempStr)^, 2*length(TempStr));
    OutFile.Write(postheader, 18);
    OutFile.CopyFrom(Resource, Resource.Size);
    OutFile.Free;
end;


//******************************************************************************
//  GRush ImageCollection
//******************************************************************************

constructor TKOLGRushImageCollection.Create;
begin
    inherited;
    NeedFree := FALSE;
    fImageType := None;
end;

destructor TKOLGRushImageCollection.Destroy;
begin
    try
        fDataStream.Free;
    finally
        fDataStream := nil;
    end;
    inherited;
end;


function TKOLGRushImageCollection.Pcode_Generate: Boolean;
begin
    Result := TRUE;
end;

function TKOLGRushImageCollection.TypeName: String;
begin
    Result := 'GRushImageCollection';
end;

function TKOLGRushImageCollection.GetResourceName: String;
begin
    Result := UpperCase(Self.ParentForm.Name + '_' + Self.Name);
end;

function TKOLGRushImageCollection.GetResourceFileName: String;
begin
    Result := Self.ParentForm.Name + '_' + Self.Name;
end;

procedure TKOLGRushImageCollection.SetImageType (Value: TKOLGRushImageCollectionImageType);
var     OSD: KOL.POpenSaveDialog;
        KOLBitmap: KOL.PBitmap;
        KOLStream: KOL.PStream;
        ret: DWORD;
        ActiveWindow: HWnd;
        WindowList: Pointer;
        Ex: Boolean;
begin
    if Value = fImageType then exit;
    if csReading in ComponentState then begin
        fImageType := Value;
        exit;
    end;
    if fImageType <> None then begin
        if Value <> None then begin
            MessageBox(Self.ParentForm.Handle, 'The image type was automatically detected by the file'
                + ' content and can not be changed. Select "None" first to free current image.'
                , '', MB_ICONQUESTION);
            exit;
        end;
        fImageType := Value;
        try
            fDataStream.Free;
        except
            ShowMessage('Произошла ошибка при смене типа картинки.');
        end;
        fDataStream := nil;
    end else {if not assigned(fDataStream) then }begin
        if assigned(fDataStream) then
            fDataStream.Free;
        fDataStream := nil;
        OSD := NewOpenSaveDialog('chose file to open', ProjectSourcePath
            , [OSFileMustExist, OSHideReadonly, OSPathMustExist] );
        OSD.Filter := 'Jpeg files|*.jpg;*.jpeg|Png files|*.png|Gif files|*.gif|Bmp files|*.bmp|'
            + 'All suported files|*.jpg;*.jpeg;*.png;*.gif;*.bmp|All files|*.*|';
        OSD.FilterIndex := 5;
        ActiveWindow := GetActiveWindow;
        WindowList := DisableTaskWindows(0);
        Ex := OSD.Execute;
        EnableTaskWindows(WindowList);
        SetActiveWindow(ActiveWindow);
        if Ex then begin
            KOLStream := NewReadFileStream(OSD.Filename);
            try
                ret := tinyPNG.tinyLoadPNG(KOLBitmap, KOLStream);
            except
                ShowMessage('Произошла ошибка во время попытки декадировать файл как *.png'
                    + '. Пожалуста сообщите об этом автору (homm86@mail.ru) и прекрепите'
                    + ' проблемный файл если его размер менее мегабайта.');
                ret := tinyERROR_NotPNGFile;
            end;
            KOLStream.Free;
            if (KOLBitmap <> nil) and (ret = tinyPNG.tinyERROR_OK) then begin
                fDataStream := TMemoryStream.Create;
                fDataStream.LoadFromFile(OSD.Filename);
                fImageType := PNG;
            end;
            KOLBitmap.Free;
            if not assigned(fDataStream) then begin
                tinyJPGGIFBMP.tinyLoadJPGGIFBMPFile(KOLBitmap, OSD.FileName);
                if (KOLBitmap <> nil) then begin
                    fDataStream := TMemoryStream.Create;
                    fDataStream.LoadFromFile(OSD.Filename);
                    fImageType := BMP_GIF_JPG;
                end;
                KOLBitmap.Free;
                if not assigned(fDataStream) then begin
                    ShowMessage('This file format not supported.');
                end;
            end;
        end;
        OSD.Free;
    end;
    Change;
end;

procedure TKOLGRushImageCollection.SetItemWidth (Value: DWORD);
begin
    fItemWidth := Value;
    Change;
end;

procedure TKOLGRushImageCollection.SetItemHeight (Value: DWORD);
begin
    fItemHeight := Value;
    Change;
end;

procedure TKOLGRushImageCollection.DefineProperties(Filer: TFiler);
begin
    inherited;
    Filer.DefineBinaryProperty('Data', ReadData, WriteData, fImageType <> None);
end;

procedure TKOLGRushImageCollection.ReadData(Stream: Classes.TStream);
var     _t: DWORD;
begin
    Stream.Read(_t, 4);
    if not assigned(fDataStream) then begin
        fDataStream := TMemoryStream.Create;
    end;
    fDataStream.Clear;
    fDataStream.Position := 0;
    fDataStream.CopyFrom(Stream, _t);
end;

procedure TKOLGRushImageCollection.WriteData(Stream: Classes.TStream);
var     _t: DWORD;
begin
    _t := fDataStream.Size;
    Stream.Write(_t, 4);
    fDataStream.Position := 0;
    Stream.CopyFrom(fDataStream, _t );
end;

function TKOLGRushImageCollection.AdditionalUnits: String;
begin
    Result := '';
    if fNotifyList.Count = 0 then exit;
    if fImageType = PNG then
        Result := ', tinyPNG'
    else if fImageType <> None then
        Result := ', tinyJPGGIFBMP'
end;

procedure TKOLGRushImageCollection.SetupFirst(SL: TStringList; const AName, AParent, Prefix: String);
begin
    if fNotifyList.Count = 0 then exit;
    if fImageType = PNG then begin
        SL.Add( Format(Prefix + 'tinyLoadPNGResource( %s, HINSTANCE, ''%s'', ''GRUSHCOLLECTIONS'');'
            , [ AName, GetResourceName ] ) );
    end else if fImageType <> None then begin
        SL.Add( Format(Prefix + 'tinyLoadJPGGIFBMPResource( %s, HINSTANCE, ''%s'', ''GRUSHCOLLECTIONS'');'
            , [ AName, GetResourceName ] ) );
    end;
    if (fImageType <> None) and assigned(fDataStream) then begin
        GenerateCustomResource(fDataStream, ProjectSourcePath + GetResourceFileName + '.res'
            , GetResourceName, 'GRUSHCOLLECTIONS');
        SL.Add(Prefix + '{$R '+ GetResourceFileName + '.res}');
    end;
end;

procedure TKOLGRushImageCollection.P_SetupFirst(SL: TStringList; const AName, AParent, Prefix: String);
begin

end;

procedure TKOLGRushImageCollection.SetupLast( SL: TStringList; const AName, AParent, Prefix: String );
begin
    if fNotifyList.Count = 0 then exit;
    if fImageType <> None then
        SL.Add ( Prefix + AName + '.Free;' );
end;

procedure TKOLGRushImageCollection.P_SetupLast( SL: TStringList; const AName, AParent, Prefix: String );
begin

end;

procedure TKOLGRushImageCollection.AssignEvents( SL: TStringList; const AName: String );
begin

end;

function TKOLGRushImageCollection.P_AssignEvents( SL: TStringList; const AName: String;
            CheckOnly: Boolean ): Boolean;
begin
    Result := FALSE;
end;

procedure tinyLoadJPGGIFBMPStream(var TargetBitmap: KOL.PBitMap; Stream: Classes.TStream);
var     Ptr: Pointer;
begin
    DWORD(Ptr) := LocalAlloc(GMEM_FIXED, Stream.Size);
    Stream.Position := 0;
    Stream.Read(Ptr^, Stream.Size);
    tinyLoadJPGGIFBMPMemory(TargetBitmap, DWORD(Ptr), Stream.Size);
end;

function TKOLGRushImageCollection.LoadBitmap: PBitmap;
var     KOLStream: KOL.PStream;
begin
    Result := nil;
    if fImageType <> None then begin
        if fImageType = PNG then begin
            KOLStream := NewExMemoryStream(fDataStream.Memory, fDataStream.Size);
            try
                tinyLoadPNG(Result, KOLStream);
            except
                ShowMessage('Произошла ошибка во время попытки декадировать файл как *.png'
                    + '. Пожалуста сообщите об этом автору (homm86@mail.ru) и прекрепите'
                    + ' проблемный файл если его размер менее мегабайта.');
                try
                    Result.Free;
                finally
                    Result := nil;
                end;
            end;
            KOLStream.Free;
            if Result = nil then begin
                ShowMessage('Компонент должен хранить картинку в формате PNG, но из-за ошибки'
                    + ' компонента она не может быть прочитана.');
            end;
        end else begin
            tinyLoadJPGGIFBMPStream(Result, fDataStream);
            if Result = nil then begin
                ShowMessage('Компонент должен хранить картинку в формате BMP_GIF_JPG, но'
                    + ' из-за ошибки компонента она не может быть прочитана.');
            end;
        end;
    end;
end;

//******************************************************************************
//  GRush Button
//******************************************************************************

constructor TKOLGRushButton.Create(AOwner: TComponent);
begin
    inherited;
    fStyles := TKOLGRushButtonStyles.Create(Self);
    fAutoSzX := 12;
    fAutoSzY := 11;
end;

destructor TKOLGRushButton.Destroy;
begin
    fStyles.Free;
    inherited;
end;

procedure TKOLGRushButton.SetStyles(Val: TKOLGRushButtonStyles);
begin
    fStyles.Assign( Val );
end;

function TKOLGRushButton.Pcode_Generate: Boolean;
begin
    Result := TRUE;
end;

procedure TKOLGRushButton.NotifyLinkedComponent(Sender: TObject;
    Operation: TNotifyOperation);
begin
    inherited;
    if Operation = noRemoved then
        fImageCollection := nil;
end;

procedure TKOLGRushButton.SetImageCollection(const Value: TKOLGRushImageCollection);
begin
    if fImageCollection <> nil then
        fImageCollection.NotifyLinkedComponent( Self, noRemoved );
    fImageCollection := Value;
    if (Value <> nil) and (Value is TKOLGRushImageCollection) then begin
        Value.AddToNotifyList( Self );
    end;
    Change;
end;

function TKOLGRushButton.DefaultParentColor;
begin
    Result := TRUE;
end;
function TKOLGRushButton.CanChangeColor;
begin
    Result := TRUE;
end;
function TKOLGRushButton.CanNotChangeFontColor;
begin
    Result := FALSE;
end;
procedure TKOLGRushButton.SetOnRecalcRects;
begin
    fOnRecalcRects := Value;
    Change;
end;

function TKOLGRushButton.TypeName: String;
begin
    Result := 'GRushButton';
end;

function TKOLGRushButton.AdditionalUnits: String;
begin
    Result := ', KOLGRushControls';
end;

procedure TKOLGRushButton.SetupConstruct(SL: TStringList; const AName, AParent, Prefix: String);
begin
  SL.Add( Format('%s%s  :=  PGRushControl( NewGRushButton(%s)%s );',
                 [ Prefix, AName, SetupParams( AName, AParent ),
                   GenerateTransparentInits ] ) );
end;

procedure TKOLGRushButton.SetupFirst(SL: TStringList; const AName, AParent, Prefix: String);
const   Booleans: array [Boolean] of String = ('FALSE', 'TRUE');
        TGRushSpeeds: array [TGRushSpeed] of String = ('usImmediately', 'usVeryFast', 'usFast', 'usNormal', 'usSlow', 'usVerySlow');
var     CtlStyles: TKOLGrushButtonStyles;
begin
    inherited;
    CtlStyles := TKOLGrushButtonStyles.Create( Self );
    SetUpState4States(Self, fStyles, SL,  AName, Prefix, CtlStyles);
    SetUpCommon(Self, fStyles, SL,  AName, Prefix, CtlStyles, fImageCollection);

    if fStyles.DrawFocusRect <> CtlStyles.DrawFocusRect then
        SL.Add( Prefix + AName + '.All_DrawFocusRect := '+Booleans[fStyles.DrawFocusRect]+';');
    if fStyles.UpdateSpeed <> CtlStyles.UpdateSpeed then
        SL.Add( Prefix + AName + '.All_UpdateSpeed := '+TGRushSpeeds[fStyles.UpdateSpeed]+';');

    CtlStyles.Free;
end;

procedure TKOLGRushButton.P_SetupFirst(SL: TStringList; const AName, AParent, Prefix: String);
const   Booleans: array [Boolean] of String = ('FALSE', 'TRUE');
        TGRushSpeeds: array [TGRushSpeed] of String = ('usImmediately', 'usVeryFast', 'usFast', 'usNormal', 'usSlow', 'usVerySlow');
var     CtlStyles: TKOLGrushButtonStyles;
begin
    inherited;
    CtlStyles := TKOLGrushButtonStyles.Create( Self );
    P_SetUpState4States(Self, fStyles, SL, CtlStyles);
    P_SetUpCommon(Self, fStyles, SL, CtlStyles);

    if fStyles.fDrawFocusRect <> CtlStyles.fDrawFocusRect then
        SL.Add( ' L(0) C1 GR0O_.SetAll_DrawFocusRect<2>' );
    if fStyles.fUpdateSpeed <> CtlStyles.fUpdateSpeed then
        SL.Add( ' L(' + int2str( Byte ( fStyles.fUpdateSpeed ) ) + ') C1 GR0O_.SetAll_UpdateSpeed<2>' );

    CtlStyles.Free;
end;

procedure TKOLGRushButton.AssignEvents(SL: TStringList; const AName: String);
begin
  inherited;
  DoAssignEvents( SL, AName, [ 'OnRecalcRects' ], [ @fOnRecalcRects ] );
end;

function TKOLGRushButton.P_AssignEvents(SL: TStringList; const AName: String;
    CheckOnly: Boolean): Boolean;
begin
    Result := inherited P_AssignEvents( SL, AName, CheckOnly );
    Result := Result or (@OnRecalcRects <> nil);
    if CheckOnly then exit;
    if @OnRecalcRects <> nil then
        SL.Add( ' LoadSELF Load4 ####T' + ParentKOLForm.FormName + '.' +
            ParentForm.MethodName( @ OnRecalcRects ) + #13#10' C2 GR0O_.SetOnRecalcRects<1>');
end;

//******************************************************************************
//  GRush Panel
//******************************************************************************

constructor TKOLGRushPanel.Create(AOwner: TComponent);
begin
    inherited;
    fStyles := TKOLGRushPanelStyles.Create( Self );
end;

destructor TKOLGRushPanel.Destroy;
begin
    fStyles.Free;
    inherited;
end;

procedure TKOLGRushPanel.SetStyles(Val: TKOLGRushPanelStyles);
begin
    fStyles.Assign( Val );
end;

function TKOLGRushPanel.Pcode_Generate: Boolean;
begin
  Result := TRUE;
end;

procedure TKOLGRushPanel.NotifyLinkedComponent(Sender: TObject;
    Operation: TNotifyOperation);
begin
    inherited;
    if Operation = noRemoved then
        fImageCollection := nil;
end;

procedure TKOLGRushPanel.SetOnRecalcRects;
begin
    fOnRecalcRects := Value;
    Change;
end;

procedure TKOLGRushPanel.SetImageCollection(const Value: TKOLGRushImageCollection);
begin
    if fImageCollection <> nil then
        fImageCollection.NotifyLinkedComponent( Self, noRemoved );
    fImageCollection := Value;
    if (Value <> nil) and (Value is TKOLGRushImageCollection) then begin
        Value.AddToNotifyList( Self );
    end;
    Change;
end;

function TKOLGRushPanel.ClientMargins;
begin
    Result := MakeRect(0, 0, 0, 0);
end;

function TKOLGRushPanel.TypeName: String;
begin
  Result := 'GRushPanel';
end;

function TKOLGRushPanel.AdditionalUnits: String;
begin
  Result := ', KOLGRushControls';
end;

function TKOLGRushPanel.SetupParams(const AName, AParent: String): String;
begin
  Result := AParent;
end;

function TKOLGRushPanel.P_SetupParams(const AName, AParent: String; var nparams: Integer): String;
begin
    nparams := 1;
    Result := ' DUP ';
end;

procedure TKOLGRushPanel.SetupConstruct(SL: TStringList; const AName, AParent, Prefix: String);
begin
  SL.Add( Format('%s%s  :=  PGRushControl( NewGRushPanel(%s)%s );',
                 [ Prefix, AName, SetupParams( AName, AParent ),
                   GenerateTransparentInits ] ) );
end;


procedure TKOLGRushPanel.SetupFirst(SL: TStringList; const AName, AParent, Prefix: String);
var     CtlStyles: TKOLGrushPanelStyles;
begin
    inherited;
    CtlStyles := TKOLGrushPanelStyles.Create( Self );
    SetUpState2States(Self, fStyles, SL,  AName, Prefix, CtlStyles);
    SetUpCommon(Self, fStyles, SL, AName, Prefix, CtlStyles, fImageCollection);

    CtlStyles.Free;
end;

procedure TKOLGRushPanel.P_SetupFirst(SL: TStringList; const AName, AParent, Prefix: String);
var     CtlStyles: TKOLGrushPanelStyles;
begin
    inherited;
    CtlStyles := TKOLGrushPanelStyles.Create( Self );
    P_SetUpState2States(Self, fStyles, SL, CtlStyles);
    P_SetUpCommon(Self, fStyles, SL, CtlStyles);

    CtlStyles.Free;
end;

procedure TKOLGRushPanel.AssignEvents(SL: TStringList; const AName: String);
begin
  inherited;
  DoAssignEvents( SL, AName, [ 'OnRecalcRects' ], [ @fOnRecalcRects ] );
end;

function TKOLGRushPanel.P_AssignEvents(SL: TStringList; const AName: String;
    CheckOnly: Boolean): Boolean;
begin
    Result := inherited P_AssignEvents( SL, AName, CheckOnly );
    Result := Result or (@OnRecalcRects <> nil);
    if CheckOnly then exit;
    if @OnRecalcRects <> nil then
        SL.Add( ' LoadSELF Load4 ####T' + ParentKOLForm.FormName + '.' +
            ParentForm.MethodName( @ OnRecalcRects ) + #13#10' C2 GR0O_.SetOnRecalcRects<1>');
end;

//******************************************************************************
//  GRush Check Box
//******************************************************************************

constructor TKOLGRushCheckBox.Create(AOwner: TComponent);
begin
    inherited;
    fStyles := TKOLGRushCheckBoxStyles.Create( Self );
end;

destructor TKOLGRushCheckBox.Destroy;
begin
    fStyles.Free;
    inherited;
end;

procedure TKOLGRushCheckBox.SetStyles(Val: TKOLGRushCheckBoxStyles);
begin
    fStyles.Assign( Val );
end;

function TKOLGRushCheckBox.Pcode_Generate: Boolean;
begin
  Result := TRUE;
end;

{procedure TKOLGRushCheckBox.P_DoProvideFakeType( SL: TStringList );
begin
    P_ProvideFakeType(SL, ' GR0O_ = object( TGRushControl ) end; ');
end; }

procedure TKOLGRushCheckBox.SetOnRecalcRects;
begin
    fOnRecalcRects := Value;
    Change;
end;

procedure TKOLGRushCheckBox.NotifyLinkedComponent(Sender: TObject;
    Operation: TNotifyOperation);
begin
    inherited;
    if Operation = noRemoved then
        fImageCollection := nil;
end;

procedure TKOLGRushCheckBox.SetImageCollection(const Value: TKOLGRushImageCollection);
begin
    if fImageCollection <> nil then
        fImageCollection.NotifyLinkedComponent( Self, noRemoved );
    fImageCollection := Value;
    if (Value <> nil) and (Value is TKOLGRushImageCollection) then begin
        Value.AddToNotifyList( Self );
    end;
    Change;
end;

function TKOLGRushCheckBox.TypeName: String;
begin
  Result := 'GRushCheckBox';
end;

function TKOLGRushCheckBox.AdditionalUnits: String;
begin
  Result := ', KOLGRushControls';
end;

procedure TKOLGRushCheckBox.SetupConstruct(SL: TStringList; const AName, AParent, Prefix: String);
begin
  SL.Add( Format('%s%s  :=  PGRushControl( NewGRushCheckBox(%s)%s );',
                 [ Prefix, AName, SetupParams( AName, AParent ),
                   GenerateTransparentInits ] ) );
end;

procedure TKOLGRushCheckBox.SetupFirst(SL: TStringList; const AName, AParent, Prefix: String);
const   Booleans: array [Boolean] of String = ('FALSE', 'TRUE');
        TGRushSpeeds: array [TGRushSpeed] of String = ('usImmediately', 'usVeryFast', 'usFast', 'usNormal', 'usSlow', 'usVerySlow');
var     CtlStyles: TKOLGrushCheckBoxStyles;
begin
    inherited;
    CtlStyles := TKOLGRushCheckBoxStyles.Create( Self );
    SetUpState4States(Self, fStyles, SL, AName, Prefix, CtlStyles);
    SetUpCommon(Self, fStyles, SL, AName, Prefix, CtlStyles, fImageCollection);

    if fStyles.fCheckMetric <> CtlStyles.fCheckMetric then
        SL.Add( Prefix + AName + '.All_CheckMetric := '+int2str(fStyles.CheckMetric)+';');
    if fStyles.fColorCheck <> CtlStyles.fColorCheck then
        SL.Add( Prefix + AName + '.All_ColorCheck := '+Int2str(fStyles.ColorCheck)+';');
    if fStyles.DrawFocusRect <> CtlStyles.DrawFocusRect then
        SL.Add( Prefix + AName + '.All_DrawFocusRect := '+Booleans[fStyles.DrawFocusRect]+';');
    if fStyles.UpdateSpeed <> CtlStyles.UpdateSpeed then
        SL.Add( Prefix + AName + '.All_UpdateSpeed := '+TGRushSpeeds[fStyles.UpdateSpeed]+';');

    CtlStyles.Free;
end;

procedure TKOLGRushCheckBox.P_SetupFirst(SL: TStringList; const AName, AParent, Prefix: String);
const   Booleans: array [Boolean] of String = ('FALSE', 'TRUE');
        TGRushSpeeds: array [TGRushSpeed] of String = ('usImmediately', 'usVeryFast', 'usFast', 'usNormal', 'usSlow', 'usVerySlow');
var     CtlStyles: TKOLGrushRadioBoxStyles;
begin
    inherited;
    CtlStyles := TKOLGrushRadioBoxStyles.Create( Self );
    P_SetUpState4States(Self, fStyles, SL, CtlStyles);
    P_SetUpCommon(Self, fStyles, SL, CtlStyles);
    if fStyles.fCheckMetric <> CtlStyles.fCheckMetric then
        SL.Add( ' L(' + int2str( fStyles.fCheckMetric ) + ') C1 GR0O_.SetAll_CheckMetric<2>' );
    if fStyles.fColorCheck <> CtlStyles.fColorCheck then
        SL.Add( ' L(' + int2str( fStyles.fColorCheck ) + ') C1 GR0O_.SetAll_ColorCheck<2>' );
    if fStyles.fDrawFocusRect <> CtlStyles.fDrawFocusRect then
        SL.Add( ' L(0) C1 GR0O_.SetAll_DrawFocusRect<2>' );
    if fStyles.fUpdateSpeed <> CtlStyles.fUpdateSpeed then
        SL.Add( ' L(' + int2str( Byte ( fStyles.fUpdateSpeed ) ) + ') C1 GR0O_.SetAll_UpdateSpeed<2>' );

    CtlStyles.Free;
end;

procedure TKOLGRushCheckBox.AssignEvents(SL: TStringList; const AName: String);
begin
  inherited;
  DoAssignEvents( SL, AName, [ 'OnRecalcRects' ], [ @fOnRecalcRects ] );
end;

function TKOLGRushCheckBox.P_AssignEvents(SL: TStringList; const AName: String;
    CheckOnly: Boolean): Boolean;
begin
    Result := inherited P_AssignEvents( SL, AName, CheckOnly );
    Result := Result or (@OnRecalcRects <> nil);
    if CheckOnly then exit;
    if @OnRecalcRects <> nil then
        SL.Add( ' LoadSELF Load4 ####T' + ParentKOLForm.FormName + '.' +
            ParentForm.MethodName( @ OnRecalcRects ) + #13#10' C2 GR0O_.SetOnRecalcRects<1>');
end;

//******************************************************************************
//  GRush Radio Box
//******************************************************************************

constructor TKOLGRushRadioBox.Create(AOwner: TComponent);
begin
    inherited;
    fStyles := TKOLGRushRadioBoxStyles.Create( Self );
end;

destructor TKOLGRushRadioBox.Destroy;
begin
    fStyles.Free;
    inherited;
end;

procedure TKOLGRushRadioBox.SetStyles(Val: TKOLGRushRadioBoxStyles);
begin
    fStyles.Assign( Val );
end;

function TKOLGRushRadioBox.Pcode_Generate: Boolean;
begin
  Result := TRUE;
end;

{procedure TKOLGRushRadioBox.P_DoProvideFakeType( SL: TStringList );
begin
    P_ProvideFakeType(SL, ' GR0O_ = object( TGRushControl ) end; ');
end; }

procedure TKOLGRushRadioBox.SetOnRecalcRects;
begin
    fOnRecalcRects := Value;
    Change;
end;

procedure TKOLGRushRadioBox.NotifyLinkedComponent(Sender: TObject;
    Operation: TNotifyOperation);
begin
    inherited;
    if Operation = noRemoved then
        fImageCollection := nil;
end;

procedure TKOLGRushRadioBox.SetImageCollection(const Value: TKOLGRushImageCollection);
begin
    if fImageCollection <> nil then
        fImageCollection.NotifyLinkedComponent( Self, noRemoved );
    fImageCollection := Value;
    if (Value <> nil) and (Value is TKOLGRushImageCollection) then begin
        Value.AddToNotifyList( Self );
    end;
    Change;
end;

function TKOLGRushRadioBox.TypeName: String;
begin
  Result := 'GRushRadioBox';
end;

function TKOLGRushRadioBox.AdditionalUnits: String;
begin
  Result := ', KOLGRushControls';
end;

procedure TKOLGRushRadioBox.SetupConstruct(SL: TStringList; const AName, AParent, Prefix: String);
begin
  SL.Add( Format('%s%s  :=  PGRushControl( NewGRushRadioBox(%s)%s );',
                 [ Prefix, AName, SetupParams( AName, AParent ),
                   GenerateTransparentInits ] ) );
end;

procedure TKOLGRushRadioBox.SetupFirst(SL: TStringList; const AName, AParent, Prefix: String);
const   Booleans: array [Boolean] of String = ('FALSE', 'TRUE');
        TGRushSpeeds: array [TGRushSpeed] of String = ('usImmediately', 'usVeryFast', 'usFast', 'usNormal', 'usSlow', 'usVerySlow');
var     CtlStyles: TKOLGrushRadioBoxStyles;
begin
    inherited;
    CtlStyles := TKOLGrushRadioBoxStyles.Create( Self );
    SetUpState4States(Self, fStyles, SL, AName, Prefix, CtlStyles);
    SetUpCommon(Self, fStyles, SL, AName, Prefix, CtlStyles, fImageCollection);

    if fStyles.fCheckMetric <> CtlStyles.fCheckMetric then
        SL.Add( Prefix + AName + '.All_CheckMetric := '+int2str(fStyles.CheckMetric)+';');
    if fStyles.fColorCheck <> CtlStyles.fColorCheck then
        SL.Add( Prefix + AName + '.All_ColorCheck := '+Int2str(fStyles.ColorCheck)+';');
    if fStyles.fDrawFocusRect <> CtlStyles.fDrawFocusRect then
        SL.Add( Prefix + AName + '.All_DrawFocusRect := '+Booleans[fStyles.DrawFocusRect]+';');
    if fStyles.fUpdateSpeed <> CtlStyles.fUpdateSpeed then
        SL.Add( Prefix + AName + '.All_UpdateSpeed := '+TGRushSpeeds[fStyles.UpdateSpeed]+';');
        
    CtlStyles.Free;
end;

procedure TKOLGRushRadioBox.P_SetupFirst(SL: TStringList; const AName, AParent, Prefix: String);
const   Booleans: array [Boolean] of String = ('FALSE', 'TRUE');
        TGRushSpeeds: array [TGRushSpeed] of String = ('usImmediately', 'usVeryFast', 'usFast', 'usNormal', 'usSlow', 'usVerySlow');
var     CtlStyles: TKOLGrushRadioBoxStyles;
begin
    inherited;
    CtlStyles := TKOLGrushRadioBoxStyles.Create( Self );
    P_SetUpState4States(Self, fStyles, SL, CtlStyles);
    P_SetUpCommon(Self, fStyles, SL, CtlStyles);
    if fStyles.fCheckMetric <> CtlStyles.fCheckMetric then
        SL.Add( ' L(' + int2str( fStyles.fCheckMetric ) + ') C1 GR0O_.SetAll_CheckMetric<2>' );
    if fStyles.fColorCheck <> CtlStyles.fColorCheck then
        SL.Add( ' L(' + int2str( fStyles.fColorCheck ) + ') C1 GR0O_.SetAll_ColorCheck<2>' );
    if fStyles.fDrawFocusRect <> CtlStyles.fDrawFocusRect then
        SL.Add( ' L(0) C1 GR0O_.SetAll_DrawFocusRect<2>' );
    if fStyles.fUpdateSpeed <> CtlStyles.fUpdateSpeed then
        SL.Add( ' L(' + int2str( Byte( fStyles.fUpdateSpeed ) ) + ') C1 GR0O__.SetAll_UpdateSpeed<2>' );

    CtlStyles.Free;
end;

procedure TKOLGRushRadioBox.AssignEvents(SL: TStringList; const AName: String);
begin
  inherited;
  DoAssignEvents( SL, AName, [ 'OnRecalcRects' ], [ @fOnRecalcRects ] );
end;

function TKOLGRushRadioBox.P_AssignEvents(SL: TStringList; const AName: String;
    CheckOnly: Boolean): Boolean;
begin
    Result := inherited P_AssignEvents( SL, AName, CheckOnly );
    Result := Result or (@OnRecalcRects <> nil);
    if CheckOnly then exit;
    if @OnRecalcRects <> nil then
        SL.Add( ' LoadSELF Load4 ####T' + ParentKOLForm.FormName + '.' +
            ParentForm.MethodName( @ OnRecalcRects ) + #13#10' C2 GR0O_.SetOnRecalcRects<1>');
end;

//******************************************************************************
//  GRush Splitter
//******************************************************************************

constructor TKOLGRushSplitter.Create(AOwner: TComponent);
begin
    fStyles := TKOLGRushSplitterStyles.Create( Self );
    inherited;
end;

destructor TKOLGRushSplitter.Destroy;
begin
    fStyles.Free;
    inherited;
end;

procedure TKOLGRushSplitter.SetStyles(Val: TKOLGRushSplitterStyles);
begin
    fStyles.Assign( Val );
end;

function TKOLGRushSplitter.Pcode_Generate: Boolean;
begin
    Result := TRUE;
end;

{procedure TKOLGRushSplitter.P_DoProvideFakeType( SL: TStringList );
begin
    P_ProvideFakeType(SL, ' GR0O_ = object( TGRushControl ) end; ');
end; }

procedure TKOLGRushSplitter.NotifyLinkedComponent(Sender: TObject;
    Operation: TNotifyOperation);
begin
    inherited;
    if Operation = noRemoved then
        fImageCollection := nil;
end;

procedure TKOLGRushSplitter.SetCaption;
begin
    if fCaption = Value then begin
        LogOK;
        Exit;
    end;
    if action = nil then
        fCaption := Value
    else
        fCaption := action.Caption;
    {$IFDEF _KOLCtrlWrapper_}
    if Assigned(FKOLCtrl) then
        FKOLCtrl.Caption := fCaption;
    {$ENDIF}
    Invalidate;
    Change;
end;

procedure TKOLGRushSplitter.SetOnRecalcRects;
begin
    fOnRecalcRects := Value;
    Change;
end;

procedure TKOLGRushSplitter.SetImageCollection(const Value: TKOLGRushImageCollection);
begin
    if fImageCollection <> nil then
        fImageCollection.NotifyLinkedComponent( Self, noRemoved );
    fImageCollection := Value;
    if (Value <> nil) and (Value is TKOLGRushImageCollection) then begin
        Value.AddToNotifyList( Self );
    end;
    Change;
end;

function TKOLGRushSplitter.TypeName: String;
begin
  Result := 'GRushSplitter';
end;

function TKOLGRushSplitter.AdditionalUnits: String;
begin
  Result := ', KOLGRushControls';
end;

procedure TKOLGRushSplitter.SetupConstruct(SL: TStringList; const AName, AParent, Prefix: String);
begin
  SL.Add( Format('%s%s  :=  PGRushControl( NewGRushSplitter(%s)%s );',
                 [ Prefix, AName, SetupParams( AName, AParent ),
                   GenerateTransparentInits ] ) );
end;

function TKOLGRushSplitter.SetupParams(const AName, AParent: String): String;
begin
  Result := Format('%s, %d, %d', [AParent, MinSizePrev, MinSizeNext]);
end;

function TKOLGRushSplitter.P_SetupParams(const AName, AParent: String; var nparams: Integer): String;
begin
    nparams := 3;
    Result := ' L( ' + IntToStr( MinSizeNext ) + ')' +
    #13#10' L( ' + IntToStr( MinSizePrev ) + ') ' +
    #13#10' C2';
end;

procedure TKOLGRushSplitter.SetupFirst(SL: TStringList; const AName, AParent, Prefix: String);
const   TGRushSpeeds: array [TGRushSpeed] of String = ('usImmediately', 'usVeryFast', 'usFast', 'usNormal', 'usSlow', 'usVerySlow');
var     CtlStyles: TKOLGrushSplitterStyles;
begin
    inherited;
    CtlStyles := TKOLGrushSplitterStyles.Create( Self );
    CtlStyles.SetUpSplitterAlign(Align in [mirror.caLeft, mirror.caRight]);
    SetUpState4States(Self, fStyles, SL, AName, Prefix, CtlStyles);
    SetUpCommon(Self, fStyles, SL, AName, Prefix, CtlStyles, fImageCollection);

    if fStyles.UpdateSpeed <> CtlStyles.UpdateSpeed then
        SL.Add( Prefix + AName + '.All_UpdateSpeed := '+TGRushSpeeds[fStyles.UpdateSpeed]+';');
    if fStyles.SplitterDotsCount <> CtlStyles.SplitterDotsCount then
        SL.Add( Prefix + AName + '.All_SplitterDotsCount := '+int2str(fStyles.SplitterDotsCount)+';');

    CtlStyles.Free;
end;

procedure TKOLGRushSplitter.P_SetupFirst(SL: TStringList; const AName, AParent, Prefix: String);
const   TGRushSpeeds: array [TGRushSpeed] of String = ('usImmediately', 'usVeryFast', 'usFast', 'usNormal', 'usSlow', 'usVerySlow');
var     CtlStyles: TKOLGrushSplitterStyles;
begin
    inherited;
    CtlStyles := TKOLGrushSplitterStyles.Create( Self );
    CtlStyles.SetUpSplitterAlign(Align in [mirror.caLeft, mirror.caRight]);
    P_SetUpState4States(Self, fStyles, SL, CtlStyles);
    P_SetUpCommon(Self, fStyles, SL, CtlStyles);

    if fStyles.fUpdateSpeed <> CtlStyles.fUpdateSpeed then
        SL.Add( ' L(' + int2str( Byte( fStyles.fUpdateSpeed ) ) + ') C1 GR0O_.SetAll_UpdateSpeed<2>' );
    if fStyles.SplitterDotsCount <> CtlStyles.SplitterDotsCount then
        SL.Add( ' L(' + int2str(fStyles.SplitterDotsCount) + ') C1 GR0O_.SetAll_SplitterDotsCount<2>' );

    CtlStyles.Free;
end;

procedure TKOLGRushSplitter.Change;
begin
    Inherited;
    if fLastAlign <> Align then begin
        fLastAlign := Align;
        fStyles.SetUpSplitterAlign(Align in [mirror.caLeft, mirror.caRight]);
    end;
end;

procedure TKOLGRushSplitter.AssignEvents(SL: TStringList; const AName: String);
begin
    inherited;
    DoAssignEvents( SL, AName, [ 'OnRecalcRects' ], [ @fOnRecalcRects ] );
end;

function TKOLGRushSplitter.P_AssignEvents(SL: TStringList; const AName: String;
    CheckOnly: Boolean): Boolean;
begin
    Result := inherited P_AssignEvents( SL, AName, CheckOnly );
    Result := Result or (@OnRecalcRects <> nil);
    if CheckOnly then exit;
    if @OnRecalcRects <> nil then
        SL.Add( ' LoadSELF Load4 ####T' + ParentKOLForm.FormName + '.' +
            ParentForm.MethodName( @ OnRecalcRects ) + #13#10' C2 GR0O_.SetOnRecalcRects<1>');
end;

//******************************************************************************
//  GRush Progress Bar
//******************************************************************************

constructor TKOLGRushProgressBar.Create(AOwner: TComponent);
begin
    inherited;
    fStyles := TKOLGRushProgressBarStyles.Create( Self );
end;

destructor TKOLGRushProgressBar.Destroy;
begin
    fStyles.Free;
    inherited;
end;

procedure TKOLGRushProgressBar.SetStyles(Val: TKOLGRushProgressBarStyles);
begin
    fStyles.Assign( Val );
end;

function TKOLGRushProgressBar.Pcode_Generate: Boolean;
begin
  Result := TRUE;
end;

{procedure TKOLGRushProgressBar.P_DoProvideFakeType( SL: TStringList );
begin
    P_ProvideFakeType(SL, ' GR0O_ = object( TGRushControl ) end; ');
end; }


procedure TKOLGRushProgressBar.SetCaption;
begin
    if fCaption = Value then begin
        LogOK;
        Exit;
    end;
    if action = nil then
        fCaption := Value
    else
        fCaption := action.Caption;
    {$IFDEF _KOLCtrlWrapper_}
    if Assigned(FKOLCtrl) then
        FKOLCtrl.Caption := fCaption;
    {$ENDIF}
    Invalidate;
    Change;
end;

procedure TKOLGRushProgressBar.SetOnRecalcRects;
begin
    fOnRecalcRects := Value;
    Change;
end;

procedure TKOLGRushProgressBar.NotifyLinkedComponent(Sender: TObject;
    Operation: TNotifyOperation);
begin
    inherited;
    if Operation = noRemoved then
        fImageCollection := nil;
end;

procedure TKOLGRushProgressBar.SetImageCollection(const Value: TKOLGRushImageCollection);
begin
    if fImageCollection <> nil then
        fImageCollection.NotifyLinkedComponent( Self, noRemoved );
    fImageCollection := Value;
    if (Value <> nil) and (Value is TKOLGRushImageCollection) then begin
        Value.AddToNotifyList( Self );
    end;
    Change;
end;

procedure TKOLGRushProgressBar.SetOnProgressChange;
begin
    fOnProgressChange := Value;
    Change;
end;

function TKOLGRushProgressBar.TypeName: String;
begin
  Result := 'GRushProgressBar';
end;

function TKOLGRushProgressBar.AdditionalUnits: String;
begin
  Result := ', KOLGRushControls';
end;

function TKOLGRushProgressBar.SetupParams(const AName, AParent: String): String;
begin
  Result := AParent;
end;

function TKOLGRushProgressBar.P_SetupParams(const AName, AParent: String; var nparams: Integer): String;
begin
    nparams := 1;
    Result := ' DUP ';
end;

procedure TKOLGRushProgressBar.SetupConstruct(SL: TStringList; const AName, AParent, Prefix: String);
begin
  SL.Add( Format('%s%s  :=  PGRushControl( NewGRushProgressBar(%s)%s );',
                 [ Prefix, AName, SetupParams( AName, AParent ),
                   GenerateTransparentInits ] ) );
end;


procedure TKOLGRushProgressBar.SetupFirst(SL: TStringList; const AName, AParent, Prefix: String);
const   Booleans: array [Boolean] of String = ('FALSE', 'TRUE');
        TGRushSpeeds: array [TGRushSpeed] of String = ('usImmediately', 'usVeryFast', 'usFast', 'usNormal', 'usSlow', 'usVerySlow');
var     CtlStyles: TKOLGrushProgressBarStyles;
begin
    inherited;
    CtlStyles := TKOLGrushProgressBarStyles.Create( Self );
    if fCaption <> '' then
        SL.Add( Prefix + AName + '.Caption := '+StringConstant('Caption', fCaption)+';');
    if fStyles.ProgressVertical <> CtlStyles.ProgressVertical then
        SL.Add( Prefix + AName + '.All_ProgressVertical := '+Booleans[fStyles.ProgressVertical]+';');
    CtlStyles.SetUpProgressVertical(fStyles.ProgressVertical);
    SetUpState2States(Self, fStyles, SL, AName, Prefix, CtlStyles);
    SetUpCommon(Self, fStyles, SL, AName, Prefix, CtlStyles, fImageCollection);

    if fStyles.DrawProgress <> CtlStyles.DrawProgress then
        SL.Add( Prefix + AName + '.All_DrawProgress := '+Booleans[fStyles.DrawProgress]+';');
    if fStyles.DrawProgressRect <> CtlStyles.DrawProgressRect then
        SL.Add( Prefix + AName + '.All_DrawProgressRect := '+Booleans[fStyles.DrawProgressRect]+';');

    CtlStyles.Free;
end;

procedure TKOLGRushProgressBar.P_SetupFirst(SL: TStringList; const AName, AParent, Prefix: String);
const   Booleans: array [Boolean] of String = ('FALSE', 'TRUE');
        TGRushSpeeds: array [TGRushSpeed] of String = ('usImmediately', 'usVeryFast', 'usFast', 'usNormal', 'usSlow', 'usVerySlow');
var     CtlStyles: TKOLGrushProgressBarStyles;
begin
    inherited;
    CtlStyles := TKOLGrushProgressBarStyles.Create( Self );
    if fCaption <> '' then begin
        SL.Add( P_StringConstant('Caption', Caption) );
        SL.Add( ' C2 TControl_.SetCaption<2> DelAnsiStr' );
    end;
    if fStyles.fProgressVertical <> CtlStyles.fProgressVertical then
        SL.Add( ' L(1) C1 GR0O_.SetAll_ProgressVertical<2>' );

    CtlStyles.SetUpProgressVertical(fStyles.ProgressVertical);
    P_SetUpState2States(Self, fStyles, SL, CtlStyles);
    P_SetUpCommon(Self, fStyles, SL, CtlStyles);

    if fStyles.fDrawProgress <> CtlStyles.fDrawProgress then
        SL.Add( ' L(0) C1 GR0O_.SetAll_DrawProgress<2>' );
    if fStyles.fDrawProgressRect <> CtlStyles.fDrawProgressRect then
        SL.Add( ' L(0) C1 GR0O_.SetAll_DrawProgressRect<2>' );

    CtlStyles.Free;
end;

procedure TKOLGRushProgressBar.AssignEvents(SL: TStringList; const AName: String);
begin
  inherited;
  DoAssignEvents( SL, AName, [ 'OnProgressChange', 'OnRecalcRects' ], [ @OnProgressChange, @OnRecalcRects ] );
end;

function TKOLGRushProgressBar.P_AssignEvents(SL: TStringList; const AName: String;
    CheckOnly: Boolean): Boolean;
begin
    Result := inherited P_AssignEvents( SL, AName, CheckOnly );
    Result := Result or (@OnRecalcRects <> nil) or (@OnProgressChange <> nil);
    if CheckOnly then exit;
    if @OnRecalcRects <> nil then
        SL.Add( ' LoadSELF Load4 ####T' + ParentKOLForm.FormName + '.' +
            ParentForm.MethodName( @ OnRecalcRects ) + #13#10' C2 GR0O_.SetOnRecalcRects<1>');
    if @OnProgressChange <> nil then
        SL.Add( ' LoadSELF Load4 ####T' + ParentKOLForm.FormName + '.' +
            ParentForm.MethodName( @ OnProgressChange ) + #13#10' C2 GR0O_.SetOnProgressChange<1>');
end;

end.         