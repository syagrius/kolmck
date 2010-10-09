{$I KOLDEF.inc}
unit KOLQProgBar;
{

  ("`-''-/").___..--''"`-._
 `6_ 6  )   `-.  (     ).`-.__.`)
 (_Y_.)'  ._   )  `._ `. ``-..-'
  _..`--'_..-_/  /--'_.' ,'
(il).-''  (li).'  ((!.-'

 QnnO Progress Bar (KOL)
 The component that provides a set of various progress bars.

 Ported to KOL © 2007 Danger
 E-Mail: <danger@artline.kz> <fateyev@inbox.ru>

 Original excellent TQProgressBar VCL component was developed by QnnO
 and was ported to KOL with his permission. Merci a Qnno!
 Thanks to 'MTsv DN' for his 'standard progress bar' compatibility idea.

}

 { ****************************************************************** }
 { v 1.1                                                              }
 {           Delphi (6) unit  --   progressbar replacement, with      }
 {                                 several features...                }
 {                                                                    }
 {           Copyright © 2004 by Olivier Touzot "QnnO"                }
 {    (http://mapage.noos.fr/qnno/delphi_en.htm  -  qnno@noos.fr)     }
 {                                                                    }
 {              ----------------------------------                    }
 {                                                                    }
 { History :                                                          }
 { v 1.1 : 2004-05-12 (!)  Correction of the "extreme colors" bug in  }
 {         the GetGradientAr2(); function by Bernd Kirchhoff, allowing}
 {         the use of pure white or black colors in the bars. Thanks  }
 {         and congratulations (he made the work under cbuilder 4.0 !)}
 { v 1.0 : 2004-05-11 First release ;                                 }
 { ****************************************************************** }

    //  This unit is freeware, but under copyrights that remain mine for my
    //  parts of the code, and original writters for their parts of the code.
    //  This is mainly the case with :
    //  -> The polynomial expression of the MakeCylinder(); function, provided
    //     by Matthieu Contensou, (with lots of help too, on many other
    //     subjects (see below)).
    //     (http://www25.brinkster.com/waypointfrance/cpulog/index.asp)
    //  -> The RGBtoHLS(); and HLStoRGB(); procedures, that come from a
    //     Microsoft knowledge base article (Q29240), at :
    //     http://support.microsoft.com/default.aspx?scid=kb;en-us;29240
    //  -> The GetColorBetween(); function, which computes the main gradient,
    //     found at efg's colors page, and which author is saddly unknown :
    //     http://homepages.borland.com/efg2lab/Library/Delphi/Graphics/Color.htm
    //     http://homepages.borland.com/efg2lab/Library/UseNet/2001/0821.txt
    //  -> The GetGradientAr2(); new version, by Bernd Kirchhoff, which now
    //     correctly handles white and black colors in bars.
    //     (http://home.germany.net/100-445474/)

    //  This unit can be freely used in any application, freeware, shareware
    //  or commercial. However, I would apreciate your sending me an email if
    //  you decide to use it. Of course, you use it under your own and single
    //  responsability. Neither me, nor contributors, could be held responsible
    //  for any problem resulting from the use of this unit.  ;-)

    //  It can also be freely distributed, provided all the above (and current)
    //  lines remain within it unchanged, and the readme.txt file be distributed
    //  with it too.

    //  Many thanks go to Matthieu Contensou, who spent a lot of time (and
    //  patience ... ) trying to explain me the subtleties of the RGB -> YUV
    //  and return conversions.)
    //  He gave the idea of using the HLS space too, which is now used in this
    //  component.

  {* TKOLQProgressBar is the visual component that provides a set of various progress bars.
  Adapted for KOL library, this was designed with maximal usability in mind and looks nice.
  Original excellent TQProgressBar VCL component was developed by QnnO and several contributors,
  and was ported to KOL with his permission. Merci a Qnno!
  |<pre>
  |Copyright (C) 2004 Olivier Touzot "QnnO" and TQProgressBar contributors.
  |It can be found on the web at <a href="http://mapage.noos.fr/qnno/delphi_en.htm">http://mapage.noos.fr/qnno/delphi_en.htm</a>.
  |Copyright (C) 2007 Danger (<a href="mailto:danger@artline.kz">danger@artline.kz</a>).
  |</pre>
  |TKOLQProgressBar coming under the form of a KOL library unit, it can be simply used
  by creating bars at runtime, setting the necessary properties:
  !uses Windows, Messages, KOL, ..., KOLQProgBar;
  ! //...
  !var aPBar : PQProgressBar;
  ! //...
  !aPBar := NewQProgressBar( AParentForm );
  !aPBar.Progress:= 55;
  !aPBar. ...
  |<p>Certainly you can use the 'MCK mirror' provided with component to manage control properties at design time
  (this still actually for Delphi versions earlier than Delphi 2005). In this case the visual component will
   draws itself in design time with one of two available painting methods (see Readme.txt for details).
   Note that control appearance at design time isn't depends on any of KOLCtrlWrapper routines and uses native VCL stuff.
   |</p><p><b>Known problem:</b><br>
   It's latency in the drawing of the first of a series of bars. The laging one is the first one updated, if
   |<i>ShowInactivePos</i> is set to <i>True</i>, and whatever are it's other characteristics (size, appearence, aso).
   The problem appears only under XP (despite a high cpu speed). A workaround is to call
   |<i>Form.ProcessMessages</i> just after the change of the position value of the first bar.</p><p>
   In the demo, the four vertical bars illustrate this. They should slide all together, but the first one lags, unless
   |I add the <i>Form.ProcessMessages</i> like this:
   !procedure TForm1.TrackBar2Scroll( Sender: PTrackbar; Code: Integer );
   !begin
   !  Form.ProcessMessages;                        // Avoids the lag.
   !  QProgressBar7.Progress:= Sender.Position;
   !  QProgressBar8.Progress:= Sender.Position;
   !  QProgressBar9.Progress:= Sender.Position;
   !  QProgressBar10.Progress:= Sender.Position;
   !end;
   |<p> }

interface

// ----------------------------------------------------------
uses
  Windows, Messages, KOL;

// ----------------------------------------------------------
type
  TQBarKind = ( bkFlat, bkCylinder );
  {* Progress bar style. }

  TQBarLook = ( blMetal, blGlass );
  {* Progress bar appearance. }

  TQBarOrientation = ( boHorizontal, boVertical );
  {* Visual control orientation. }

  TRGBArray = array[0..2] of Byte;
  TCLRArray = array of TColor;
  THLSRange = 0..240;

  THLSRec   = record                            // Color conversion -> RgbToHls and return
    hue: THLSRange;
    lum: THLSRange;
    sat: THLSRange;
  end;

  TPosDescr = record                            // Bar description, rows or column ...
    isInBlock: Boolean;                         // ... depending on orientation
    blkLimit : Integer;
  end;

// ----------------------------------------------------------
  PQProgressBar = ^TQProgressBar;
  TKOLQProgressBar = PQProgressBar;

  TOnQProgressBar = procedure( Sender: PQProgressBar ) of object;
  {* |Event to be called when <i>Progress</i> value is changed. }
  
  PQDataObj = ^TQDataObj;

// ----------------------------------------------------------
  TQDataObj = object( TObj )
    fPosDescr      : array of TPosDescr;        // Bar description, blocks and spaces
    fPixDescr      : array of TCLRArray;        // Bar description, pixels colors
    fInactDescr    : TCLRArray;                 // Bar description, inactive positions colors (if reversed gradient);
    fBarKind       : TQBarKind;                 // flat or rounded
    fBarLook       : TQBarLook;                 // blMetal or blGlass
    fOrientation   : TQBarOrientation;          // horizontal or vertical
    fInternalBorder,                            // space between the shape and the bar itself (1 or two pixels)
    fUSefullDrawSpace,                          // size of the bar minus border
    fBorderSize    : Integer;                   // 2*(border+shape)
    fHasShape      : Boolean;                   // the surrounding line
    fShapeClr      : TColor;                    // above' color
    fCorner        : Integer;                   // shape' corner
    fStartClr,                                  // left (or bottom) color
    fFinalClr,                                  // right (or top) color
    fBkgClr        : TColor;                    // background color.
    fMonoClr       : Boolean;                   // True if StartColor = FinalColor.
    fInvInactPos,                               // If true, and gradient, -> inverted;
    fShowInactPos  : Boolean;                   // Bars corresp. to positions above actual are drawn in fInactPosClr
    fInactPosClr   : TColor;                    // Above's color
    fUSerPosPct    : Real;                      // same as below, as percent, for displays
    fUserPos,                                   // value sent by user
    fPosition,                                  // above, normalized to width or height, and max;
    fMinVisPos,                                 // Minimum position to send to Paint(), to see at least one bar
    fMaxPos        : Integer;                   // max position as sent by user.
    fByBlock,                                   // if true, alternates colored and not colored pixels
    fFullBlock     : Boolean;                   // if true, blocks are drawn only when their max position is reached;
    fSpaceSize,                                 // space between two blocks
    fBlockSize     : Integer;                   // width (or height) of a block
    fHideOnTerm    : Boolean;                   // Hides the bar a tenth of a second after the painting of the last pixel row/column;
    fCapAlign      : TTextAlign;                // left - right - centered
    fCapPos        : TPoint;                    // Internal - caption's top and left, based on canvas' current font
    fHasCaption    : Boolean;                   // Internal
    fShowPosAsPct  : Boolean;                   // If True, Hint and/or caption will show the value as a percent of the maximum.
    fCaptionOvr    : Boolean;                   // id. below;
    fHintOvr       : Boolean;                   // if True, each position changes => Hint <- fUserPos or fUSerPosPct dep. on ShowPosAsPct True/false;
    fOnProgChange  : TOnQProgressBar;           // ProgressBar changing event
    destructor Destroy; virtual;
  end;

// ----------------------------------------------------------
  TQProgressBar = object( TControl )
  {* This object implements all functionality of component.
  
  |TKOLQProgressBar is similar to a standard progress bar control and tries to emulate many of its features:
  |<ul><li><p>Has the same properties. Obviously you can use <i>Progress</i>, <i>MaxProgress</i> and <i>Caption</i> derived from
  PControl with some specific caused by the component. Here its short description:
  |<p><ul><li><p><font face="Courier" color="#808000"><b>Progress</b></font> is the position to be drawn on the bar. This should be the only thing changing, once setup is complete;</p></li>
  |<li><p><font face="Courier" color="#808000"><b>MaxProgress</b></font> is the maximum value you may send to the bar. It will be used to normalize positions sent compared to
  |the size of the bar's drawspace;</p></li>
  |<li><p><font face="Courier" color="#808000"><b>Caption</b></font> - the control may display a basic caption. This caption's appearance depends on the bar canvas' font property.
  It is neither XOR'ed nor anything like that: authors couldn't succeed at it. Moreover, despite a caption appears correctly within
  horizontal bars, it certainly will give poor results within vertical bars as long as the caption stays horizontal.
  |</p></li></ul></p></p></li>
  |<li><p>Can handle progress bar control's specific messages. You can send messages to control or receive from it (see the MSDN documentation for details) thus it behaves as an usual progress bar control:
  |<p><ul><li><p><font face="Courier" color="#808000"><b>PBM_GETPOS</b></font> retrieves the current position of the progress bar;</p></li>
  |<li><p><font face="Courier" color="#808000"><b>PBM_SETPOS</b></font> sets the current position for a progress bar and redraws the bar to reflect the new position;</p></li>
  |<li><p><font face="Courier" color="#808000"><b>PBM_GETRANGE</b></font> retrieves information about the current high and low limits of a given progress bar control;</p></li>
  |<li><p><font face="Courier" color="#808000"><b>PBM_SETRANGE</b></font> sets the maximum value for a progress bar and redraws the bar to reflect the new range;</p></li>
  |<li><p><font face="Courier" color="#808000"><b>PBM_SETRANGE32</b></font> Sets the range of a progress bar control to a 32-bit value.</p></li></ul></p></p></li></ul>

  |<br>Use <i>NewQProgressBar</i> constuction function for creation of object instance. Here is the prototype:
  ! function NewQProgressBar( AParent: PControl ): PQProgressBar; }

  protected

    procedure Paint;
    procedure Resize;
    procedure SetUsefullWidth;
    procedure InitBlockArray;
    procedure InitPixArray;
    function  MakeCylinder( h: Real ): Extended;
    function  GetGradientAr2( aColor: TColor; sz: Integer ): TClrArray;
    function  HLStoRGB( hue, lum, sat: THLSRange ): TColor;
    function  RGBtoHLS( RGBColor: TColor): THLSRec;
    function  GetColorBetween( AStartColor, AEndColor: TColor; PointValue, Von, Bis : Extended ): TColor;
    function  GetOrientation: TQBarOrientation;
    procedure SetOrientation( Value: TQBarOrientation );
    function  GetBarKind: TQBarKind;
    procedure SetBarKind    ( Value: TQBarKind );
    function  GetBarLook: TQBarLook;
    procedure SetBarLook    ( Value: TQBarLook );
    procedure SetFCorner    ( IsRounded: Boolean );
    function  GetBoolCorner: Boolean;
    function  GetBkgColor: TColor;
    procedure SetBkgColor   ( aColor: TColor );
    function  GetShape: Boolean;
    procedure SetShape      ( Value: Boolean );
    function  GetShapeColor: TColor;
    procedure SetShapeColor ( Value: TColor );
    function  GetBlockSize: Integer;
    procedure SetBlockSize  ( Value: Integer );
    function  GetSpaceSize: Integer;
    procedure SetSpaceSize  ( Value: Integer );
    function  GetFullBlock: Boolean;
    procedure SetFullBlock  ( Value: Boolean );
    function  GetMaxPos: Integer;
    procedure SetMaxPos     ( Value: Integer );
    function  GetHideOnTerm: Boolean;
    procedure SetHideOnTerm ( Value: Boolean);
    function  GetPosition: Integer;
    procedure SetPosition   ( Value: Integer );
    function  GetStartClr: TColor;
    procedure SetStartClr   ( Value: TColor );
    function  GetFinalClr: TColor;
    procedure SetFinalClr   ( Value: TColor );
    procedure SetBothColors ( Value: TColor );
    function  GetInactivePos: Boolean;
    procedure SetInactivePos( Value: Boolean );
    function  GetInactPosClr: TColor;
    procedure SetInactPosClr( Value: TColor );
    function  GetInvInactPos: Boolean;
    procedure SetInvInactPos( Value: Boolean );
    procedure SetCaption   ( Value: string );
    function  GetCapAlign:  TTextAlign;
    procedure SetCapAlign   ( Value: TTextAlign );
    function  GetCaptionOvr: Boolean;
    procedure SetCaptionOvr ( Value: Boolean );
    function  GetHintOvr: Boolean;
    procedure SetHintOvr ( Value: Boolean );
    function  GetShowPosAsPct: Boolean;
    procedure SetShowPosAsPct( Value: Boolean );
    function  GetOnProgressChange: TOnQProgressBar;
    procedure SetOnProgressChange( const Value: TOnQProgressBar );

  public
    property Orientation     : TQBarOrientation read GetOrientation  write SetOrientation;
    {* |It's the control orientation parameters at the parent, i.e. if you assign it to <i>boVertical</i>
     then the control's progress will grow up from below upwards instead of from left corner to right.
     |By default: <i>boHorizontal.</i> }

    property BarKind         : TQBarKind        read GetBarKind      write SetBarKind;
    {* Parameter that defines how the control's progress bar row will appear.
     |By default: <i>bkFlat.</i> }

    property BarLook         : TQBarLook        read GetBarLook      write SetBarLook;
    {* Parameter that defines how the control's bar will look.
     |<i>blMetal</i> takes the original color luminence into account when computing each pixel;
     |<i>blGlass</i> don't. <i>blGlass</i> only works on the 'basic color' part of the color of each pixel.
     |By default: <i>blMetal.</i> }

    property RoundCorner     : Boolean          read GetBoolCorner   write SetFCorner;
    {* |If <i>True</i>, the bar's external shape will appear with smoothly rounded corners,
     otherwise it will be a rectangle.
     |By default: <i>True.</i> }

    property BackgroundColor : TColor           read GetBkgColor     write SetBkgColor;
    {* Parameter that defines control background color.
     |By default: <i>clWhite.</i> }

    property BarColor        : TColor           read GetStartClr     write SetBothColors;
    {* Parameter that allows to define a single color bar in one shot: using
    ! aPBar.BarColor:= clLime;
    is equivalent to :
    ! aPBar.StartColor := clLime;
    ! aPBar.FinalColor := clLime; }

    property StartColor      : TColor           read GetStartClr     write SetStartClr;
    {* Left color of a two-colors horizontal bar, or bottom color for vertical bars.
     |By default: <i>clLime.</i> }

    property FinalColor      : TColor           read GetFinalClr     write SetFinalClr;
    {* Right color of a two-colors horizontal bar, or Top color for vertical bars.
     |By default: <i>clLime</i> (default bar is thus monocolor). }

    property ShowInactivePos : Boolean          read GetInactivePos  write SetInactivePos;
    {* Inactive position are the positions not yet reached.
     |If <i>True</i>, they'll be drawn in the
     InactivePosColor,
     |if <i>False</i>, only the background appears there. Inactive positions share appearance
     properties and behaviour (like : by blocks or not, full blocks, BarKind, aso.) with active positions.
     |Only the color differs. By default: <i>False.</i> }

    property InvertInactPos  : Boolean          read GetInvInactPos  write SetInvInactPos;
    {* |If <i>True</i>, the luminance of inactive positions color array is inverted.
     Notice that the result is most often really dark. There's still some work to do there.
     |Applies only on <i>bkCylinder</i> bars. By default: <i>False.</i> }

    property InactivePosColor: TColor           read GetInactPosClr  write SetInactPosClr;
    {* Base color of inactive positions.
     |By default: <i>clGray.</i> }

    property Shaped          : Boolean          read GetShape        write SetShape;
    {* Decides whether the bar has a surrounding line or not.
     |By default: <i>True.</i> }

    property ShapeColor      : TColor           read GetShapeColor   write SetShapeColor;
    {* The color of that surrounding line.
     |By default: <i>RGB (0, 60, 116) (Dark blue)</i> }

    property BlockSize       : Integer          read GetBlockSize    write SetBlockSize;
    {* TKOLQProgressBars can appear under the form of a continuous area or like "blocks"
     separated by not-drawn spaces (where the background appears).
     BlockSize defines the size of blocks in pixels. BlockSize and SpaceSize are ignored if
     one of them is set to zero or set to a value greater than the internal available draw space.
     |By default: <i>0.</i> }

    property SpaceSize       : Integer          read GetSpaceSize    write SetSpaceSize;
    {* TKOLQProgressBars can appear under the form of a continuous area or like "blocks"
     separated by not-drawn spaces (where the background appears).
     SpaceSize defines the size of none drawn parts between two blocks in pixels. BlockSize and
     SpaceSize are ignored if one of them is set to zero or set to a value greater than the internal
     |available draw space. By default: <i>0.</i> }

    property ShowFullBlock   : Boolean          read GetFullBlock    write SetFullBlock;
    {* If both BlockSize and SpaceSize have been defined, the bar will show an alternance
     of blocks and spaces. In this case, if ShowFullBlock is set
     |to <i>True</i>, each new block is drawn only when the position sent corresponds to
     |the end of a block. If set to <i>False</i>, blocks are filled little by little.
     |By default: <i>False.</i> }

    property HideOnTerminate : Boolean          read GetHideOnTerm   write SetHideOnTerm default False;
    {* |If <i>True</i>, the bar will hide itself after it will receive a progress position
     |equal to <i>MaxProgress</i>. In such a case, it will be up to you to show it again if you use it again:
     !uses Windows, Messages, KOL, ..., KOLQProgBar;
     ! //...
     !var aPBar : PQProgressBar;
     ! //...
     !aPBar := NewQProgressBar( AParentForm );
     !aPBar.HideOnTerminate:= true;
     !aPBar. ...
     !// ... do something
     !// ... our jobs finished and progress bar is hidden now
     !// ... restore it with Progress:= 0
     !aPBar.Progress:= 0;
     !aPBar.Show;
     
     |By default: <i>False.</i> }

    property CaptionAlign    : TTextAlign       read GetCapAlign     write SetCapAlign;
    {* Vertical alignment is always almost centered, this one is horizontal alignment,
     |and can be <i>taLeft</i>, <i>taCenter</i>, <i>taRight</i>.
     |By default: <i>taLeft.</i> }

    property AutoCaption     : Boolean          read GetCaptionOvr   write SetCaptionOvr;
    {* |Both caption and hint can be set to display automatically the value <i>Progress</i>.
     |If <i>True</i>, <i>Hint</i> value is refreshed each time you send a new position and <i>Caption</i>
     value is updated within the paint method.
     |By default: <i>False.</i> }

    property AutoHint        : Boolean          read GetHintOvr      write SetHintOvr;
    {* |Both caption and hint can be set to display automatically the value <i>Progress</i>.
     |If <i>True</i>, <i>Hint</i> value is refreshed each time you send a new position and <i>Caption</i>
     value is updated within the paint method. For hint to show when your user moves it's mouse over your bar,
     |you must add <b>USE_MHTOOLTIP</b> conditional symbol into the project options list and your KOLProject
     |must have the <i>ShowHint</i> property set to <i>True</i>. By default: <i>False.</i> }

    property ShowPosAsPct    : Boolean          read GetShowPosAsPct write SetShowPosAsPct;
    {* |If <i>True</i>, both <i>Hint</i> and <i>Caption</i> will show the last received position as
     |a percentage of <i>MaxProgress</i>, followed by the string <i>' %'</i>.
     |By default: <i>False</i>. }

    property OnProgressChange: TOnQProgressBar  read GetOnProgressChange write SetOnProgressChange;
    {* | Called when <i>Progress</i> value is changed. }

  end;

// ----------------------------------------------------------
const
  // NIH... Out a Microsoft knowledge base article, see below "RGBtoHLS" and "HLStoRGB"
  HLSMAX = High(THLSRange);	// H,L, and S vary over 0-HLSMAX
  RGBMAX = 255;		        // R,G, and B vary over 0-RGBMAX
				// HLSMAX BEST IF DIVISIBLE BY 6
				// RGBMAX, HLSMAX must each fit in a byte.
  // Hue is undefined if Saturation is 0 (grey-scale)
  // This value determines where the Hue scrollbar is
  // initially set for achromatic colors 
  UNDEFINED = HLSMAX * 2 div 3;

// ----------------------------------------------------------
function NewQProgressBar( AParent: PControl ): PQProgressBar;
// ----------------------------------------------------------

implementation

// ----------------------------------------------------------
function QProgBar_WndProc( Control: PControl; var Msg: TMsg; var Rslt: Integer): Boolean;
var
 PaintStruct: TPaintStruct;
 ProgressBar: PQProgressBar;
begin
 Result := False;
 ProgressBar:= PQProgressBar( Control );
 case ( Msg.message ) of
  WM_PAINT:
   begin
    BeginPaint( ProgressBar.Handle, PaintStruct );
    ProgressBar.Paint;
    //Result:= True;
    //Rslt:= 0;
    EndPaint( ProgressBar.Handle, PaintStruct );
   end;
  WM_SIZE:
    ProgressBar.Resize;
  PBM_GETPOS:
   begin
    Rslt:= ProgressBar.GetPosition;
    Result:= true;
   end;
  PBM_SETPOS:
   begin
    Rslt:= ProgressBar.GetPosition;
    if ( Msg.wParam > 0 ) then
     ProgressBar.SetPosition( Msg.wParam )
    else
     ProgressBar.SetPosition( 0 );
     with PQDataObj( ProgressBar.CustomObj )^ do
      if Assigned( fOnProgChange ) then
       fOnProgChange( ProgressBar );
    Result := true;
   end;
  PBM_GETRANGE:
   begin
    if ( Msg.wParam ) > 0 then
     Rslt:= 0
    else
     Rslt:= ProgressBar.GetMaxPos;
    Result:= true;
   end;
  PBM_SETRANGE:
   begin
    ProgressBar.SetMaxPos( Hi(Msg.lParam) );
    Result:= true;
   end;
  PBM_SETRANGE32:
   begin
    ProgressBar.SetMaxPos( Msg.lParam );
    Result:= true;
   end;
 end; // case
end;

// ----------------------------------------------------------
function NewQProgressBar( AParent: PControl ): PQProgressBar;
var
 Data: PQDataObj;
begin
 Result := PQProgressBar( _NewControl( AParent, 'QProgressBar',
   WS_VISIBLE + WS_CHILD + SS_NOTIFY, False, {$IFDEF PACK_COMMANDACTIONS}@LabelActions_Packed{$ELSE}@LabelActions{$ENDIF} ) );

 New( Data, Create );  // releases authomatically when the object destroys
 Result.CustomObj := Data;

 with Data^ do
  begin
   SetLength( fPosDescr, 1 );
   fPosDescr[0].isInBlock := False;
   fByBlock       := False;
   fFullBlock     := False;
   fBlockSize     := 0;
   fSpaceSize     := 0;
   fOrientation   := boHorizontal;
   fBarKind       := bkFlat;
   fBarLook       := blMetal;
   fPosition      := 0;
   fHasShape      := True;
   fShapeClr      := RGB (0, 60, 116);
   fStartClr      := clLime;
   fFinalClr      := clLime;
   fMonoClr       := True;
   fBkgClr        := clWhite;
   fShowInactPos  := False;
   fInactPosClr   := clGray;
   fInvInactPos   := False;
   fMaxPos        := 100;
   fInternalBorder:= 2;
   fBorderSize    := 4;
   with Result^ do
   begin
     SetUsefullWidth;
     InitPixArray;
   end;
   fCorner        := 5;
   fCapPos.X      := 0;
   fCapPos.Y      := 0;
   fHasCaption    := False;
   fCaptionOvr    := False;
   fHintOvr       := False;
   fShowPosAsPct  := False;
   fUserPos:= 0;
 end;

 with Result^ do
 begin
  TabStop:= False;
  Caption:= '';
  Enabled:= True;
  Width:= 200;
  Height:= 20;
  DoubleBuffered:= true;
 end;

 Result.AttachProc( QProgBar_WndProc );
end;


// ----------------------------------------------------------
procedure TQProgressBar.InitBlockArray;
// fPosDescr[n] describes each possible position, storing :
// - wether it is in a block or not ;               <- drawing blocks instead of a continuous line
// - what is the block limit for this position;     <- (if full blocks only are to be drawn, then
//   only those which limit is below(H) above(V) current position will be drawn.)
// Computed on size/resize and blocks/space sizes changes only, to avoid computations at runTime.
var i,
    blkStart,
    blkStop : Integer;
    D: PQDataObj;
begin
  D:= PQDataObj( CustomObj );
  if ( D.fBlockSize = 0 ) or ( D.fSpaceSize = 0 ) then Exit;

  if ( D.fUSefullDrawSpace <= 0 ) then
   SetLength( D.fPosDescr, 1 )                           // Position 0 is always False
  else SetLength( D.fPosDescr, D.fUSefullDrawSpace + 1 );

  case ( D.fOrientation ) of
    boHorizontal :
      begin
        D.fPosDescr[0].isInBlock := False;
        blkStart := 3;
        blkStop  := blkStart + D.fBlockSize -1 ;
        with D^ do
        for i := 1 to High( fPosDescr ) do
        begin
          fPosDescr[i].isInBlock := (i >= blkStart) and (i <= blkStop);
          fPosDescr[i].blkLimit  := blkStop;
          if ( i = blkStop ) then
          begin
            blkStart := blkStop  + fSpaceSize + 1;
            blkStop  := blkStart + fBlockSize - 1;
            if blkStop > High( fPosDescr ) then blkStop := High( fPosDescr );
          end;
        end;
      end; {boHrz}
    else                                                             // boVertical; "Else" avoids compiler warnings
      begin
        D.fPosDescr[High( D.fPosDescr )].isInBlock := False;
        blkStart := High( D.fPosDescr ) - 3;
        blkStop  := blkStart - D.fBlockSize + 1 ;
        with D^ do
        for i := D.fUSefullDrawSpace downto D.fBorderSize do
        begin
          fPosDescr[i].isInBlock := (i <= blkStart) and (i >= blkStop);
          fPosDescr[i].blkLimit  := blkStop;
          if ( i = blkStop ) then
          begin
            blkStart := blkStop  - fSpaceSize - 1;
            blkStop  := blkStart - fBlockSize + 1;
            if ( blkStop < fBorderSize ) then blkStop := fBorderSize;
          end;
        end;
      end; {boVert}
  end; {case}
end;

// ----------------------------------------------------------
procedure TQProgressBar.InitPixArray;
// Compute and stores each pixel color, in the case of a gradient, or a double 
// gradient (both directions) in order to speed up things at run time.
var i, j,
    rowSz : integer;
    clr   : TColor;
    HLSr  : THLSRec;
    D: PQDataObj;
begin
  D:= PQDataObj( CustomObj );

  with D^ do
  case ( fOrientation ) of
    boHorizontal : rowSz := Height - (fBorderSize) + 1;
    else           rowSz := Width  - (fBorderSize) + 1;  // boVertical;
  end; {Case}

  with D^ do
  if ( fUSefullDrawSpace <= 0 ) then
   SetLength( fPixDescr, 1)                        // Position 0 is allways False
  else SetLength( fPixDescr, fUSefullDrawSpace + 1);

  // Populates active positions colors array ;
  // -> GetColorBetween works on the horizontal gradient, in the case of a 
  //    boHorizontal bar, with two colors (or on the vertical one, if the 
  //    bar is vertical).
  // -> GetGradientAr2  then returns the row gradient, based upon the header
  //    pixel value for that row in order to give the cylinder appearance.

  with D^ do
  for i := 0 to fUSefullDrawSpace do
  begin
    clr := GetColorBetween( fStartClr, fFinalClr, (i), 0, fUSefullDrawSpace );
    if ( fBarKind = bkCylinder ) then
     fPixDescr[i] := GetGradientAr2( clr, rowSz )
       else
       for j := 0 to rowSz -1 do
            begin
              SetLength( fPixDescr[i], rowSz);
              fPixDescr[i, j] := clr;
            end;
  end;

  // inactive positions decription, used in case 'showInactive positions' is true;
  with D^ do
  if ( ( Height - fBorderSize ) <= 0 ) then
  begin
    SetLength( fInactDescr, 1 );
    fInactDescr[0] := fInactPosClr;
  end
  else
  begin
    if ( fBarKind = bkCylinder ) then
     fInactDescr := GetGradientAr2( fInactPosClr, rowSz )
    else
    begin
           SetLength( fInactDescr,rowSz );
           for j := 0 to rowSz - 1 do
            fInactDescr[j] := fInactPosClr;
         end;
  end;
  
  // case cylindric bar : the background can be basically reversed.
  with D^ do
  if ( ( fBarKind = bkCylinder ) and ( fInvInactPos ) ) then
     for i := 0 to rowSz - 1 do
     begin
       HLSr := RGBtoHLS( fInactDescr[i] );
       HLSr.lum := 240 - HLSr.lum;
       fInactDescr[i] := HLStoRGB(HLSr.hue, HLSr.lum, HLSr.sat);
     end;

end;

// ----------------------------------------------------------
function TQProgressBar.MakeCylinder( h: real): Extended;                      // NIH
// (c) Matthieu Contensou (http://www25.brinkster.com/waypointfrance/cpulog/index.asp)
// who computed the polynome used to provide the "cylinder" appearence to bars :
// "f (h) = -4342,9 h^5 + 10543 h^4 - 8216 h^3 + 2018,1 h^2 + 11,096 h + 164,6"
// "h is the order of the wanted pixel in a column (horizontal bar), or in
// a row (vertical bar), with a value between 0 and 1 (0 -> 100%)"
begin
  Result := ( (-4342.9 * ( IntPower(h, 5) ) )
          +   ( 10543  * ( IntPower(h, 4) ) )
          -   ( 8216   * ( IntPower(h, 3) ) )
          +   ( 2018.1 * ( IntPower(h, 2) ) )
          +   ( 11.096 * h ) + 164.6  ) ;
end;

// ----------------------------------------------------------
function TQProgressBar.GetGradientAr2( aColor: TColor; sz: Integer): TClrArray;
// Version corrected by Bernd Kirchhoff (http://home.germany.net/100-445474/)
// Returns an array of size sz, filled up with a basic gradient; Used to
// provide the "cylindric" appearance.
var i,RP: Integer;
    HLSr: THLSRec;
    D: PQDataObj;
begin
  D:= PQDataObj( CustomObj );
  SetLength( Result, sz );
  for i := 0 to sz - 1 do
  begin
    HLSr := RGBtoHLS(aColor);
    // (c) Bernd Kirchhoff >>>--------------------------------------------------
    if ( D.fBarLook = blGlass ) then
      HLSr.lum := Round( MakeCylinder( (i / sz)) )
    else
    begin
      rp:= HLSr.lum - 212;
      rp:= rp + Trunc( MakeCylinder( i / sz) );
      if ( rp < 0 ) then rp:= 0;
      if ( rp > 240 ) then rp:= 240;
      HLSr.lum :=rp;
    end;
   // <<<-----------------------------------------------------------------------
    Result[i] := HLStoRGB(HLSr.hue, HLSr.lum, HLSr.sat);
  end;
end;

// ----------------------------------------------------------
function TQProgressBar.RGBtoHLS(RGBColor: TColor): THLSRec;              // NIH
// (c) Microsoft. http://support.microsoft.com/default.aspx?scid=kb;en-us;29240
// This is the translation of a Microsoft knowledge base article, pubilshed
// under number Q29240. Msft's knowledge base has a lot of interesting articles.

//(knowledge base = http://support.microsoft.com/default.aspx?scid=FH;EN-US;KBHOWTO)

var
   R, G, B: Integer;                        // input RGB values
   H, L, S: Integer;
   cMax, cMin: Byte;                        // max and min RGB values
   Rdelta, Gdelta, Bdelta: Integer;           // intermediate value: % of spread from max
begin
  // get R, G, and B out of DWORD
  R := GetRValue(RGBColor);
  G := GetGValue(RGBColor);
  B := GetBValue(RGBColor);

  // calculate lightness
  cMax := max( max(R,G), B);
  cMin := min( min(R,G), B);
  L := ( ( (cMax+cMin) * HLSMAX) + RGBMAX ) div (2*RGBMAX);

  if (cMax = cMin) then                     // r=g=b --> achromatic case
  begin
     S := 0;                                // saturation
     H := UNDEFINED;                        // hue
  end else
  begin	                                    // chromatic case
     if (L <= (HLSMAX div 2) )              // saturation
     then S := ( ( (cMax-cMin) * HLSMAX ) + ( (cMax+cMin) div 2) ) div (cMax+cMin)
     else S := ( ( (cMax-cMin) * HLSMAX ) + ( (2*RGBMAX-cMax-cMin) div 2) ) div (2*RGBMAX-cMax-cMin);
     // hue
     Rdelta := ( ( (cMax-R) * (HLSMAX div 6) ) + ((cMax-cMin) div 2) ) div (cMax-cMin);
     Gdelta := ( ( (cMax-G) * (HLSMAX div 6) ) + ((cMax-cMin) div 2) ) div (cMax-cMin);
     Bdelta := ( ( (cMax-B) * (HLSMAX div 6) ) + ((cMax-cMin) div 2) ) div (cMax-cMin);

     if R = cMax then      H := Bdelta - Gdelta
     else if G = cMax then H := (HLSMAX div 3) + Rdelta - Bdelta
          else {B=cMax}    H := ( (2*HLSMAX) div 3) + Gdelta - Rdelta;
     if (H < 0)      then  H := H + HLSMAX;
     if (H > HLSMAX) then  H := H - HLSMAX;
  end;

  Result.Hue := H;
  Result.Lum := L;
  Result.Sat := S;
end;

// ----------------------------------------------------------
function TQProgressBar.HLStoRGB( hue, lum, sat: THLSRange): TColor;         // NIH
// (c) Microsoft. http://support.microsoft.com/default.aspx?scid=kb;en-us;29240
var
   R,G,B : Integer;                         // RGB component values
   Magic1,Magic2: Integer;                  // calculated magic numbers (really!)


   { -----------------  LOCAL  -----------------}

     function HueToRGB(n1, n2, hue: Integer): Integer;                // (c) Microsoft.
     // utility routine for HLStoRGB
     begin
       // range check: note values passed add/subtract thirds of range
       if      hue < 0      then Inc(hue, HLSMAX)
       else if hue > HLSMAX then Dec(hue, HLSMAX);

       (* return r,g, or b value from this tridrant *)
       if ( hue < (HLSMAX div 6) )
       then result := ( n1 + ( ( (n2-n1) * hue + (HLSMAX div 12) ) div (HLSMAX div 6) ) )
       else if hue < (HLSMAX div 2)
            then result := n2
            else if hue < ( (HLSMAX*2) div 3 )
                 then result := ( n1 + ( ( (n2-n1) * ( ( (HLSMAX*2) div 3 ) - hue )
                                     + (HLSMAX div 12) ) div (HLSMAX div 6) ) )
                 else result := n1;
     end; {HueToRGB}

   { ----------------- \LOCAL\ -----------------}

begin
  if ( Sat = 0 ) then                           // achromatic case
  begin
    R := (Lum*RGBMAX) div HLSMAX;
    G := R;
    B := R;
    if not( Hue = UNDEFINED ) then
    begin
      // ...trap impossible conversions (?)...
    end;
  end else
  begin                                     // chromatic case
    if (Lum <= (HLSMAX div 2))              // set up magic numbers
    then Magic2 := ( Lum * ( HLSMAX + Sat )  + ( HLSMAX div 2 ) ) div HLSMAX
    else Magic2 := Lum + Sat - ( (Lum * Sat) + ( HLSMAX div 2 ) ) div HLSMAX;
    Magic1 := 2*Lum - Magic2;

    // get RGB, change units from HLSMAX to RGBMAX
    R := ( HueToRGB( Magic1, Magic2, Hue  + ( HLSMAX div 3 ) ) * RGBMAX + ( HLSMAX div 2) ) div HLSMAX;
    G := ( HueToRGB( Magic1, Magic2, Hue )* RGBMAX +(HLSMAX div 2 ) ) div HLSMAX;
    B := ( HueToRGB( Magic1, Magic2, Hue  - ( HLSMAX div 3 ) ) * RGBMAX + ( HLSMAX div 2) ) div HLSMAX;
  end;
  Result :=  RGB(R ,G, B);
end;

// ----------------------------------------------------------
function TQProgressBar.GetColorBetween( AStartColor, AEndColor: TColor; PointValue,
                                       Von, Bis : Extended ): TColor;                 // NIH
// Found on efg's colors pages, at http://homepages.borland.com/efg2lab/Library/Delphi/Graphics/Color.htm
// "Color gradient" row, cworn's UseNet Post.
// Author is unknown, but remains holder for intellectual property.
// High speed function which returns the gradient color value for a pixel depending
// on start and final color, size of the gradient area , and the place of the current pixel;

var
 F: Extended; r1, r2, r3, g1, g2, g3, b1, b2, b3: Byte;

   { -----------------  LOCAL  -----------------}
  function CalcColorBytes(fb1, fb2: Byte): Byte;
  begin
    Result := fb1;
    if ( fb1 < fb2 ) then Result := FB1 + Trunc( F * (fb2 - fb1) );
    if ( fb1 > fb2 ) then Result := FB1 - Trunc( F * (fb1 - fb2) );
  end;
  { ----------------- \LOCAL\ -----------------}

begin
  if ( PQDataObj( CustomObj ).fMonoClr ) or ( PointValue <= Von ) then
  begin
    Result := AStartColor;
    Exit;
  end;
  if ( PointValue >= Bis ) then
  begin
    Result := AEndColor;
    Exit;
  end;
  F := (PointValue - Von) / (Bis - Von);
  asm
     mov EAX, AStartColor
     cmp EAX, AEndColor
     je @@exit
     mov r1, AL
     shr EAX,8
     mov g1, AL
     shr Eax,8
     mov b1, AL
     mov Eax, AEndColor
     mov r2, AL
     shr EAX,8
     mov g2, AL
     shr EAX,8
     mov b2, AL
     push ebp
     mov al, r1
     mov dl, r2
     call CalcColorBytes
     pop ecx
     push ebp
     Mov r3, al
     mov dL, g2
     mov al, g1
     call CalcColorBytes
     pop ecx
     push ebp
     mov g3, Al
     mov dL, B2
     mov Al, B1
     call CalcColorBytes
     pop ecx
     mov b3, al
     XOR EAX,EAX
     mov AL, B3
     SHL EAX,8
     mov AL, G3
     SHL EAX,8
     mov AL, R3
@@Exit:
     mov @result, eax
  end;
end;

// ----------------------------------------------------------
procedure TQProgressBar.Paint;
// Main loop. Called each time a setting changes, notably, each time
// a new position is sent.
// Surround is drawn first, then the bar itself. Caption is added lastly (if needed).

var i,k,sp: Integer;
    OldBkMode : Integer;
    D: PQDataObj;
begin
  D:= PQDataObj( CustomObj );

  with Canvas^ do
  begin
    Brush.Color:= Parent.Color;
    FillRect( MakeRect(0, 0, Width, Height ));

    // -1- Bevel
    if ( D.fHasShape ) then
    begin
      Pen.PenWidth := 1;
      Brush.BrushStyle := bsSolid;
      Brush.Color:= Parent.Color;
      FillRect( MakeRect(0, 0, Width, Height ));
      Brush.Color := D.fBkgClr;
      Pen.Color   := D.fShapeClr;
      RoundRect (0, 0, Width, Height, D.fCorner, D.fCorner);
    end;
  end;

  // -2- The bar itself
  case D.fOrientation of
    boHorizontal :
      begin
        for i := ( D.fBorderSize - 1 ) to D.fPosition  do
        begin
          if ( D.fByBlock ) then
          begin
            if ( D.fPosDescr[i].isInBlock = true) then
            begin
              if  ( (D.fFullBlock) and (D.fPosition >= D.fPosDescr[i].blkLimit) )
              or not( D.fFullBlock ) then
              for k := (D.fBorderSize - 1) to (Height - (D.fBorderSize))
                       do Canvas.Pixels [i,k] := D.fPixDescr[i,k]
              else if (D.fShowInactPos) then
                      for k := (D.fBorderSize - 1) to (Height -(D.fBorderSize))
                          do Canvas.Pixels [i,k] := D.fInactDescr[k];
            end;
          end else
          begin
            for k := (D.fBorderSize - 1) to (Height -(D.fBorderSize)) do
                Canvas.Pixels [i,k] := D.fPixDescr[i,k];
          end;
        end;
        // Now dealing with inactive positions, if they're to be drawn.
        if ( D.fShowInactPos ) then
        begin
          if (D.fPosition < 3) then sp := 3
          else sp := D.fPosition + 1;
          for i := sp to D.fUSefullDrawSpace do
          begin
            if (D.fByBlock) then
            begin
              if (D.fPosDescr[i].isInBlock = True) then
              begin
                 for k := (D.fBorderSize -1) to (Height -(D.fBorderSize)) do
                       Canvas.Pixels [i,k] := D.fInactDescr[k];
              end;
            end else  //If not(byBlock), all pixels must be drawn
            begin
              for k := (D.fBorderSize - 1) to (Height -(D.fBorderSize)) do
                  Canvas.Pixels [i,k] := D.fInactDescr[k];
            end;
          end; {for}
        end; {inactive}
      end; {boHorizontal}
    boVertical :
      begin
        for i := (D.fUSefullDrawSpace-1) downto Height - D.fPosition  do
        begin
          if (D.fByBlock) then
          begin
            if (D.fPosDescr[i].isInBlock = true) then
            begin
              if  ( (D.fFullBlock) and ((Height - D.fPosition) <= D.fPosDescr[i].blkLimit) )
              or not( D.fFullBlock ) then
               for k := (D.fBorderSize - 1 ) to (Width - (D.fBorderSize))
                       do Canvas.Pixels [k,i] := D.fPixDescr[i,k]
              else if ( D.fShowInactPos ) then
                       for k := (D.fBorderSize - 1) to (Width -(D.fBorderSize))
                           do Canvas.Pixels [k,i] := D.fInactDescr[k];
            end;
          end
          else
           for k := (D.fBorderSize - 1) to (Width -(D.fBorderSize))
                       do Canvas.Pixels [k,i] := D.fPixDescr[i,k];
        end;
        // inactive positions :
        if (D.fShowInactPos) then
        begin
          if ( D.fPosition < 3 ) then sp := D.fUSefullDrawSpace
          else sp := Height - D.fPosition - 1;
          for i := sp downto D.fBorderSize do
          begin
            if ( D.fByBlock ) then
            begin
              if ( D.fPosDescr[i].isInBlock = true ) then
              begin
                 for k := (D.fBorderSize - 1) to (Width -(D.fBorderSize)) do
                     Canvas.Pixels [k,i] := D.fInactDescr[k];
              end;
            end else
              for k := (D.fBorderSize - 1) to (Width -(D.fBorderSize))
                  do Canvas.Pixels [k,i] := D.fInactDescr[k];
          end; {for... downto}
        end; {inactive}
      end; {boVertical}
    end; // Case

    // caption management. The font is the canvas' one. Can be overrided
    // using the Font property :
    if ( D.fCaptionOvr ) then
    begin
       if ( D.fShowPosAsPct ) then SetCaption( Double2Str( D.fUSerPosPct ) + '%')
       else SetCaption( Int2Str(D.fUSerPos) );
    end
    else SetCaption( Caption );

    if ( D.fHasCaption ) then
    begin
      OldBkMode := SetBkMode(Canvas.Handle, Windows.TRANSPARENT);
      with Canvas^ do
      begin
        TextOut(D.fCapPos.X, D.fCapPos.Y, Caption);
      end;
      SetBkMode(Canvas.Handle, OldBkMode);
    end;

end;

// ----------------------------------------------------------
procedure TQProgressBar.Resize;
var
  D: PQDataObj;
begin
  D:= PQDataObj( CustomObj );
  D.fBorderSize := D.fInternalBorder shl 1;
  SetUsefullWidth;

  if ( D.fByBlock ) then InitBlockArray;
  InitPixArray;
  SetPosition( D.fUserPos );  // position  is computed, then bar is invalidated ;
end;

// ----------------------------------------------------------
procedure TQProgressBar.SetUsefullWidth;
var
 D: PQDataObj;
begin
  D:= PQDataObj( CustomObj );
  case ( D.fOrientation ) of
    boHorizontal : D.fUSefullDrawSpace := ( Width  - ( D.fBorderSize ));
    boVertical   : D.fUSefullDrawSpace := ( Height - ( D.fBorderSize ));
  end;
  D.fMinVisPos := D.fBorderSize + 1;
end;

// ----------------------------------------------------------
procedure TQProgressBar.SetFCorner( IsRounded:Boolean );
var
 D: PQDataObj;
begin
  D:= PQDataObj( CustomObj );
  if ( IsRounded ) then D.fCorner := 5
   else D.fCorner := 0;
  Invalidate;
end;

// ----------------------------------------------------------
function TQProgressBar.GetBoolCorner: Boolean;
begin
  Result := ( PQDataObj( CustomObj ).fCorner > 0 );
end;

// ----------------------------------------------------------
function TQProgressBar.GetBarKind: TQBarKind;
begin
  Result:= PQDataObj( CustomObj ).fBarKind;
end;

// ----------------------------------------------------------
procedure TQProgressBar.SetBarKind( Value: TQBarKind );
begin
  PQDataObj( CustomObj ).fBarKind := Value;
  InitPixArray;
  Invalidate;
end;

// ----------------------------------------------------------
function TQProgressBar.GetBarLook: TQBarLook;
begin
 Result:= PQDataObj( CustomObj ).fBarLook;
end;

// ----------------------------------------------------------
procedure TQProgressBar.SetBarLook( Value: TQBarLook );
begin
  PQDataObj( CustomObj ).fBarLook := Value;
  InitPixArray;
  Invalidate;
end;

// ----------------------------------------------------------
function TQProgressBar.GetOrientation: TQBarOrientation;
begin
 Result:= PQDataObj( CustomObj ).fOrientation;
end;

// ----------------------------------------------------------
procedure TQProgressBar.SetOrientation( Value: TQBarOrientation );
var newH,
    newW: Integer;
    D: PQDataObj;
begin
  D:= PQDataObj( CustomObj );
  if ( Value <> D.fOrientation ) then
  begin
    if ( ( Value = boVertical)   and ( Height < Width) )
    or ( ( Value = boHorizontal) and ( Width  < Height) )
    then
    begin
       newW := Height;
       newH := Width;
       Height := newH;
       Width  := newW;
    end;
    D.fOrientation := Value;
  end;
  case ( Value ) of
    boHorizontal : if Height < 10
                      then D.fInternalBorder := 1
                      else D.fInternalBorder := 2;
    boVertical   : if Width  < 10
                      then D.fInternalBorder := 1
                      else D.fInternalBorder := 2;
  end; //Case
  D.fBorderSize := D.fInternalBorder shl 1;
  SetUsefullWidth;
  InitBlockArray;
  InitPixArray;
  Invalidate;
end;

// ----------------------------------------------------------
function TQProgressBar.GetBkgColor: TColor;
begin
 Result:= PQDataObj( CustomObj ).fBkgClr;
end;

// ----------------------------------------------------------
procedure TQProgressBar.SetBkgColor( aColor: TColor );
begin
  PQDataObj( CustomObj ).fBkgClr := aColor;
  Invalidate;
end;

// ----------------------------------------------------------
procedure TQProgressBar.SetShape( Value: Boolean );
begin
  PQDataObj( CustomObj ).fHasShape := Value;
  Invalidate;
end;

// ----------------------------------------------------------
function TQProgressBar.GetShape: Boolean;
begin
 Result:= PQDataObj( CustomObj ).fHasShape;
end;

// ----------------------------------------------------------
procedure TQProgressBar.SetShapeColor( Value: TColor );
begin
  PQDataObj( CustomObj ).fShapeClr := Value;
  Invalidate;
end;

// ----------------------------------------------------------
function TQProgressBar.GetShapeColor: TColor;
begin
 Result:= PQDataObj( CustomObj ).fShapeClr;
end;

// ----------------------------------------------------------
procedure TQProgressBar.SetBlockSize( Value:Integer );
var
  D: PQDataObj;
begin
  D:= PQDataObj( CustomObj );
  case D.fOrientation of
    boHorizontal : if ( Value > Width  - ( D.fInternalBorder shl 1 ) ) then Exit;
    boVertical   : if ( Value > Height - ( D.fInternalBorder shl 1) ) then Exit;
  end; {case}

  D.fBlockSize := Abs(value);
  D.fByBlock   := (D.fBlockSize > 0) and (D.fSpaceSize > 0);
  if ( D.fByBlock ) then InitBlockArray;
  InitPixArray;
  Invalidate;
end;

// ----------------------------------------------------------
function TQProgressBar.GetBlockSize: Integer;
begin
 Result:= PQDataObj( CustomObj ).fBlockSize;
end;

// ----------------------------------------------------------
procedure TQProgressBar.SetSpaceSize( Value: Integer);
var
  D: PQDataObj;
begin
  D:= PQDataObj( CustomObj );
  case D.fOrientation of
    boHorizontal : if ( Value > Width  - (D.fInternalBorder SHL 1) ) then Exit;
    boVertical   : if ( Value > Height - (D.fInternalBorder SHL 1) ) then Exit;
  end; {case}

  D.fSpaceSize := Abs(value);
  D.fByBlock   := ( D.fBlockSize > 0 ) and ( D.fSpaceSize > 0 );
  if ( D.fByBlock ) then InitBlockArray;
  InitPixArray;
  Invalidate;
end;

// ----------------------------------------------------------
function TQProgressBar.GetSpaceSize: Integer;
begin
 Result:= PQDataObj( CustomObj ).fSpaceSize;
end;

// ----------------------------------------------------------
procedure TQProgressBar.SetFullBlock( Value:Boolean );
begin
  PQDataObj( CustomObj ).fFullBlock := Value;
  if ( Value ) then InitBlockArray;
  InitPixArray;
  Invalidate;
end;

// ----------------------------------------------------------
function TQProgressBar.GetFullBlock: Boolean;
begin
 Result:= PQDataObj( CustomObj ).fFullBlock;
end;

// ----------------------------------------------------------
procedure TQProgressBar.SetMaxPos( Value: Integer );
var
  D: PQDataObj;
begin
  D:= PQDataObj( CustomObj );
  if ( Value < 0 ) then D.fMaxPos := 0
  else D.fMaxPos := Value;
  SetPosition( D.fUserPos );
end;

// ----------------------------------------------------------
function TQProgressBar.GetMaxPos: Integer;
begin
 Result:= PQDataObj( CustomObj ).fMaxPos;
end;

// ----------------------------------------------------------
procedure TQProgressBar.SetPosition( Value: Integer );
var
 tmpfPos : Real;  D: PQDataObj;
begin
  D:= PQDataObj( CustomObj );
  D.fUserPos := Value;
  if ( D.fMaxPos = 0 ) then Exit;
  try
    if ( Value <= 0 ) then
    begin
      D.fPosition := 0;
      Exit;
  end
  else if ( Value > D.fMaxPos ) then Value := D.fMaxPos;

    D.fUSerPosPct := (100 * Value) / D.fMaxPos;
    tmpfPos := D.fUsefullDrawSpace * D.fUSerPosPct / 100;
    // If value( user position) > 0, make sure that at least one bar is visible
    if ( tmpfPos > 0.00 ) and ( tmpfPos < D.fMinVisPos )
       then D.fPosition := D.fMinVisPos
       else if tmpfPos  > D.fUsefullDrawSpace
            then D.fPosition := D.fUsefullDrawSpace
            else D.fPosition := Round( tmpfPos );
    // Hint is managed here (whereas caption, which ahs to be painted,
    // is managed in the paint() proc).
    {$IFDEF USE_MHTOOLTIP}
    if ( D.fHintOvr ) then
       if ( D.fShowPosAsPct ) then Hint.Text := Double2Str( D.fUSerPosPct ) + ' %'
       else Hint.Text := Int2Str( D.fUSerPos );
    {$ENDIF}
  finally
    Invalidate;
    if ( ( D.fHideOnTerm ) and ( Value = D.fMaxPos ) ) then
    begin
     Sleep(100);
     Hide;
    end;
  end;
end;

// ----------------------------------------------------------
function TQProgressBar.GetPosition: Integer;
begin
  Result:= PQDataObj( CustomObj ).fUserPos;
end;

// ----------------------------------------------------------
procedure TQProgressBar.SetStartClr( Value: TColor);
var
  D: PQDataObj;
begin
  D:= PQDataObj( CustomObj );
  D.fStartClr :=  Value;
  D.fMonoClr  := ( D.fStartClr = D.fFinalClr );
  InitPixArray;
  Invalidate;
end;

// ----------------------------------------------------------
function TQProgressBar.GetStartClr: TColor;
begin
  Result:= PQDataObj( CustomObj ).fStartClr;
end;

// ----------------------------------------------------------
procedure TQProgressBar.SetFinalClr( Value: TColor );
var
  D: PQDataObj;
begin
  D:= PQDataObj( CustomObj );
  D.fFinalClr :=  Value;
  D.fMonoClr  := ( D.fStartClr = D.fFinalClr );
  InitPixArray;
  Invalidate;
end;

// ----------------------------------------------------------
function TQProgressBar.GetFinalClr: TColor;
begin
  Result:= PQDataObj( CustomObj ).fFinalClr;
end;

// ----------------------------------------------------------
procedure TQProgressBar.SetBothColors( Value: TColor );
var
  D: PQDataObj;
begin
  D:= PQDataObj( CustomObj );
  D.fMonoClr  := True;
  D.fStartClr := Value;
  D.fFinalClr := Value;
  InitPixArray;
  Invalidate;
end;

// ----------------------------------------------------------
function TQProgressBar.GetInactivePos: Boolean;
begin
  Result:= PQDataObj( CustomObj ).fShowInactPos;
end;

// ----------------------------------------------------------
procedure TQProgressBar.SetInactivePos( Value: Boolean );
begin
  PQDataObj( CustomObj ).fShowInactPos :=  Value;
  InitPixArray;
  Invalidate;
end;

// ----------------------------------------------------------
function TQProgressBar.GetInactPosClr: TColor;
begin
  Result:= PQDataObj( CustomObj ).fInactPosClr;
end;

// ----------------------------------------------------------
procedure TQProgressBar.SetInactPosClr( Value: TColor );
begin
  PQDataObj( CustomObj ).fInactPosClr :=  Value;
  InitPixArray;
  Invalidate;
end;

// ----------------------------------------------------------
procedure TQProgressBar.SetHideOnTerm( Value: Boolean );
begin
 PQDataObj( CustomObj ).fHideOnTerm:= Value;
end;

// ----------------------------------------------------------
function TQProgressBar.GetHideOnTerm: Boolean;
begin
  Result:= PQDataObj( CustomObj ).fHideOnTerm;
end;

// ----------------------------------------------------------
function TQProgressBar.GetInvInactPos: Boolean;
begin
  Result:= PQDataObj( CustomObj ).fInvInactPos;
end;

// ----------------------------------------------------------
procedure TQProgressBar.SetInvInactPos( Value: Boolean);
// invert Inactive Positions lum.
begin
  PQDataObj( CustomObj ).fInvInactPos := Value;
  InitPixArray;
  Invalidate;
end;

// ----------------------------------------------------------
procedure TQProgressBar.SetCaption( Value: string );
var
  D: PQDataObj;
begin
  D:= PQDataObj( CustomObj );
  Caption := Value;
  D.fHasCaption := not( Value = '' );

  if ( D.fHasCaption ) then
  begin
    //-1- Centering vertically
    D.fCapPos.Y := ( Height - Canvas.textHeight( 'Pg' ) ) div 2 ;
    case ( D.fCapAlign ) of
      taLeft:
            begin
              D.fCapPos.X := 0;
            end;
      taCenter:
            begin
              D.fCapPos.X := ( Width - Canvas.textWidth( Value ) ) div 2;
            end;
      else  begin  //right alignment; -taRight-
              D.fCapPos.X := ( Width - Canvas.textWidth( value ) ) -1 ;
            end;
    end; {case}
  end;
end;

// ----------------------------------------------------------
function TQProgressBar.GetCapAlign: TTextAlign;
begin
  Result:= PQDataObj( CustomObj ).fCapAlign;
end;

// ----------------------------------------------------------
procedure TQProgressBar.SetCapAlign( Value: TTextAlign );
var
  D: PQDataObj;
begin
  D:= PQDataObj( CustomObj );
  D.fCapAlign := Value;
  Invalidate;
end;

// ----------------------------------------------------------
function TQProgressBar.GetCaptionOvr: Boolean;
begin
  Result:= PQDataObj( CustomObj ).fCaptionOvr;
end;

// ----------------------------------------------------------
procedure TQProgressBar.SetCaptionOvr( Value:Boolean );
begin
  PQDataObj( CustomObj ).fCaptionOvr := Value;
  Invalidate;
end;

// ----------------------------------------------------------
function TQProgressBar.GetHintOvr: Boolean;
begin
  Result:= PQDataObj( CustomObj ).fHintOvr;
end;

// ----------------------------------------------------------
procedure TQProgressBar.SetHintOvr( Value: Boolean );
begin
  PQDataObj( CustomObj ).fHintOvr:= Value;
end;

// ----------------------------------------------------------
procedure TQProgressBar.SetShowPosAsPct( Value: Boolean );
begin
  PQDataObj( CustomObj ).fShowPosAsPct:= Value;
end;

// ----------------------------------------------------------
function TQProgressBar.GetShowPosAsPct: Boolean;
begin
  Result:= PQDataObj( CustomObj ).fShowPosAsPct;
end;

// ----------------------------------------------------------
function TQProgressBar.GetOnProgressChange: TOnQProgressBar;
begin
  Result:= PQDataObj( CustomObj ).fOnProgChange;
end;

// ----------------------------------------------------------
procedure TQProgressBar.SetOnProgressChange( const Value: TOnQProgressBar );
begin
  PQDataObj( CustomObj ).fOnProgChange:= Value;
end;

// ----------------------------------------------------------
destructor TQDataObj.Destroy;
begin
  @fOnProgChange:= nil;
  SetLength( fPosDescr, 0);
  SetLength( fPixDescr, 0);
  inherited;
end;

// ----------------------------------------------------------


end.

