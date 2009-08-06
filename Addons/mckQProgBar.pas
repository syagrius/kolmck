unit mckQProgBar;
{

  ("`-''-/").___..--''"`-._
 `6_ 6  )   `-.  (     ).`-.__.`)
 (_Y_.)'  ._   )  `._ `. ``-..-'
  _..`--'_..-_/  /--'_.' ,'
(il).-''  (li).'  ((!.-'

 QnnO Progress Bar (MCK Classes)
 The component that provides a set of various progress bars.

 Ported to KOL © 2007 Danger
 E-Mail: <danger@artline.kz> <fateyev@inbox.ru>

 Original excellent TQProgressBar VCL component was developed by QnnO
 and was ported to KOL with his permission. Merci a Qnno!

}
interface

// ----------------------------------------------------------
uses
  Windows, Messages, KOL, SysUtils, Math, Classes, Controls,
  KOLQProgBar, mirror, Graphics;
// ----------------------------------------------------------

type
  TKOLQProgressBar = class( TKOLControl )

  private
    fOrientation: TQBarOrientation;
    fBarKind: TQBarKind;
    fBarLook: TQBarLook;
    fRoundCorner: Boolean;
    fBackColor: TColor;
    fBarColor: TColor;
    fStartColor: TColor;
    fFinalColor: TColor;
    fShowInactPos: Boolean;
    fInvertInactPos: Boolean;
    fInactPosColor: TColor;
    fShaped: Boolean;
    fShapeColor: TColor;
    fBlockSize: Integer;
    fSpaceSize: Integer;
    fShowFullBlock: Boolean;
    fMaximum: Integer;
    fPosition: Integer;
    fHideOnTerm: Boolean;
    fCaptionAlign: TTextAlign;
    fAutoCaption: Boolean;
    fAutoHint: Boolean;
    fShowPosAsPct: Boolean;
    fFont: TKOLFont;

    fCorner: Integer;
    fBorderSize: Integer;
    fByBlock: Boolean;
    fPosDescr: array of TPosDescr;
    fPixDescr: array of TCLRArray;
    fInactDescr: TCLRArray;
    fUSefullDrawSpace: Integer;
    fMonoClr: Boolean;
    fUserPos: Integer;
    fUSerPosPct: Real;
    fHasCaption: Boolean;
    fCapPos: TPoint;
    fMinVisPos: Integer;
    fInternalBorder: Integer;
    fOnProgressChange: TOnQProgressBar;
    fNotAvailable: Boolean;
    fColorNotAvailable: TColor;
    fOnPaintNotAvailable: TOnPaint;

    procedure SetOrientation( Value: TQBarOrientation );
    procedure SetBarKind( Value: TQBarKind );
    procedure SetBarLook( Value: TQBarLook);
    procedure SetRoundCorner( Value: Boolean );
    procedure SetBackColor( Value: TColor );
    procedure SetBarColor( Value: TColor );
    procedure SetStartColor( Value: TColor );
    procedure SetFinalColor(Value: TColor );
    procedure SetShowInactPos( Value: Boolean );
    procedure SetInvertInactPos( Value: Boolean );
    procedure SetInactPosColor( Value: TColor );
    procedure SetShaped( Value: Boolean );
    procedure SetShapeColor( Value: TColor );
    procedure SetBlockSize( Value: Integer );
    procedure SetSpaceSize( Value: Integer );
    procedure SetShowFullBlock( Value: Boolean );
    procedure SetMaximum( Value: Integer );
    procedure SetPosition( Value: Integer );
    procedure SetHideOnTerm( Value: Boolean );
    procedure SetCaptionAlign( Value: TTextAlign );
    procedure SetAutoCaption( Value: Boolean );
    procedure SetAutoHint( Value: Boolean );
    procedure SetShowPosAsPct( Value: Boolean );

    procedure SetUsefullWidth;
    procedure InitBlockArray;
    procedure InitPixArray;
    function GetGradientAr2( aColor: TColor; sz:Integer ): TClrArray;
    function HLStoRGB( hue, lum, sat: THLSRange ): TColor;
    function GetColorBetween( StartColor, EndColor: TColor; Pointvalue,
      Von, Bis : Extended ): TColor;
    function RGBtoHLS( RGBColor: TColor ): THLSRec;
    function MakeCylinder( h: Real ): Extended;
    procedure SetCaption( Value: string );
    procedure SetFont( Value: TKOLFont );

  protected
    procedure Paint; override;
    procedure Resize; override;
    function  AdditionalUnits: string; override;
    procedure SetupFirst( SL: TStringList; const AName, AParent, Prefix: string ); override;
    function  SetupParams( const AName, AParent: string ): string; override;
    procedure SetupConstruct( SL: TStringList; const AName, AParent, Prefix: string ); override;
    procedure AssignEvents( SL: TStringList; const AName: string ); override;
    procedure SetOnProgressChange( Value: TOnQProgressBar );

  public
    constructor Create( AOwner: TComponent ); override;

  published
    property Orientation: TQBarOrientation read fOrientation write SetOrientation;
    property BarKind: TQBarKind read fBarKind write SetBarKind;
    property BarLook: TQBarLook read fBarLook write SetBarLook;
    property RoundCorner: Boolean read fRoundCorner write SetRoundCorner;
    property BackgroundColor: TColor read fBackColor write SetBackColor;
    property BarColor: TColor read fBarColor write SetBarColor;
    property StartColor: TColor read fStartColor write SetStartColor;
    property FinalColor: TColor read fFinalColor write SetFinalColor;
    property ShowInactivePos: Boolean read fShowInactPos write SetShowInactPos;
    property InvertInactPos: Boolean read fInvertInactPos write SetInvertInactPos;
    property InactivePosColor: TColor read fInactPosColor write SetInactPosColor;
    property Shaped: Boolean read fShaped write SetShaped;
    property ShapeColor: TColor read fShapeColor write SetShapeColor;
    property BlockSize: Integer read fBlockSize write SetBlockSize;
    property SpaceSize: Integer read fSpaceSize write SetSpaceSize;
    property ShowFullBlock: Boolean read fShowFullBlock write SetShowFullBlock;
    property MaxProgress: Integer read fMaximum write SetMaximum;
    property Progress: Integer read fUserPos write SetPosition;
    property HideOnTerminate: Boolean read fHideOnTerm write SetHideOnTerm;
    property CaptionAlign: TTextAlign read fCaptionAlign write SetCaptionAlign;
    property AutoCaption: Boolean read fAutoCaption write SetAutoCaption;
    property AutoHint: Boolean read fAutoHint write SetAutoHint;
    property ShowPosAsPct: Boolean read fShowPosAsPct write SetShowPosAsPct;
    property Caption: string read fCaption write SetCaption;
    property Font: TKOLFont read fFont write SetFont;
    property OnProgressChange: TOnQProgressBar read fOnProgressChange write SetOnProgressChange;
    property DoubleBuffered: Boolean read fNotAvailable;
    property Ctl3D: Boolean read fNotAvailable;
    property Color: TColor read fColorNotAvailable;
    property EraseBackground: Boolean read fNotAvailable;
    property OnEraseBkgnd: TOnPaint read fOnPaintNotAvailable;
  end;

// ----------------------------------------------------------
procedure Register;

implementation

// ----------------------------------------------------------
procedure Register;
begin
  RegisterComponents( 'KOLAddons', [ TKOLQProgressBar ] );
end;

// ----------------------------------------------------------
{ TKOLQProgressBar }

function TKOLQProgressBar.AdditionalUnits: String;
begin
  Result := ', KOLQProgBar';
end;

// ----------------------------------------------------------
constructor TKOLQProgressBar.Create( AOwner: TComponent );
begin
  inherited;
  Width:= 200;
  DefaultWidth:= Width;
  Height:= 20;
  DefaultHeight:= Height;
  ControlStyle := ControlStyle + [ csAcceptsControls ];
  SetLength( fPosDescr, 1 );
  fPosDescr[0].IsInBlock := False;
  fFont := TKOLFont.Create( Self );

  fShowFullBlock:= false;
  fBlockSize:= 0;
  fSpaceSize:= 0;
  fOrientation:= boHorizontal;
  fBarKind:= bkFlat;
  fBarLook:= blMetal;
  fPosition:= 0;
  fShaped:= true;
  fShapeColor:= $00743C00;
  fBarColor:= clLime;
  fStartColor:= clLime;
  fFinalColor:= clLime;
  fBackColor:= clWhite;
  fShowInactPos:= false;
  fInactPosColor:= clGray;
  fInvertInactPos:= false;
  fMaximum:= 100;
  fAutoCaption:= false;
  fAutoHint:= false;
  fShowPosAsPct:= false;
  fHideOnTerm:= false;
  fRoundCorner:= true;
  fCaptionAlign:= taLeft;

  SetUsefullWidth;
  InitPixArray;
  fCorner:= 5;
  fBorderSize:= 4;
  fByBlock:= False;
  fMonoClr:= True;
  fHasCaption:= False;
  fCaption:= '';
  fCapPos.X:= 0;
  fCapPos.Y:= 0;
  fInternalBorder:= 2;
end;

// ----------------------------------------------------------
procedure TKOLQProgressBar.Resize;
begin
  inherited Resize;
  fBorderSize := fInternalBorder shl 1;
  SetUsefullWidth;

  if ( fByBlock ) then InitBlockArray;
  InitPixArray;
  Progress := fUserPos;
  SetCaption( fCaption );
end;

// ----------------------------------------------------------
procedure TKOLQProgressBar.SetCaption( Value: string);
begin
  fCaption := Value;
  fHasCaption := not(Value = '');
  if ( fHasCaption ) then
  begin
    //-1- Centering vertically
    fCapPos.Y := ( Height - Canvas.textHeight('Pg')) div 2 ;

    case ( fCaptionAlign ) of
      taLeft :
            begin
              fCapPos.X := 0;
            end;
      taCenter :
            begin
              fCapPos.X := ( Width - Canvas.textWidth(value) ) div 2;
            end;
      else  
            begin  //right alignment;
              fCapPos.X := ( Width - Canvas.textWidth(value) ) -1 ;
            end;
    end; {case}
  end;
  if not( fAutoCaption ) then Invalidate;
  Change;
end;

// ----------------------------------------------------------
procedure TKOLQProgressBar.SetFont( Value: TKOLFont );
begin
   fFont.Assign( Value );
   Invalidate;
   Change;
end;

// ----------------------------------------------------------
procedure TKOLQProgressBar.InitBlockArray;
// fPosDescr[n] describes each possible position, storing :
// - wether it is in a block or not ;               <- drawing blocks instead of a continuous line
// - what is the block limit for this position;     <- (if full blocks only are to be drawn, then
//   only those which limit is bellow(H) above(V) current position will be drawn.)
// Computed on size/resize and blocks/space sizes changes only, to avoid computations at runTime.
var i,
    blkStart,
    blkStop : Integer;
begin
  if (fBlockSize = 0) or (fSpaceSize = 0) then Exit;

  if fUSefullDrawSpace <= 0
     then SetLength(fPosDescr, 1)                               // Position 0 is allways False
     else SetLength(fPosDescr, fUSefullDrawSpace+1);

  case ( fOrientation ) of
    boHorizontal :
      begin
        fPosDescr[0].isInBlock := False;
        blkStart := 3;
        blkStop  := blkStart + fBlockSize -1 ;
        for i := 1 to High(fPosDescr) do
        begin
          fPosDescr[i].isInBlock := (i >= blkStart) and (i <= blkStop);
          fPosDescr[i].blkLimit  := blkStop;
          if i = blkStop then
          begin
            blkStart := blkStop  + fSpaceSize + 1;
            blkStop  := blkStart + fBlockSize - 1;
            if blkStop > High(fPosDescr) then blkStop := High(fPosDescr);
          end;
        end;
      end; {boHrz}
    else                                                             // boVertical; "Else" avoids compiler warnings
      begin
        fPosDescr[High(fPosDescr)].isInBlock := False;
        blkStart := High(fPosDescr)-3;
        blkStop  := blkStart - fBlockSize + 1 ;
        for i := fUSefullDrawSpace downTo fBorderSize do
        begin
          fPosDescr[i].isInBlock := (i <= blkStart) and (i >= blkStop);
          fPosDescr[i].blkLimit  := blkStop;
          if i = blkStop then
          begin
            blkStart := blkStop  - fSpaceSize - 1;
            blkStop  := blkStart - fBlockSize + 1;
            if blkStop < fBorderSize then blkStop := fBorderSize;
          end;
        end;
      end; {boVert}
  end; {case}
end;

// ----------------------------------------------------------
function TKOLQProgressBar.RGBtoHLS( RGBColor: TColor ): THLSRec;              // NIH
// (c) Microsoft. http://support.microsoft.com/default.aspx?scid=kb;en-us;29240
// This is the translation of a Microsoft knowledge base article, pubilshed
// under number Q29240. Msft's knowledge base has a lot of interesting articles.

//(knowledge base = http://support.microsoft.com/default.aspx?scid=FH;EN-US;KBHOWTO)

var
   R, G, B: Integer;                        // input RGB values
   H, L, S: Integer;
   cMax, cMin: Byte;                        // max and min RGB values
   Rdelta,Gdelta,Bdelta: Integer;           // intermediate value: % of spread from max
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
     If (H > HLSMAX) then  H := H - HLSMAX;
  end;

  Result.Hue := H;
  Result.Lum := L;
  Result.Sat := S;
end;

// ----------------------------------------------------------
function TKOLQProgressBar.HLStoRGB( hue, lum, sat: THLSRange ): TColor;         // NIH
// (c) Microsoft. http://support.microsoft.com/default.aspx?scid=kb;en-us;29240
var
   R,G,B : Integer;                         // RGB component values
   Magic1,Magic2: Integer;                  // calculated magic numbers (really!)


   { -----------------  LOCAL  -----------------}

     function HueToRGB(n1,n2,hue: Integer): Integer;                // (c) Microsoft.
     // utility routine for HLStoRGB
     begin
       // range check: note values passed add/subtract thirds of range
       if      hue < 0      then Inc(hue, HLSMAX)
       else if hue > HLSMAX then Dec(hue, HLSMAX);

       (* return r,g, or b value from this tridrant *)
       if hue < (HLSMAX div 6)
       then result := ( n1 + ( ( (n2-n1) * hue + (HLSMAX div 12) ) div (HLSMAX div 6) ) )
       else if hue < (HLSMAX div 2)
            then result := n2
            else if hue < ( (HLSMAX*2) div 3 )
                 then result := ( n1 + ( ( (n2-n1) * ( ( (HLSMAX*2) div 3 ) - hue )
                                     + (HLSMAX div 12) ) div (HLSMAX div 6) ) )
                 else result := n1;
     end;{HueToRGB}

   { ----------------- \LOCAL\ -----------------}

begin
  if Sat = 0 then                           // achromatic case
  begin
    R :=(Lum*RGBMAX) div HLSMAX;
    G := R;
    B := R;
    if not(Hue = UNDEFINED) then
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
  Result :=  RGB(R,G,B);
end;

// ----------------------------------------------------------
function TKOLQProgressBar.MakeCylinder( h: Real ):Extended;                      // NIH
// (c) Matthieu Contensou (http://www25.brinkster.com/waypointfrance/cpulog/index.asp)
// who computed the polynome used to provide the "cylinder" appearence to bars :
// "f (h) = -4342,9 h^5 + 10543 h^4 - 8216 h^3 + 2018,1 h^2 + 11,096 h + 164,6"
// "h is the order of the wanted pixel in a column (horizontal bar), or in
// a row (vertical bar), with a value between 0 and 1 (0 -> 100%)"
begin
  Result := ( (-4342.9 * ( Power(h,5) ) )
          +   ( 10543  * ( Power(h,4) ) )
          -   ( 8216   * ( Power(h,3) ) )
          +   ( 2018.1 * ( Power(h,2) ) )
          +   ( 11.096 * h ) + 164.6  ) ;
end;

// ----------------------------------------------------------
function TKOLQProgressBar.GetColorBetween(StartColor, EndColor: TColor; Pointvalue,
                                       Von, Bis : Extended): TColor;                 // NIH
// Found on efg's colors pages, at http://homepages.borland.com/efg2lab/Library/Delphi/Graphics/Color.htm
// "Color gradient" row, cworn's UseNet Post.
// Author is unknown, but remains holder for intellectual property.
// High speed function which returns the gradient color value for a pixel depending
// on start and final color, size of the gradient area , and the place of the current pixel;

var F: Extended; r1, r2, r3, g1, g2, g3, b1, b2, b3: Byte;
  function CalcColorBytes(fb1, fb2: Byte): Byte;
  begin
    Result := fb1;
    if fb1 < fb2 then Result := FB1 + Trunc(F * (fb2 - fb1));
    if fb1 > fb2 then Result := FB1 - Trunc(F * (fb1 - fb2));
  end;
begin
  if ( (fMonoClr) or (Pointvalue <= Von) ) then
  begin
    Result := StartColor;
    Exit;
  end;
  if ( Pointvalue >= Bis ) then
  begin
    Result := EndColor;
    Exit;
  end;
  F := (Pointvalue - von) / (Bis - Von);
  asm
     mov EAX, Startcolor
     cmp EAX, EndColor
     je @@exit
     mov r1, AL
     shr EAX,8
     mov g1, AL
     shr Eax,8
     mov b1, AL
     mov Eax, Endcolor
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
     mov @Result, eax
  end;
end;

// ----------------------------------------------------------
procedure TKOLQProgressBar.InitPixArray;
// Compute and stores each pixel color, in the case of a gradient, or a double 
// gradient (both directions) in order to speed up things at run time.
var i, j,
    rowSz : integer;
    clr   : TColor;
    HLSr  : THLSRec;
begin

  case ( fOrientation ) of
    boHorizontal : rowSz := Height - (fBorderSize) + 1;
    else           rowSz := Width  - (fBorderSize) + 1;  // boVertical;
  end; {Case}

  if ( fUSefullDrawSpace <= 0 )
     then SetLength(fPixDescr, 1)                        // Position 0 is allways False
     else SetLength(fPixDescr, fUSefullDrawSpace + 1);

  // Populates active positions colors array ;
  // -> GetColorBetween works on the horizontal gradient, in the case of a 
  //    boHorizontal bar, with two colors (or on the vertical one, if the 
  //    bar is vertical).
  // -> GetGradientAr2  then returns the row gradient, based upon the header
  //    pixel value for that row in order to give the cylinder appearance.
  for i := 0 to fUSefullDrawSpace do
  begin
    clr := GetColorBetween(fStartColor, fFinalColor, (i), 0, fUSefullDrawSpace);
    if fBarKind = bkCylinder
       then fPixDescr[i] := GetGradientAr2( clr, rowSz )
       else for j := 0 to rowSz -1 do
            begin
              SetLength(fPixDescr[i],rowSz);
              fPixDescr[i,j] := clr;
            end;
  end;
  // inactive positions decription, used in case 'showInactive positions' is true;
  if ( Height - fBorderSize ) <= 0 Then
  begin
    SetLength( fInactDescr, 1 );
    fInactDescr[0] := fInactPosColor;
  end else
  begin
    if fBarKind = bkCylinder
    then fInactDescr := GetGradientAr2(fInactPosColor, rowSz )
    else
    begin
           SetLength(fInactDescr,rowSz);
           for j := 0 to rowSz -1 do
            fInactDescr[j] := fInactPosColor;
    end;
  end;
  // case cylindric bar : the background can be basically reversed.
  if (fBarKind = bkCylinder) and (fInvertInactPos) then
     for i := 0 to rowSz -1 do
     begin
       HLSr := RGBtoHLS(fInactDescr[i]);
       HLSr.lum := 240-HLSr.lum;
       fInactDescr[i] := HLStoRGB(HLSr.hue,HLSr.lum,HLSr.sat);
     end;
end;

// ----------------------------------------------------------
procedure TKOLQProgressBar.SetUsefullWidth;
begin
  case fOrientation of
    boHorizontal : fUSefullDrawSpace := (Width  - ( fBorderSize ));
    boVertical   : fUSefullDrawSpace := (Height - ( fBorderSize ));
  end;
  fMinVisPos := fBorderSize + 1;
end;

// ----------------------------------------------------------
function TKOLQProgressBar.GetGradientAr2( aColor: TColor; sz: Integer): TClrArray;
// Version corrected by Bernd Kirchhoff (http://home.germany.net/100-445474/)
// Returns an array of size sz, filled up with a basic gradient; Used to
// provide the "cylindric" appearance.
var i,RP: Integer;
    HLSr: THLSRec;
begin
  SetLength(result,sz);
  for i := 0 to sz - 1 do
  Begin
    HLSr := RGBtoHLS(aColor);
    // (c) Bernd Kirchhoff >>>--------------------------------------------------
    if self.fBarLook = blGlass then
      HLSr.lum := Round(MakeCylinder( (i / sz)) )
    else
    begin
      rp:= HLSr.lum - 212;
      rp:= rp+Trunc(MakeCylinder( i / sz));
      if ( rp < 0 ) then rp:=0;
      if ( rp > 240 ) then rp:=240;
      HLSr.lum := rp;
    end;
   // <<<-----------------------------------------------------------------------
    Result[i] := HLStoRGB(HLSr.hue,HLSr.lum,HLSr.sat);
  end;
end;

// ----------------------------------------------------------
procedure TKOLQProgressBar.Paint;
var i,k,sp: Integer;
    OldBkMode : Integer;
begin
  // -1- Bevel
  if ( fShaped ) then
  with Canvas do
  begin
    Pen.Width   := 1;
    Brush.Style := bsSolid;
    Brush.Color := fBackColor;
    Pen.Color   := fShapeColor;
    RoundRect (0, 0, Width, Height, fCorner, fCorner);
  end;

  // -2- The bar itself
  case fOrientation of
    boHorizontal :
      begin
        for i := (fBorderSize - 1) to fPosition  do
        begin
          if (fByBlock) then
          begin
            if (fPosDescr[i].isInBlock = true) then
            begin
              if  ( (fShowFullBlock) and (fPosition >= fPosDescr[i].blkLimit) )
              or ( not fShowFullBlock )
              then for k := (fBorderSize -1) to (Height -(fBorderSize))
                       do Canvas.Pixels [i,k] := fPixDescr[i,k]
              else if ( fShowInactPos ) then
                      for k := (fBorderSize -1) to (Height -(fBorderSize))
                          do Canvas.Pixels [i,k] := fInactDescr[k];
            end;
          end else
          begin
            for k := (fBorderSize -1) to (Height -(fBorderSize))  do
                Canvas.Pixels [i,k] := fPixDescr[i,k];
          end;
        end;
        // Now dealing with inactive positions, if they're to be drawn.
        if ( fShowInactPos ) then
        begin
          if fPosition < 3 then sp := 3 else sp := fPosition + 1;
          for i := sp to fUSefullDrawSpace do
          begin
            if (fByBlock) then
            begin
              if (fPosDescr[i].isInBlock = True) then
              begin
                 for k := (fBorderSize -1) to (Height -(fBorderSize)) do
                       Canvas.Pixels [i,k] := fInactDescr[k];
              end;
            end
            else  //If not(byBlock), all pixels must be drawn
            begin
              for k := (fBorderSize -1) to (self.Height -(fBorderSize))  do
                  Canvas.Pixels [i,k] := fInactDescr[k];
            end;
          end; {for}
        end; {inactive}
      end; {boHorizontal}
    boVertical :
      begin
        for i := (fUSefullDrawSpace-1) downto ( height - fPosition ) do
        begin
          if (fByBlock) then
          begin
            if (fPosDescr[i].isInBlock = true) then
            begin
              if  ( (fShowFullBlock) and ((height - fPosition) <= fPosDescr[i].blkLimit) )
              or not( fShowFullBlock )
              then for k := (fBorderSize - 1) to (self.Width -(fBorderSize))
                       do Canvas.Pixels [k,i] := fPixDescr[i,k]
              else if fShowInactPos then
                       for k := (fBorderSize - 1) to (Width -(fBorderSize))
                           do Canvas.Pixels [k,i] := fInactDescr[k];
            end;
          end
          else for k := (fBorderSize - 1) to (Width -(fBorderSize))
                       do Canvas.Pixels [k,i] := fPixDescr[i,k];
        end;
        // inactive positions :
        if ( fShowInactPos ) then
        begin
          if fPosition < 3 then sp := Self.fUSefullDrawSpace
          else sp := height - fPosition - 1;
          for i := sp downto fBorderSize do
          begin
            if ( fByBlock ) then
            begin
              if (fPosDescr[i].isInBlock = true) then
              begin
                 for k := (fBorderSize -1) to (Width -(fBorderSize)) do
                     Canvas.Pixels [k,i] := fInactDescr[k];
              end;
            end
            else
              for k := (fBorderSize -1) to (Width -(fBorderSize))
                  do Canvas.Pixels [k,i] := fInactDescr[k];
          end; {for... downto}
        end; {inactive}
      end; {boVertical}
    end; // Case

    // caption management. The font is the canvas' one. Can be overrided
    // using the Font property :
   if ( fAutoCaption ) then
       if ( fShowPosAsPct ) then SetCaption( FloatToStr(fUSerPosPct) + '%' )
       else SetCaption( IntToStr(fUSerPos) );

    if ( fHasCaption ) then
    begin
      if ( fParentFont ) then
       // attach KOLForm change events to this control
       PrepareCanvasFontForWYSIWIGPaint( Canvas )
      else
      with Canvas do
      begin
        // use control font instead of KOLForm's one.
        Font.Name:= fFont.FontName;
        Font.Height:= fFont.FontHeight;
        Font.Style:= fFont.FontStyle;
      end;
      with Canvas do
      begin
       // set transparent mode (actually for canvas font)
       OldBkMode := SetBkMode( Handle, Windows.TRANSPARENT );
       Font.Color:= fFont.Color;
       TextOut( fCapPos.X, fCapPos.Y, fCaption );
      end;
      SetBkMode(Canvas.Handle, OldBkMode);
    end;
end;

// ----------------------------------------------------------
procedure TKOLQProgressBar.SetupConstruct(SL: TStringList; const AName,
  AParent, Prefix: string);
var
 S: string;
begin
  S := GenerateTransparentInits;
  SL.Add( Prefix + AName + ' := PQProgressBar( New' + TypeName + '( '
          + SetupParams( AName, AParent ) + ' )' + S + ');' );
end;

// ----------------------------------------------------------
procedure TKOLQProgressBar.SetupFirst(SL: TStringList; const AName,
 AParent, Prefix: string);
var 
 FontStyle: string;
begin
  inherited;
  if ( fShowFullBlock ) then
   SL.Add( Prefix + AName + '.ShowFullBlock:= true;' );
  if ( fBlockSize <> 0 ) then
   SL.Add( Prefix + AName + '.BlockSize:= ' + IntToStr( BlockSize ) + ';' );
  if ( fSpaceSize <> 0 ) then
   SL.Add( Prefix + AName + '.SpaceSize:= ' + IntToStr( SpaceSize ) + ';' );
  if ( fOrientation <> boHorizontal ) then
   SL.Add( Prefix + AName + '.Orientation:= boVertical;' );
  if ( fBarKind <> bkFlat ) then
   SL.Add( Prefix + AName + '.BarKind:= bkCylinder;' );
  if ( fBarLook <> blMetal ) then
   SL.Add( Prefix + AName + '.BarLook:= blGlass;' );
  if ( fPosition <> 0 ) then
   SL.Add( Prefix + AName + '.Progress:= ' + IntToStr( Progress ) + ';' );
  if ( fMaximum <> 100 ) then
   SL.Add( Prefix + AName + '.MaxProgress:= ' + IntToStr( MaxProgress ) + ';' );
  if ( not Shaped ) then
   SL.Add( Prefix + AName + '.Shaped:= false;' );
  if ( fShapeColor <> $00743C00 ) then
   SL.Add( Prefix + AName + '.ShapeColor:= ' + Color2Str( ShapeColor ) + ';');
  if ( fStartColor <> clLime ) then
   SL.Add( Prefix + AName + '.StartColor:= ' + Color2Str( StartColor ) + ';');
  if ( fFinalColor <> clLime ) then
   SL.Add( Prefix + AName + '.FinalColor:= ' + Color2Str( FinalColor ) + ';');
  if ( fBackColor <> clWhite ) then
   SL.Add( Prefix + AName + '.BackgroundColor:= ' + Color2Str( BackgroundColor ) + ';');
  if ( fInactPosColor <> clGray ) then
   SL.Add( Prefix + AName + '.InactivePosColor:= ' + Color2Str( InactivePosColor ) + ';');
  if ( fShowInactPos ) then
   SL.Add( Prefix + AName + '.ShowInactivePos:= true;' );
  if ( fInvertInactPos ) then
   SL.Add( Prefix + AName + '.InvertInactPos:= true;' );
  if ( fAutoCaption ) then
   SL.Add( Prefix + AName + '.AutoCaption:= true;' )
  else
   if ( Caption <> '' ) then 
    SL.Add( Prefix + AName + '.Caption:= ''' + Caption + ''';');
  if ( fAutoHint ) then
   SL.Add( Prefix + AName + '.AutoHint:= true;' );
  if ( fShowPosAsPct ) then
   SL.Add( Prefix + AName + '.ShowPosAsPct:= true;' );
  if ( fHideOnTerm ) then
   SL.Add( Prefix + AName + '.HideOnTerminate:= true;' );
  if ( not fRoundCorner ) then
   SL.Add( Prefix + AName + '.RoundCorner:= false;' );
  if ( fCaptionAlign <> taLeft ) then
  begin
   if ( fCaptionAlign = taCenter ) then
    SL.Add( Prefix + AName + '.CaptionAlign:= taCenter;' );
   if ( fCaptionAlign = taRight ) then
    SL.Add( Prefix + AName + '.CaptionAlign:= taRight;' );
  end;

 if ( not fParentFont ) then
 begin
  SL.Add( Prefix + AName + '.Font.FontHeight:= ' + Int2Str( fFont.FontHeight ) + ';' );
  SL.Add( Prefix + AName + '.Font.FontName:= ''' + fFont.FontName + ''';' );
  if ( fFont.FontWeight <> 0 ) then
   SL.Add( Prefix + AName + '.Font.FontWeight:= '+ Int2Str( fFont.FontWeight ) + ';' );
  if ( fFont.FontWidth <> 0 ) then
   SL.Add( Prefix + AName + '.Font.FontWidth:= '+ Int2Str( fFont.FontWidth ) + ';' );
  if ( fFont.Color <> clWindowText ) then
   SL.Add( Prefix + AName + '.Font.Color:= '+ Color2Str( fFont.Color ) + ';');

  FontStyle := '';
  if ( fsBold in fFont.FontStyle ) then FontStyle := FontStyle + 'fsBold, ';
  if ( fsItalic in fFont.FontStyle ) then FontStyle := FontStyle + 'fsItalic, ';
  if ( fsUnderline in fFont.FontStyle ) then FontStyle := FontStyle + 'fsUnderline, ';
  if ( fsStrikeOut in fFont.FontStyle ) then FontStyle := FontStyle + 'fsStrikeOut, ';

  if ( Length( FontStyle ) > 0 ) then
  begin
   SetLength( FontStyle, Length( FontStyle ) - 2 );
   SL.Add( Prefix + AName + '.Font.FontStyle := [' + FontStyle + '];');
  end;
  
 end;
end;

// ----------------------------------------------------------
function TKOLQProgressBar.SetupParams( const AName, AParent: string ): string;
begin
  Result:= AParent;
end;

// ----------------------------------------------------------
procedure TKOLQProgressBar.AssignEvents(SL: TStringList; const AName: string);
begin
  inherited;
  DoAssignEvents( SL, AName, [ 'OnProgressChange' ], [ @OnProgressChange ]);
end;

// ----------------------------------------------------------
procedure TKOLQProgressBar.SetOrientation( Value: TQBarOrientation );
var tmpClr: TColor;
    newH,
    newW: Integer;
begin
  if ( value <> fOrientation ) then
  begin
    if ( (value = boVertical)   and (Height < width)  )
    or ( (value = boHorizontal) and (width  < height) ) then
    begin
       newW := Height;
       newH := Width;
       Height := newH;
       Width  := newW;
    end;
    fOrientation := value;
    if  ( csDesigning in componentState ) then
     begin
      tmpClr    := fStartColor;
      fStartColor := fFinalColor;
      fFinalColor := tmpClr;
    end;
  end;
  case ( value ) of
    boHorizontal : if Height < 10
                      then fInternalBorder := 1
                      else fInternalBorder := 2;
    boVertical   : if Width  < 10
                      then fInternalBorder := 1
                      else fInternalBorder := 2;
  end; // Case
  fBorderSize := fInternalBorder shl 1;
  SetUsefullWidth;
  InitBlockArray;
  InitPixArray;
  Progress:= fUserPos;
  Invalidate;
  Change;
end;

// ----------------------------------------------------------
procedure TKOLQProgressBar.SetBarKind( Value: TQBarKind );
begin
  fBarKind:= Value;
  InitPixArray;
  Invalidate;
  Change;
end;

// ----------------------------------------------------------
procedure TKOLQProgressBar.SetBarLook( Value: TQBarLook);
begin
  fBarLook:= Value;
  InitPixArray;
  Invalidate;
  Change;
end;

// ----------------------------------------------------------
procedure TKOLQProgressBar.SetRoundCorner( Value: Boolean );
begin
  fRoundCorner:= Value;
  if ( Value ) then fCorner:= 5
  else fCorner:= 0;
  Invalidate;
  Change;
end;

// ----------------------------------------------------------
procedure TKOLQProgressBar.SetBackColor( Value: TColor );
begin
  fBackColor:= Value;
  Invalidate;
  Change;
end;

// ----------------------------------------------------------
procedure TKOLQProgressBar.SetBarColor( Value: TColor );
begin
  fMonoClr  := True;
  fBarColor:= Value;
  fStartColor:= Value;
  fFinalColor:= Value;
  InitPixArray;
  Invalidate;
  Change;
end;

// ----------------------------------------------------------
procedure TKOLQProgressBar.SetStartColor( Value: TColor );
begin
  fStartColor:= Value;
  fMonoClr  := (fStartColor = fFinalColor);
  InitPixArray;
  Invalidate;
  Change;
end;

// ----------------------------------------------------------
procedure TKOLQProgressBar.SetFinalColor(Value: TColor );
begin
  fFinalColor:= Value;
  fMonoClr  := (fStartColor = fFinalColor);
  InitPixArray;
  Invalidate;
  Change;
end;

// ----------------------------------------------------------
procedure TKOLQProgressBar.SetShowInactPos( Value: Boolean );
begin
  fShowInactPos:= Value;
  InitPixArray;
  Invalidate;
  Change;
end;

// ----------------------------------------------------------
procedure TKOLQProgressBar.SetInvertInactPos( Value: Boolean );
begin
  fInvertInactPos:= Value;
  InitPixArray;
  Invalidate;
  Change;
end;

// ----------------------------------------------------------
procedure TKOLQProgressBar.SetInactPosColor( Value: TColor );
begin
  fInactPosColor:= Value;
  InitPixArray;
  Invalidate;
  Change;
end;

// ----------------------------------------------------------
procedure TKOLQProgressBar.SetShaped( Value: Boolean );
begin
  fShaped:= Value;
  Invalidate;
  Change;
end;

// ----------------------------------------------------------
procedure TKOLQProgressBar.SetShapeColor( Value: TColor );
begin
  fShapeColor:= Value;
  Invalidate;
  Change;
end;

// ----------------------------------------------------------
procedure TKOLQProgressBar.SetBlockSize( Value: Integer );
begin
  case ( fOrientation ) of
    boHorizontal : if value > Width  - (fInternalBorder SHL 1) then Exit;
    boVertical   : if value > Height - (fInternalBorder SHL 1) then Exit;
  end; {case}

  fBlockSize := Abs(value);
  fByBlock   := (fBlockSize > 0) And (fSpaceSize > 0);
  if ( fByBlock ) then InitBlockArray;
  InitPixArray;
  Invalidate;
  Change;
end;

// ----------------------------------------------------------
procedure TKOLQProgressBar.SetSpaceSize( Value: Integer );
begin
  case ( fOrientation ) of
    boHorizontal : if value > Self.Width  - (fInternalBorder shl 1) then Exit;
    boVertical   : if value > Self.Height - (fInternalBorder shl 1) then Exit;
  end; {case}

  fSpaceSize := Abs(value);
  fByBlock   := (fBlockSize > 0) and (fSpaceSize > 0);
  if ( fByBlock ) then InitBlockArray;
  InitPixArray;
  Invalidate;
  Change;
end;

// ----------------------------------------------------------
procedure TKOLQProgressBar.SetShowFullBlock( Value: Boolean );
begin
  fShowFullBlock := value;
  InitBlockArray;
  InitPixArray;
  Invalidate;
  Change;
end;

// ----------------------------------------------------------
procedure TKOLQProgressBar.SetMaximum( Value: Integer );
begin
  fMaximum:= Value;
  SetPosition( fUserPos );
  Invalidate;
  Change;
end;

// ----------------------------------------------------------
procedure TKOLQProgressBar.SetPosition( Value: Integer);
var tmpfPos : real;
begin
  fUserPos := value;
  if ( fMaximum = 0 ) then exit;
  try
    if (value <= 0) Then
    begin
      fPosition := 0;
      Exit;
    end
    else
    if ( value > fMaximum ) then value := fMaximum;

    fUSerPosPct := (100 * value)/fMaximum;
    tmpfPos := fUsefullDrawSpace * fUSerPosPct / 100;
    // If value( user position) > 0, make sure that at least one bar is visible
    if (tmpfPos > 0.00) and (tmpfPos < fMinVisPos )
       then fPosition := fMinVisPos
       else if ( tmpfPos  > fUsefullDrawSpace )
            then fPosition := fUsefullDrawSpace
            else fPosition := Round( tmpfPos );
  finally
    Invalidate;
  end;
  Change;
end;

// ----------------------------------------------------------
procedure TKOLQProgressBar.SetHideOnTerm( Value: Boolean );
begin
  fHideOnTerm:= Value;
  Change;
end;

// ----------------------------------------------------------
procedure TKOLQProgressBar.SetCaptionAlign( Value: TTextAlign );
begin
  fCaptionAlign:= Value;
  SetCaption( fCaption );
  Invalidate;
  Change;
end;

// ----------------------------------------------------------
procedure TKOLQProgressBar.SetAutoCaption( Value: Boolean );
begin
  fAutoCaption:= Value;
  Invalidate;
  Change;
end;

// ----------------------------------------------------------
procedure TKOLQProgressBar.SetAutoHint( Value: Boolean );
begin
  fAutoHint:= Value;
  Invalidate;
  Change;
end;

// ----------------------------------------------------------
procedure TKOLQProgressBar.SetShowPosAsPct( Value: Boolean );
begin
  fShowPosAsPct:= Value;
  Invalidate;
  Change;
end;

// ----------------------------------------------------------
procedure TKOLQProgressBar.SetOnProgressChange( Value: TOnQProgressBar );
begin
  fOnProgressChange:= Value;
  Change;
end;

// ----------------------------------------------------------


end.
