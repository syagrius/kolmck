unit ToGRush;

interface

//{$DEFINE TOGRUSH_AUTO_DISIMAGES} // add this symbol to provide 256 gray images
                                   // based on original ones for Disabled state
                                   // of toolbar buttons
//{$DEFINE TOGRUSH_AUTO_DIS_EQ} // RGB channels of the same level while TOGRUSH_AUTO_DISIMAGES
//{$DEFINE TOGRUSH_DROPBTN2} // Drop button will be placed right to the button
                             // having property DropDown, not in the button
//{$DEFINE TOGRUSH_NO_AUTO_SIZE_BTNS} // not use AutoSize for buttons
           // (sensible only in a case, when only images are in the toolbar)

uses Windows, KOL, KOLGRushControls;

function NewButton( AParent: PControl; const Caption: KOLString ): PControl;

function NewCheckbox( AParent: PControl; const Caption: KOLString ): PControl;
function NewRadiobox( AParent: PControl; const Caption: KOLString ): PControl;

function NewPanel( AParent: PControl; EdgeStyle: TEdgeStyle ): PControl;

function NewSplitter( AParent: PControl; MinSizePrev, MinSizeNext: Integer ): PControl;
function NewSplitterEx( AParent: PControl; MinSizePrev, MinSizeNext: Integer;
         EdgeStyle: TEdgeStyle ): PControl;


function NewToolbar( AParent: PControl; Align: TControlAlign; Options: TToolbarOptions;
                     Bitmap: HBitmap; const Buttons: array of PChar;
                     const BtnImgIdxArray: array of Integer ) : PControl;

function ToolbarButtonRect( Toolbar: PControl; BtnID: Integer ): TRect;
procedure ToolbarSetTooltips( Toolbar: PControl; BtnID1st: Integer; const Tooltips: array of PKOLChar );
function ToolbarButtonEnabled( Toolbar: PControl; BtnID: Integer ): Boolean;
procedure EnableToolbarButton( Toolbar: PControl; BtnID: Integer; Enable: Boolean );
function ToolbarButtonVisible( Toolbar: PControl; BtnID: Integer ): Boolean;
procedure ShowHideToolbarButton( Toolbar: PControl; BtnID: Integer; Show: Boolean );

function NewProgressbar( AParent: PControl ): PControl;
function NewProgressbarEx( AParent: PControl; Options: TProgressbarOptions ): PControl;

implementation

const
  IS_DRDWN = 16;
type
  PControl_ = ^TControl_;
  TControl_ = object( TControl )
  end;

////////////////////////////////////////////////////////////////////////////////
// BUTTON, CHECK, RADIO CHECK
////////////////////////////////////////////////////////////////////////////////
function NewButton( AParent: PControl; const Caption: KOLString ): PControl;
begin
  Result := Pointer( NewGRushButton( AParent, Caption ).SetSize( 64, 22 ) );
  PControl_( Result ).fIsButton := TRUE;
end;

function NewCheckbox( AParent: PControl; const Caption: KOLString ): PControl;
begin
  Result := Pointer( NewGRushCheckBox( AParent, Caption ).SetSize( 64, 22 ) );
end;

function NewRadiobox( AParent: PControl; const Caption: KOLString ): PControl;
begin
  Result := Pointer( NewGRushRadioBox( AParent, Caption ).SetSize( 64, 22 ) );
end;

////////////////////////////////////////////////////////////////////////////////
// PANEL
////////////////////////////////////////////////////////////////////////////////
function NewPanel( AParent: PControl; EdgeStyle: TEdgeStyle ): PControl;
begin
  if EdgeStyle = esTransparent then
  begin
    Result := KOL.NewPanel( AParent, esNone ).SetSize( 64, 64 );
    Result.Transparent := TRUE;
  end
  else
    Result := Pointer( NewGRushPanel( AParent ) );
end;

////////////////////////////////////////////////////////////////////////////////
// SPLITTER
////////////////////////////////////////////////////////////////////////////////
function NewSplitter( AParent: PControl; MinSizePrev, MinSizeNext: Integer ): PControl;
begin
  Result := Pointer( NewGRushSplitter( AParent, MinSizePrev, MinSizeNext ) );
  Result.Transparent := TRUE;
end;

function NewSplitterEx( AParent: PControl; MinSizePrev, MinSizeNext: Integer;
         EdgeStyle: TEdgeStyle ): PControl;
begin
  Result := Pointer( NewGRushSplitter( AParent, MinSizePrev, MinSizeNext ) );
  Result.Transparent := TRUE;
end;

////////////////////////////////////////////////////////////////////////////////
// TOOL BAR
////////////////////////////////////////////////////////////////////////////////
type
  TTBButtonEvent = packed Record
    BtnID: DWORD;
    Event: TOnToolbarButtonClick;
  end;
  PTBButtonEvent = ^TTBButtonEvent;
procedure ToGR_ClickToolbarBtn( Dummy, Sender: PControl );
var D: DWORD;
    Idx: Integer;
    IsCheck, Checked: Boolean;
    Toolbar: PControl_;
    EventRec: PTBButtonEvent;
begin
  D := GetProp( Sender.Handle, 'GRBTN' );
  Idx := LoWord( D );
  IsCheck := HiWord( D ) and 2 <> 0;
  Checked := HiWord( D ) and 4 <> 0;
  if IsCheck then
  begin
    Checked := not Checked;
    D := D xor $40000;
    SetProp( Sender.Handle, 'GRBTN', D );
    PGrushControl( Sender ).Checked := Checked;
  end;
  Toolbar := Pointer( Sender.Parent );
  if Assigned( Toolbar.fTBEvents ) and
     (Toolbar.fTBevents.Count > Idx) then
  begin
    EventRec := Toolbar.fTBevents.Items[ Idx ];
    if Assigned( EventRec.Event ) then
      EventRec.Event( Toolbar, EventRec.BtnID );
  end
    else
  if Assigned( Toolbar.fOnClick ) then
  begin
    Toolbar.fOnClick( Toolbar );
  end;
end;

procedure ToGR_ClickToolbarBtnDD( Dummy, Sender: PControl );
var D: DWORD;
    Idx: Integer;
    Toolbar: PControl_;
    EventRec: PTBButtonEvent;
begin
  D := GetProp( Sender.Handle, 'GRBTN' );
  Idx := LoWord( D );
  Toolbar := Pointer( Sender.Parent );
  {$IFDEF TOGRUSH_DROPBTN2}
  {$ELSE}
  Toolbar := Pointer( Toolbar.Parent );
  {$ENDIF}
  Toolbar.fCurItem := Idx;
  Toolbar.fCurIndex := Idx;
  Toolbar.fDropped := TRUE;
  if Assigned( Toolbar.fTBevents ) and
     (Toolbar.fTBevents.Count > Idx) then
  begin
    EventRec := Toolbar.fTBevents.Items[ Idx ];
    Toolbar.fCurItem := EventRec.BtnID;
  end;
  if Assigned( Toolbar.OnTBDropDown ) then
  begin
    Toolbar.OnTBDropDown( Toolbar );
  end
    else
  if Assigned( Toolbar.fOnClick ) then
  begin
    Toolbar.fOnClick( Toolbar );
  end;
  Toolbar.fDropped := FALSE;
end;

procedure Provide_DIS_images( var B: PBitmap );
var B2: PBitmap;
    y, y_to, x, c: Integer;
    Src, Dst: PRGBQuad;
    first_pixel: Boolean;
    Transp: DWORD;
begin
  if (B =nil) or B.Empty then Exit;
  B2 := NewDIBBitmap( B.Width, B.Height * 2, pf32bit );
  TRY
    B.Draw( B2.Canvas.Handle, 0, 0 );
    y_to := B.Height;
    first_pixel := TRUE;
    Transp := 0;
    for y := 0 to B.Height-1 do
    begin
      Src := B2.ScanLine[ y ];
      Dst := B2.ScanLine[ y_to ];
      for x := 0 to B2.Width-1 do
      begin
        if first_pixel then
          Transp := PDWORD( Src )^ and $FFFFFF;
        first_pixel := FALSE;
        if PDWORD( Src )^ and $FFFFFF = Transp then
          PDWORD( Dst )^ := Transp
        else
        begin
          {$IFDEF TOGRUSH_AUTO_DIS_BAL}
          c := (Src.rgbRed * 64 + Src.rgbGreen * 128 + Src.rgbBlue * (128 + 64))
               div 256;
          {$ELSE}
          c := (Src.rgbRed * 64 + Src.rgbGreen * 64 + Src.rgbBlue * 64)
               div 100;
          {$ENDIF}
          if c > 255 then c := 255;
          Dst.rgbBlue := c;
          Dst.rgbGreen := c;
          Dst.rgbRed := c;
        end;
        inc( Src );
        inc( Dst );
      end;
      inc( y_to );
    end;
  FINALLY
    B.Assign( B2 );
    B2.Free;
  END;
end;

var DrDownBmp: PBitmap;
function NewToolbar( AParent: PControl; Align: TControlAlign; Options: TToolbarOptions;
                     Bitmap: HBitmap; const Buttons: array of PChar;
                     const BtnImgIdxArray: array of Integer ) : PControl;
var i, BtnID: Integer;
    B, B2: PGRushControl;
    C: String;
    IsSep: Boolean;
    IsDropDown: Boolean;
    IsCheck, Checked, IsRadio: Boolean;
    Idx: Integer;
    D: DWORD;
    imgW, imgH, W, H: Integer;
    Bmp: PBitmap;
    DD_dst: PByte;
    y: Integer;
    ES: TEdgeStyle;
const DD_img: array[ 0..6 ] of Byte = ( $0, $F8, $F8, $70, $70, $20, $20 );
begin
  if Align = caNone then Align := caTop;
  H := 0;
  imgW := 0;
  imgH := 0;
  Bmp := nil;
  if Bitmap <> 0 then
  begin
    Bmp := NewBitmap( 0, 0 );
    Bmp.Handle := Bitmap;
    imgH := Bmp.Height;
    imgW := imgH;
    H := Bmp.Height + 12;
    //Bmp.PixelFormat := pf32bit;
    //Bmp.SaveToFile( GetStartDir + 'test_toolbar1.bmp' );
    {$IFDEF TOGRUSH_AUTO_DISIMAGES}
    Provide_DIS_images( Bmp );
    {$ENDIF}
  end;
  ES := esNone;
  if [tboTransparent, tboFlat] * Options <> [] then
    ES := esTransparent;
  Result := Pointer( NewPanel( AParent, ES ).SetSize( 0, H ).SetAlign(Align) );
  //if Bmp <> nil then Result.Add2AutoFree( Bmp );
  Idx := -1;
  for i := 0 to High( Buttons ) do
  begin
    C := Buttons[ i ];
    IsSep := C = '-';
    IsDropDown := FALSE;
    IsCheck := FALSE;
    Checked := FALSE;
    BtnID := i; //ToolbarsIDcmd; inc( ToolbarsIDcmd );
    if IsSep then C := ''
    else
    begin
      Inc( Idx );
      IsDropDown := (C <> '') and (C[ 1 ] = '^');
      if IsDropDown then Delete( C, 1, 1 );
      IsCheck := (C <> '') and (C[ 1 ] in [ '+', '-' ]);
      if IsCheck then
      begin
        Checked := C[ 1 ] = '+';
        Delete( C, 1, 1 );
        IsRadio := (C <> '') and (C[ 1 ] = '!');
        if IsRadio then Delete( C, 1, 1 );
      end;
    end;
    if Trim( C ) = '' then C := '';
    if IsSep then
      NewPanel( Result, esTransparent ).SetSize( 6, 0 ).SetAlign( caLeft )
    else
    begin
      if C = '' then
      begin
        W := 32;
        if H <> 0 then W := H;
      end
        else
      begin
        W := 64;
      end;
      B := Pointer( NewButton( Result, C ).SetSize( W, 0 ).SetAlign( caLeft ) );
      {$IFDEF USE_NAMES}
      //B.Name := 'TB' + Int2Str( Idx+1 );
      {$ENDIF USE_NAMES}
      B.Tabstop := FALSE;
      B.LikeSpeedButton;
      B.Transparent := TRUE;
      if IsSep then B.Enabled := FALSE;
      if B.GetWindowHandle <> 0 then
      begin
        D := i   or Integer( IsSep ) shl 16
                 or Integer( IsCheck ) shl 17
                 or Integer( Checked ) shl 18
                 or Integer( IsDropDown ) shl 19
                 ;
        SetProp( B.Handle, 'GRBTN', D );
      end;
      SetProp( B.Handle, 'BTNID', BtnID );
      B.OnClick := TOnEvent( MakeMethod( nil, @ ToGR_ClickToolbarBtn ) );
      if Bmp <> nil then
      begin
        B.All_GlyphItemX := idx;
        B.All_GlyphItemY := 0;
        B.All_GlyphBitmap := Bmp;
        B.All_GlyphWidth := ImgW;
        B.All_GlyphHeight := ImgH;
        //B.All_GlyphAttached := TRUE;
        {$IFDEF TOGRUSH_AUTO_DISIMAGES}
        B.Dis_GlyphItemX := idx;
        B.Dis_GlyphItemY := 1;
        B.All_GlyphBitmap := Bmp;
        B.All_GlyphWidth := ImgW;
        B.All_GlyphHeight := ImgH;
        {$ENDIF}
        if not IsDropDown and (C = '') then
          B.All_GlyphHAlign := haCenter;
      end;
      {$IFNDEF TOGRUSH_NO_AUTO_SIZE_BTNS}
        B.fCommandActions.aAutoSzX := 10 + ImgW;
        if ImgW > 0 then inc( B.fCommandActions.aAutoSzX, 5 );
        if IsDropDown then inc( B.fCommandActions.aAutoSzX, 10 );
        B.AutoSize( TRUE );
      {$ENDIF}
      if IsDropDown then
      begin
        {$IFDEF TOGRUSH_DROPBTN2}
        B2 := Pointer( NewButton( Result, C ).SetSize( 5 + 8, 0 ).SetAlign( caLeft ) );
        {$ELSE}
        //B.AutoSize( FALSE );
        //B.Width := W + 13;
        B.All_TextHAlign := haLeft;
        B.Border := 2;
        B2 := Pointer( NewButton( B, C ).SetSize( 5 + 8, 0 ).SetAlign( caRight ) );
        {$ENDIF}
        {$IFDEF USE_NAMES}
        //B2.Name := 'TB_dd' + Int2Str( Idx+1 );
        {$ENDIF USE_NAMES}
        B2.Tabstop := FALSE;
        B2.LikeSpeedButton;
        B2.Transparent := TRUE;
        PGrushControl( B2 ).All_BorderWidth := 0;
        PGrushControl( B2 ).Over_BorderWidth := 1;
        if B2.GetWindowHandle <> 0 then
        begin
          D := i   or Integer( IsSep ) shl 16
                   or Integer( IsCheck ) shl 17
                   or Integer( Checked ) shl 18
                   or Integer( IsDropDown ) shl 19
                   or IS_DRDWN shl 16;
          SetProp( B2.Handle, 'GRBTN', D );
        end;
        B2.OnClick := TOnEvent( MakeMethod( nil, @ ToGR_ClickToolbarBtnDD ) );
        if DrDownBmp = nil then
        begin
          DrDownBmp := NewDIBBitmap( 5, High( DD_img )+1, pf1bit );
          DrDownBmp.DIBPalEntries[ 0 ] := $686868;
          DrDownBmp.DIBPalEntries[ 1 ] := $FFFFFF;
          for y := 0 to High( DD_img ) do
          begin
            DD_dst := DrDownBmp.ScanLine[ y ];
            DD_dst^ := not DD_img[ y ];
          end;
          //B2.All_GlyphItemX := 0;
          //B.All_GlyphItemY := 0;
          B2.All_GlyphBitmap := DrDownBmp;
          DrDownBmp.RefDec;
        end
          else
        begin
          B2.All_GlyphBitmap := DrDownBmp;
        end;
        B2.All_GlyphWidth := 5;
        B2.All_GlyphHeight := High( DD_img )+1;
        B2.All_GlyphHAlign := haCenter;
        B2.All_GlyphVAlign := vaBottom;
      end;
    end;
  end;
  if Bmp <> nil then
  begin
    Bmp.Free;
  end;
end;

function ToolbarButtonRect( Toolbar: PControl; BtnID: Integer ): TRect;
var i: Integer;
    B: PControl;
begin
  for i := 0 to Toolbar.ChildCount-1 do
  begin
    B := Toolbar.Children[ i ];
    if (B.GetWindowHandle <> 0) and
       (Integer( GetProp( B.Handle, 'BTNID' ) ) = BtnID) then
    begin
      Result := B.BoundsRect;
      Exit;
    end;
  end;
  Result := MakeRect( 0, 0, 0, 0 );
end;

procedure ToolbarSetTooltips( Toolbar: PControl; BtnID1st: Integer; const Tooltips: array of PKOLChar );
{$IFDEF USE_MHTOOLTIP}
var i, j: Integer;
    B: PControl;
    found: Boolean;
{$ENDIF}
begin
  Toolbar.TBSetTooltips( BtnID1st, Tooltips );
  {$IFDEF USE_MHTOOLTIP}
  found := FALSE;
  j := 0;
  if BtnID1st < 0 then BtnID1st := 0;
  for i := 0 to Toolbar.ChildCount-1 do
  begin
    if j > High( Tooltips ) then break;
    B := Toolbar.Children[ i ];
    //if not B.IsButton then continue;
    if HiWord( GetProp( B.Handle, 'GRBTN' ) ) and IS_DRDWN <> 0 then
      continue;
    if found or (Integer( GetProp( B.Handle, 'BTNID' ) ) = BtnID1st) then
    begin
      found := TRUE;
      if Trim( Tooltips[ j ] ) <> '' then
        NewHint( B ).Text := Tooltips[ j ];
      inc( BtnID1st );
      inc( j );
    end;
  end;
  {$ENDIF USE_MHTOOLTIP}
end;

function ToolbarButtonEnabled( Toolbar: PControl; BtnID: Integer ): Boolean;
var i: Integer;
    B: PControl;
begin
  for i := 0 to Toolbar.ChildCount-1 do
  begin
    B := Toolbar.Children[ i ];
    if (B.GetWindowHandle <> 0) and
       (Integer( GetProp( B.Handle, 'BTNID' ) ) = BtnID) then
    begin
      Result := B.Enabled;
      Exit;
    end;
  end;
  Result := FALSE;
end;

procedure EnableToolbarButton( Toolbar: PControl; BtnID: Integer; Enable: Boolean );
var i: Integer;
    B: PControl;
begin
  for i := 0 to Toolbar.ChildCount-1 do
  begin
    B := Toolbar.Children[ i ];
    if (B.GetWindowHandle <> 0) and
       (Integer( GetProp( B.Handle, 'BTNID' ) ) = BtnID) then
    begin
      B.Enabled := Enable;
      Exit;
    end;
  end;
end;

function ToolbarButtonVisible( Toolbar: PControl; BtnID: Integer ): Boolean;
var i: Integer;
    B: PControl;
begin
  for i := 0 to Toolbar.ChildCount-1 do
  begin
    B := Toolbar.Children[ i ];
    if (B.GetWindowHandle <> 0) and
       (Integer( GetProp( B.Handle, 'BTNID' ) ) = BtnID) then
    begin
      Result := B.Visible;
      Exit;
    end;
  end;
  Result := FALSE;
end;

procedure ShowHideToolbarButton( Toolbar: PControl; BtnID: Integer; Show: Boolean );
var i: Integer;
    B: PControl;
begin
  for i := 0 to Toolbar.ChildCount-1 do
  begin
    B := Toolbar.Children[ i ];
    if (B.GetWindowHandle <> 0) and
       (Integer( GetProp( B.Handle, 'BTNID' ) ) = BtnID) then
    begin
      B.Visible := Show;
      Exit;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// PROGRESS BAR
////////////////////////////////////////////////////////////////////////////////
function NewProgressbar( AParent: PControl ): PControl;
begin
  Result := Pointer( NewGRushProgressBar( AParent ).SetSize( 300, 20 ) );
end;

function NewProgressbarEx( AParent: PControl; Options: TProgressbarOptions ): PControl;
begin
  Result := NewProgressbar( AParent );
end;

end.
