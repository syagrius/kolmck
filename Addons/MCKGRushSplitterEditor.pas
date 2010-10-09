unit MCKGRushSplitterEditor;

//  file: MCKGRushSplitterEditor.pas
//  file version: 0.35
//  last modified: 06.02.06
//  package: GRushControls
//  author: Karpinskyj Alexandr aka homm
//      mailto: homm86@mail.ru
//      My humble Web-Page: http://www.homm86.narod.ru

{$I KOLDEF.INC}

interface

uses    Windows,
        Messages,
        ShellAPI,
        MCKGRushControls,
        tinyJPGGIFBMP,
        tinyPNG,
        mirror,
        Classes,
        Controls,
        mckObjs,
        Graphics,
        mckCtrls,
        Forms,
        KOL,
        KOLGRushControls,
        {$IFDEF _D6orHigher}
        DesignEditors,
        DesignIntf;
        {$ELSE}
        DsgnIntf;
        {$ENDIF}

type
    TSplitterStylesProp = class(TClassProperty)
    public
        procedure Edit; override;
        function GetAttributes: TPropertyAttributes; override;
    end;

    {$I MCKfakeClasses.inc}
    PSplitterEditor = ^TSplitterEditor;
    TSplitterEditor = object(TObj)
        Form: PControl;
        GRushImageCollection1: TKOLGRushImageCollection;
        CD1: TKOLColorDialog;
        ButtonOK: TKOLGRushButton;
        ButtonCancel: TKOLGRushButton;
        LabelState: TKOLLabel;
        StatesList: TKOLComboBox;
        Down1: TKOLGRushButton;
        Down2: TKOLGRushButton;
        Down3: TKOLGRushButton;
        Down4: TKOLGRushButton;
        Down5: TKOLGRushButton;
        Down6: TKOLGRushButton;
        Down7: TKOLGRushButton;
        GRushPanel1: TKOLGRushPanel;
        CropTopFirst: TKOLGRushCheckBox;
        AntiAliasing: TKOLGRushCheckBox;
        DrawGlyph: TKOLGRushCheckBox;
        DrawText: TKOLGRushCheckBox;
        GlyphAttached: TKOLGRushCheckBox;
        Label22: TKOLLabel;
        GlyphWidth: TKOLEditBox;
        Label23: TKOLLabel;
        Label24: TKOLLabel;
        GlyphHeight: TKOLEditBox;
        Label25: TKOLLabel;
        UpdateSpeed: TKOLComboBox;
        Label26: TKOLLabel;
        Label27: TKOLLabel;
        Label28: TKOLLabel;
        GlyphHorz: TKOLComboBox;
        GlyphVert: TKOLComboBox;
        Label29: TKOLLabel;
        Label30: TKOLLabel;
        TextHorz: TKOLComboBox;
        Label31: TKOLLabel;
        TextVert: TKOLComboBox;
        GRushButton11: TKOLGRushButton;
        GRushButton12: TKOLGRushButton;
        GRushButton13: TKOLGRushButton;
        Label16: TKOLLabel;
        L: TKOLEditBox;
        Label18: TKOLLabel;
        GRushButton16: TKOLGRushButton;
        Label17: TKOLLabel;
        T: TKOLEditBox;
        Label19: TKOLLabel;
        R: TKOLEditBox;
        Label20: TKOLLabel;
        B: TKOLEditBox;
        Label21: TKOLLabel;
        Spacing: TKOLEditBox;
        GRushButton17: TKOLGRushButton;
        GRushPanel2: TKOLGRushPanel;
        Label1: TKOLLabel;
        Label2: TKOLLabel;
        Label3: TKOLLabel;
        Label4: TKOLLabel;
        Label5: TKOLLabel;
        Label6: TKOLLabel;
        Label7: TKOLLabel;
        GradStyles: TKOLComboBox;
        Label8: TKOLLabel;
        Label9: TKOLLabel;
        Label11: TKOLLabel;
        Label12: TKOLLabel;
        Label13: TKOLLabel;
        Label14: TKOLLabel;
        BorderWi: TKOLEditBox;
        BorderHe: TKOLEditBox;
        Label10: TKOLLabel;
        GlyphX: TKOLEditBox;
        Label15: TKOLLabel;
        GlyphY: TKOLEditBox;
        Col1: TKOLLabel;
        Col2: TKOLLabel;
        Col3: TKOLLabel;
        Col4: TKOLLabel;
        Col5: TKOLLabel;
        Col6: TKOLLabel;
        BorderWidth: TKOLEditBox;
        ShadowOffset: TKOLEditBox;
        GRushButton1: TKOLGRushButton;
        GRushButton2: TKOLGRushButton;
        GRushButton3: TKOLGRushButton;
        GRushButton4: TKOLGRushButton;
        GRushButton5: TKOLGRushButton;
        GRushButton6: TKOLGRushButton;
        GRushButton7: TKOLGRushButton;
        GRushButton8: TKOLGRushButton;
        GRushButton9: TKOLGRushButton;
        GRushButton10: TKOLGRushButton;
        GRushButton14: TKOLGRushButton;
        GRushPanel3: TKOLGRushPanel;
        Control: TKOLGRushButton;
        CheckEnabled: TKOLGRushCheckBox;
        CheckTransparent: TKOLGRushCheckBox;
        Caption: TKOLEditBox;
        GRushButton18: TKOLGRushButton;
        GRushButton19: TKOLGRushButton;
        GRushButton20: TKOLGRushButton;
        GRushButton15: TKOLGRushButton;
        WordWrap: TKOLGRushCheckBox;
        Label32: TKOLLabel;
        DotsCount: TKOLEditBox;
        GRushButton21: TKOLGRushButton;
        ///////////////////
        ActiveWindow: HWnd;
        WindowList: Pointer;
        Prop: TSplitterStylesProp;
        Styles: TKOLGRushSplitterStyles;
        Component: MCKGRushControls.TKOLGRushSplitter;
        procedure KOLForm1BeforeCreateWindow(Sender: PObj);
        procedure KOLForm1FormCreate(Sender: PObj);
        procedure Down1Click(Sender: PObj);
        procedure Down2Click(Sender: PObj);
        procedure CheckEnabledClick(Sender: PObj);
        procedure CheckTransparentClick(Sender: PObj);
        procedure Down3Click(Sender: PObj);
        procedure Down4Click(Sender: PObj);
        procedure Down5Click(Sender: PObj);
        procedure Down6Click(Sender: PObj);
        procedure Down7Click(Sender: PObj);
        procedure GradStylesSelChange(Sender: PObj);
        procedure Col1Click(Sender: PObj);
        procedure Col2Click(Sender: PObj);
        procedure Col3Click(Sender: PObj);
        procedure Col4Click(Sender: PObj);
        procedure Col5Click(Sender: PObj);
        procedure Col6Click(Sender: PObj);
        procedure StatesListSelChange(Sender: PObj);
        procedure UpdateSpeedSelChange(Sender: PObj);
        procedure AntiAliasingClick(Sender: PObj);
        procedure DrawGlyphClick(Sender: PObj);
        procedure DrawTextClick(Sender: PObj);
        procedure CaptionChange(Sender: PObj);
        procedure GlyphHorzSelChange(Sender: PObj);
        procedure GlyphVertSelChange(Sender: PObj);
        procedure TextHorzSelChange(Sender: PObj);
        procedure TextVertSelChange(Sender: PObj);
        procedure Col1Paint(Sender: PControl; DC: HDC);
        procedure CheckEnabledRecalcRects(Sender: PGRushControl;
            var Rects: TGrushRects);
        procedure BorderWiEnter(Sender: PObj);
        procedure BorderWiLeave(Sender: PObj);
        procedure BorderHeLeave(Sender: PObj);
        procedure GlyphXLeave(Sender: PObj);
        procedure GlyphYLeave(Sender: PObj);
        procedure GlyphWidthLeave(Sender: PObj);
        procedure GlyphHeightLeave(Sender: PObj);
        procedure SpacingLeave(Sender: PObj);
        procedure LLeave(Sender: PObj);
        procedure TLeave(Sender: PObj);
        procedure RLeave(Sender: PObj);
        procedure BLeave(Sender: PObj);
        procedure ShadowOffsetLeave(Sender: PObj);
        procedure BorderWidthLeave(Sender: PObj);
        procedure GRushButton11Click(Sender: PObj);
        procedure GRushButton16Click(Sender: PObj);
        procedure GRushButton17Click(Sender: PObj);
        procedure GRushButton19Click(Sender: PObj);
        procedure GRushButton13Click(Sender: PObj);
        procedure GRushButton10Click(Sender: PObj);
        procedure GRushPanel3MouseDown(Sender: PControl;
            var Mouse: TMouseEventData);
        procedure GRushButton9Click(Sender: PObj);
        procedure GRushButton8Click(Sender: PObj);
        procedure GRushButton7Click(Sender: PObj);
        procedure GRushButton18Click(Sender: PObj);
        procedure GRushButton1Click(Sender: PObj);
        procedure GRushButton2Click(Sender: PObj);
        procedure GRushButton3Click(Sender: PObj);
        procedure GRushButton4Click(Sender: PObj);
        procedure GRushButton5Click(Sender: PObj);
        procedure GRushButton6Click(Sender: PObj);
        procedure GRushButton14Click(Sender: PObj);
        procedure GRushButton20Click(Sender: PObj);
        procedure KOLForm1Close(Sender: PObj; var Accept: Boolean);
        procedure GRushButton15Click(Sender: PObj);
        procedure ButtonOKClick(Sender: PObj);
        procedure ButtonCancelClick(Sender: PObj);
        procedure CropTopFirstClick(Sender: PObj);
        procedure GlyphAttachedClick(Sender: PObj);
        procedure WordWrapClick(Sender: PObj);
        procedure GRushButton12Click(Sender: PObj);
        procedure GRushButton21Click(Sender: PObj);
        procedure DotsCountLeave(Sender: PObj);
    private
    public
    end;

var SplitterEditor: PSplitterEditor;

procedure Register;
procedure NewSplitterEditor(var Result: PSplitterEditor; Prop: TSplitterStylesProp);

implementation

procedure Register;
begin
    RegisterPropertyEditor(TypeInfo(TKOLGRushSplitterStyles), nil, '', TSplitterStylesProp);
end;

procedure NewSplitterEditor(var Result: PSplitterEditor; Prop: TSplitterStylesProp);
begin
    New(Result, Create);
    Result.Form := NewForm(nil, 'SplitterEditor').SetPosition(193, 124).SetClientSize(520, 570);
    Result.KOLForm1BeforeCreateWindow(Result);
    Applet := Result.Form;
    Result.Form.Add2AutoFree(Result);
    Result.Form.ExStyle := Result.Form.ExStyle or WS_EX_DLGMODALFRAME or WS_EX_WINDOWEDGE;
    Result.Form.Style := Result.Form.Style and not (WS_MINIMIZEBOX or WS_MAXIMIZEBOX);
    Result.Form.Border := 0;
    Result.Form.OnClose := Result.KOLForm1Close;

    tinyLoadJPGGIFBMPResource(Result.GRushImageCollection1, HINSTANCE, 'GRUSHIMAGECOLLECTION1', 'GRUSHCOLLECTIONS');

    Result.CD1 := NewColorDialog(ccoFullOpen);
    Result.Form.Add2AutoFree(Result.CD1);
    Result.LabelState := NewLabel(Result.Form, 'State:').SetPosition(280, 12).SetSize(41, 17);
    Result.ButtonCancel := PGRushControl(NewGRushButton(Result.Form, 'Cancel').SetPosition(400, 512).SetSize(105, 33));
    Result.ButtonOK := PGRushControl(NewGRushButton(Result.Form, 'OK').SetPosition(272, 512).SetSize(105, 33));
    Result.ButtonOK.Font.FontStyle := [fsBold];
    Result.GRushButton15 := PGRushControl(NewGRushButton(Result.Form, 'Reset to souce').SetPosition(401, 408).SetSize(104, 17));
    Result.GRushButton15.Font.FontHeight := 8;
    Result.GRushButton15.All_BorderRoundWidth := 0;
    Result.GRushButton15.All_BorderRoundHeight := 0;
    Result.GRushButton15.Down_BorderWidth := 1;
    Result.GRushButton15.All_ContentOffsets := MakeRect(0, 0, 0, 0);
    Result.GRushButton20 := PGRushControl(NewGRushButton(Result.Form, 'Reset to default').SetPosition(273, 408).SetSize(104, 17));
    Result.GRushButton20.Font.FontHeight := 8;
    Result.GRushButton20.All_BorderRoundWidth := 0;
    Result.GRushButton20.All_BorderRoundHeight := 0;
    Result.GRushButton20.Down_BorderWidth := 1;
    Result.GRushButton20.All_ContentOffsets := MakeRect(0, 0, 0, 0);
    Result.StatesList := NewComboBox(Result.Form, [coReadOnly, coNoIntegralHeight]).SetPosition(328, 10).SetSize(113, 0);
    Result.StatesList.Font.FontHeight := 8;
    Result.StatesList.Color := clWindow;
    Result.StatesList.Items[0] := 'All states (w/o)';
    Result.StatesList.Items[1] := 'Default state';
    Result.StatesList.Items[2] := 'Over state';
    Result.StatesList.Items[3] := 'Down state';
    Result.StatesList.Items[4] := 'Disabled state';
    Result.StatesList.CurIndex := 0;
    Result.GRushPanel1 := PGRushControl(NewGRushPanel(Result.Form).SetPosition(8, 208).SetSize(249, 353));
    Result.GRushPanel1.Border := 2;
    Result.GRushPanel1.Def_ColorFrom := 15259342;
    Result.GRushPanel1.Def_ColorTo := 15259600;
    Result.GRushPanel1.Def_BorderRoundWidth := 8;
    Result.GRushPanel1.Def_BorderRoundHeight := 9;
    Result.GRushPanel1.Def_GradientStyle := gsSolid;
    Result.GRushPanel1.All_ShadowOffset := 0;
    Result.Label16 := NewLabel(Result.GRushPanel1, 'L:').SetPosition(8, 248).SetSize(17, 17);
    Result.Label16.TextAlign := taRight;
    Result.Label16.Color := $E8D6CE;
    Result.Label17 := NewLabel(Result.GRushPanel1, 'T:').SetPosition(68, 248).SetSize(17, 17);
    Result.Label17.TextAlign := taRight;
    Result.Label17.Color := $E8D6CE;
    Result.Label18 := NewLabel(Result.GRushPanel1, 'Offsets of content').SetPosition(8, 224).SetSize(185, 17);
    Result.Label18.Font.FontStyle := [fsBold];
    Result.Label18.TextAlign := taCenter;
    Result.Label18.Color := $E8D6CE;
    Result.Label19 := NewLabel(Result.GRushPanel1, 'R:').SetPosition(128, 248).SetSize(17, 17);
    Result.Label19.TextAlign := taRight;
    Result.Label19.Color := $E8D6CE;
    Result.Label20 := NewLabel(Result.GRushPanel1, 'B:').SetPosition(188, 248).SetSize(17, 17);
    Result.Label20.TextAlign := taRight;
    Result.Label20.Color := $E8D6CE;
    Result.Label21 := NewLabel(Result.GRushPanel1, 'Spacing:').SetPosition(8, 296).SetSize(97, 17);
    Result.Label21.TextAlign := taRight;
    Result.Label21.Color := $E8D6CE;
    Result.Label22 := NewLabel(Result.GRushPanel1, 'Glyph size').SetPosition(8, 176).SetSize(185, 17);
    Result.Label22.Font.FontStyle := [fsBold];
    Result.Label22.TextAlign := taCenter;
    Result.Label22.Color := $E8D6CE;
    Result.Label23 := NewLabel(Result.GRushPanel1, 'width:').SetPosition(8, 200).SetSize(65, 17);
    Result.Label23.TextAlign := taRight;
    Result.Label23.Color := $E8D6CE;
    Result.Label24 := NewLabel(Result.GRushPanel1, 'height:').SetPosition(128, 200).SetSize(65, 17);
    Result.Label24.TextAlign := taRight;
    Result.Label24.Color := $E8D6CE;
    Result.Label25 := NewLabel(Result.GRushPanel1, 'Update speed:').SetPosition(8, 320).SetSize(97, 17);
    Result.Label25.TextAlign := taRight;
    Result.Label25.Color := $E8D6CE;
    Result.Label26 := NewLabel(Result.GRushPanel1, 'Glyph align').SetPosition(8, 80).SetSize(185, 17);
    Result.Label26.Font.FontStyle := [fsBold];
    Result.Label26.TextAlign := taCenter;
    Result.Label26.Color := $E8D6CE;
    Result.Label27 := NewLabel(Result.GRushPanel1, 'horz:').SetPosition(8, 104).SetSize(49, 17);
    Result.Label27.TextAlign := taRight;
    Result.Label27.Color := $E8D6CE;
    Result.Label28 := NewLabel(Result.GRushPanel1, 'vert:').SetPosition(128, 104).SetSize(49, 17);
    Result.Label28.TextAlign := taRight;
    Result.Label28.Color := $E8D6CE;
    Result.Label29 := NewLabel(Result.GRushPanel1, 'Text align').SetPosition(8, 128).SetSize(185, 17);
    Result.Label29.Font.FontStyle := [fsBold];
    Result.Label29.TextAlign := taCenter;
    Result.Label29.Color := $E8D6CE;
    Result.Label30 := NewLabel(Result.GRushPanel1, 'horz:').SetPosition(8, 152).SetSize(49, 17);
    Result.Label30.TextAlign := taRight;
    Result.Label30.Color := $E8D6CE;
    Result.Label31 := NewLabel(Result.GRushPanel1, 'vert:').SetPosition(128, 152).SetSize(49, 17);
    Result.Label31.TextAlign := taRight;
    Result.Label31.Color := $E8D6CE;
    Result.Label32 := NewLabel(Result.GRushPanel1, 'Dots count:').SetPosition(8, 272).SetSize(97, 17);
    Result.Label32.TextAlign := taRight;
    Result.Label32.Color := $E8D6CE;
    Result.B := NewEditBox(Result.GRushPanel1, []).SetPosition(208, 248).SetSize(33, 17);
    Result.B.Ctl3D := False;
    Result.B.Font.FontHeight := 8;
    Result.B.Text := '0';
    Result.DotsCount := NewEditBox(Result.GRushPanel1, []).SetPosition(112, 272).SetSize(81, 17);
    Result.DotsCount.Ctl3D := False;
    Result.DotsCount.Font.FontHeight := 8;
    Result.DotsCount.Text := '0';
    Result.GlyphHeight := NewEditBox(Result.GRushPanel1, []).SetPosition(200, 200).SetSize(41, 17);
    Result.GlyphHeight.Ctl3D := False;
    Result.GlyphHeight.Font.FontHeight := 8;
    Result.GlyphHeight.Text := '0';
    Result.GlyphWidth := NewEditBox(Result.GRushPanel1, []).SetPosition(80, 200).SetSize(41, 17);
    Result.GlyphWidth.Ctl3D := False;
    Result.GlyphWidth.Font.FontHeight := 8;
    Result.GlyphWidth.Text := '0';
    Result.L := NewEditBox(Result.GRushPanel1, []).SetPosition(28, 248).SetSize(33, 17);
    Result.L.Ctl3D := False;
    Result.L.Font.FontHeight := 8;
    Result.L.Text := '0';
    Result.R := NewEditBox(Result.GRushPanel1, []).SetPosition(148, 248).SetSize(33, 17);
    Result.R.Ctl3D := False;
    Result.R.Font.FontHeight := 8;
    Result.R.Text := '0';
    Result.Spacing := NewEditBox(Result.GRushPanel1, []).SetPosition(112, 296).SetSize(81, 17);
    Result.Spacing.Ctl3D := False;
    Result.Spacing.Font.FontHeight := 8;
    Result.Spacing.Text := '0';
    Result.T := NewEditBox(Result.GRushPanel1, []).SetPosition(88, 248).SetSize(33, 17);
    Result.T.Ctl3D := False;
    Result.T.Font.FontHeight := 8;
    Result.T.Text := '0';
    Result.GRushButton11 := PGRushControl(NewGRushButton(Result.GRushPanel1, 'Default').SetPosition(200, 80).SetSize(41, 17));
    Result.GRushButton11.Font.FontHeight := 8;
    Result.GRushButton11.All_BorderRoundWidth := 0;
    Result.GRushButton11.All_BorderRoundHeight := 0;
    Result.GRushButton11.Down_BorderWidth := 1;
    Result.GRushButton11.All_ContentOffsets := MakeRect(0, 0, 0, 0);
    Result.GRushButton12 := PGRushControl(NewGRushButton(Result.GRushPanel1, 'Default').SetPosition(200, 128).SetSize(41, 17));
    Result.GRushButton12.Font.FontHeight := 8;
    Result.GRushButton12.All_BorderRoundWidth := 0;
    Result.GRushButton12.All_BorderRoundHeight := 0;
    Result.GRushButton12.Down_BorderWidth := 1;
    Result.GRushButton12.All_ContentOffsets := MakeRect(0, 0, 0, 0);
    Result.GRushButton13 := PGRushControl(NewGRushButton(Result.GRushPanel1, 'Default').SetPosition(200, 176).SetSize(41, 17));
    Result.GRushButton13.Font.FontHeight := 8;
    Result.GRushButton13.All_BorderRoundWidth := 0;
    Result.GRushButton13.All_BorderRoundHeight := 0;
    Result.GRushButton13.Down_BorderWidth := 1;
    Result.GRushButton13.All_ContentOffsets := MakeRect(0, 0, 0, 0);
    Result.GRushButton16 := PGRushControl(NewGRushButton(Result.GRushPanel1, 'Default').SetPosition(200, 224).SetSize(41, 17));
    Result.GRushButton16.Font.FontHeight := 8;
    Result.GRushButton16.All_BorderRoundWidth := 0;
    Result.GRushButton16.All_BorderRoundHeight := 0;
    Result.GRushButton16.Down_BorderWidth := 1;
    Result.GRushButton16.All_ContentOffsets := MakeRect(0, 0, 0, 0);
    Result.GRushButton17 := PGRushControl(NewGRushButton(Result.GRushPanel1, 'Default').SetPosition(200, 296).SetSize(41, 17));
    Result.GRushButton17.Font.FontHeight := 8;
    Result.GRushButton17.All_BorderRoundWidth := 0;
    Result.GRushButton17.All_BorderRoundHeight := 0;
    Result.GRushButton17.Down_BorderWidth := 1;
    Result.GRushButton17.All_ContentOffsets := MakeRect(0, 0, 0, 0);
    Result.GRushButton19 := PGRushControl(NewGRushButton(Result.GRushPanel1, 'Default').SetPosition(200, 320).SetSize(41, 17));
    Result.GRushButton19.Font.FontHeight := 8;
    Result.GRushButton19.All_BorderRoundWidth := 0;
    Result.GRushButton19.All_BorderRoundHeight := 0;
    Result.GRushButton19.Down_BorderWidth := 1;
    Result.GRushButton19.All_ContentOffsets := MakeRect(0, 0, 0, 0);
    Result.GRushButton21 := PGRushControl(NewGRushButton(Result.GRushPanel1, 'Default').SetPosition(200, 272).SetSize(41, 17));
    Result.GRushButton21.Font.FontHeight := 8;
    Result.GRushButton21.All_BorderRoundWidth := 0;
    Result.GRushButton21.All_BorderRoundHeight := 0;
    Result.GRushButton21.Down_BorderWidth := 1;
    Result.GRushButton21.All_ContentOffsets := MakeRect(0, 0, 0, 0);
    Result.GlyphHorz := NewComboBox(Result.GRushPanel1, [coReadOnly, coNoIntegralHeight]).SetPosition(64, 102).SetSize(57, 0);
    Result.GlyphHorz.Font.FontHeight := 8;
    Result.GlyphHorz.Color := clWindow;
    Result.GlyphHorz.Items[0] := 'Left';
    Result.GlyphHorz.Items[1] := 'Center';
    Result.GlyphHorz.Items[2] := 'Right';
    Result.GlyphHorz.CurIndex := 0;
    Result.GlyphVert := NewComboBox(Result.GRushPanel1, [coReadOnly, coNoIntegralHeight]).SetPosition(184, 102).SetSize(57, 0);
    Result.GlyphVert.Font.FontHeight := 8;
    Result.GlyphVert.Color := clWindow;
    Result.GlyphVert.Items[0] := 'Top';
    Result.GlyphVert.Items[1] := 'Center';
    Result.GlyphVert.Items[2] := 'Bottom';
    Result.GlyphVert.CurIndex := 0;
    Result.TextHorz := NewComboBox(Result.GRushPanel1, [coReadOnly, coNoIntegralHeight]).SetPosition(64, 150).SetSize(57, 0);
    Result.TextHorz.Font.FontHeight := 8;
    Result.TextHorz.Color := clWindow;
    Result.TextHorz.Items[0] := 'Left';
    Result.TextHorz.Items[1] := 'Center';
    Result.TextHorz.Items[2] := 'Right';
    Result.TextHorz.CurIndex := 0;
    Result.TextVert := NewComboBox(Result.GRushPanel1, [coReadOnly, coNoIntegralHeight]).SetPosition(184, 150).SetSize(57, 0);
    Result.TextVert.Font.FontHeight := 8;
    Result.TextVert.Color := clWindow;
    Result.TextVert.Items[0] := 'Top';
    Result.TextVert.Items[1] := 'Center';
    Result.TextVert.Items[2] := 'Bottom';
    Result.TextVert.CurIndex := 0;
    Result.UpdateSpeed := NewComboBox(Result.GRushPanel1, [coReadOnly, coNoIntegralHeight]).SetPosition(112, 318).SetSize(81, 21);
    Result.UpdateSpeed.Font.FontHeight := 8;
    Result.UpdateSpeed.Color := clWindow;
    Result.UpdateSpeed.Items[0] := 'Immediately';
    Result.UpdateSpeed.Items[1] := 'Very fast';
    Result.UpdateSpeed.Items[2] := 'Fast';
    Result.UpdateSpeed.Items[3] := 'Normal';
    Result.UpdateSpeed.Items[4] := 'Slow';
    Result.UpdateSpeed.Items[5] := 'Very slow';
    Result.UpdateSpeed.CurIndex := 0;
    Result.AntiAliasing := PGRushControl(NewGRushCheckBox(Result.GRushPanel1, 'Antialiasing').SetPosition(8, 8).SetSize(113, 17));
    Result.AntiAliasing.Down_ColorFrom := 14798527;
    Result.AntiAliasing.Down_ColorTo := 16777215;
    Result.AntiAliasing.All_ColorOuter := 15259342;
    Result.AntiAliasing.All_ColorShadow := 12632256;
    Result.AntiAliasing.Over_BorderColor := 8421504;
    Result.AntiAliasing.Down_BorderWidth := 1;
    Result.AntiAliasing.All_ShadowOffset := 0;
    Result.CropTopFirst := PGRushControl(NewGRushCheckBox(Result.GRushPanel1, 'Crop top first').SetPosition(8, 32).SetSize(113, 17));
    Result.CropTopFirst.Enabled := False;
    Result.CropTopFirst.Down_ColorFrom := 14798527;
    Result.CropTopFirst.Down_ColorTo := 16777215;
    Result.CropTopFirst.All_ColorOuter := 15259342;
    Result.CropTopFirst.Dis_ColorText := 8421504;
    Result.CropTopFirst.All_ColorShadow := 12632256;
    Result.CropTopFirst.Over_BorderColor := 8421504;
    Result.CropTopFirst.Down_BorderWidth := 1;
    Result.CropTopFirst.All_ShadowOffset := 0;
    Result.DrawGlyph := PGRushControl(NewGRushCheckBox(Result.GRushPanel1, 'Draw glyph').SetPosition(8, 56).SetSize(113, 17));
    Result.DrawGlyph.Down_ColorFrom := 14798527;
    Result.DrawGlyph.Down_ColorTo := 16777215;
    Result.DrawGlyph.All_ColorOuter := 15259342;
    Result.DrawGlyph.All_ColorShadow := 12632256;
    Result.DrawGlyph.Over_BorderColor := 8421504;
    Result.DrawGlyph.Down_BorderWidth := 1;
    Result.DrawGlyph.All_ShadowOffset := 0;
    Result.DrawText := PGRushControl(NewGRushCheckBox(Result.GRushPanel1, 'Draw text').SetPosition(128, 56).SetSize(113, 17));
    Result.DrawText.Down_ColorFrom := 14798527;
    Result.DrawText.Down_ColorTo := 16777215;
    Result.DrawText.All_ColorOuter := 15259342;
    Result.DrawText.All_ColorShadow := 12632256;
    Result.DrawText.Over_BorderColor := 8421504;
    Result.DrawText.Down_BorderWidth := 1;
    Result.DrawText.All_ShadowOffset := 0;
    Result.GlyphAttached := PGRushControl(NewGRushCheckBox(Result.GRushPanel1, 'Glyph attached').SetPosition(128, 32).SetSize(113, 17));
    Result.GlyphAttached.Enabled := False;
    Result.GlyphAttached.Down_ColorFrom := 14798527;
    Result.GlyphAttached.Down_ColorTo := 16777215;
    Result.GlyphAttached.All_ColorOuter := 15259342;
    Result.GlyphAttached.Dis_ColorText := 8421504;
    Result.GlyphAttached.All_ColorShadow := 12632256;
    Result.GlyphAttached.Over_BorderColor := 8421504;
    Result.GlyphAttached.Down_BorderWidth := 1;
    Result.GlyphAttached.All_ShadowOffset := 0;
    Result.WordWrap := PGRushControl(NewGRushCheckBox(Result.GRushPanel1, 'Word wrap').SetPosition(128, 8).SetSize(113, 17));
    Result.WordWrap.Enabled := False;
    Result.WordWrap.Down_ColorFrom := 14798527;
    Result.WordWrap.Down_ColorTo := 16777215;
    Result.WordWrap.All_ColorOuter := 15259342;
    Result.WordWrap.All_ColorShadow := 12632256;
    Result.WordWrap.Over_BorderColor := 8421504;
    Result.WordWrap.Down_BorderWidth := 1;
    Result.WordWrap.All_ShadowOffset := 0;
    Result.GRushPanel2 := PGRushControl(NewGRushPanel(Result.Form).SetPosition(264, 40).SetSize(249, 353));
    Result.GRushPanel2.Font.FontStyle := [fsBold];
    Result.GRushPanel2.Border := 2;
    Result.GRushPanel2.Caption := 'State options';
    Result.GRushPanel2.Def_ColorFrom := 15259342;
    Result.GRushPanel2.Def_ColorTo := 15259600;
    Result.GRushPanel2.Def_BorderRoundWidth := 8;
    Result.GRushPanel2.Def_BorderRoundHeight := 9;
    Result.GRushPanel2.Def_GradientStyle := gsSolid;
    Result.GRushPanel2.All_ShadowOffset := 0;
    Result.GRushPanel2.All_ContentOffsets := MakeRect(12, 4, -4, -4);
    Result.GRushPanel2.All_TextHAlign := haLeft;
    Result.Col1 := NewLabel(Result.GRushPanel2, '').SetPosition(128, 32).SetSize(49, 17);
    Result.Col1.Font.FontStyle := [];
    Result.Col1.Color := clSilver;
    Result.Col2 := NewLabel(Result.GRushPanel2, '').SetPosition(128, 56).SetSize(49, 17);
    Result.Col2.Font.FontStyle := [];
    Result.Col2.Color := clSilver;
    Result.Col3 := NewLabel(Result.GRushPanel2, '').SetPosition(128, 80).SetSize(49, 17);
    Result.Col3.Font.FontStyle := [];
    Result.Col3.Color := clSilver;
    Result.Col4 := NewLabel(Result.GRushPanel2, '').SetPosition(128, 104).SetSize(49, 17);
    Result.Col4.Font.FontStyle := [];
    Result.Col4.Color := clSilver;
    Result.Col5 := NewLabel(Result.GRushPanel2, '').SetPosition(128, 128).SetSize(49, 17);
    Result.Col5.Font.FontStyle := [];
    Result.Col5.Color := clSilver;
    Result.Col6 := NewLabel(Result.GRushPanel2, '').SetPosition(128, 152).SetSize(49, 17);
    Result.Col6.Font.FontStyle := [];
    Result.Col6.Color := clSilver;
    Result.Label1 := NewLabel(Result.GRushPanel2, 'Border color:').SetPosition(8, 104).SetSize(97, 17);
    Result.Label1.Font.FontStyle := [];
    Result.Label1.TextAlign := taRight;
    Result.Label1.Color := $E8D6CE;
    Result.Label10 := NewLabel(Result.GRushPanel2, 'by X:').SetPosition(8, 320).SetSize(65, 17);
    Result.Label10.Font.FontStyle := [];
    Result.Label10.TextAlign := taRight;
    Result.Label10.Color := $E8D6CE;
    Result.Label11 := NewLabel(Result.GRushPanel2, 'Border width:').SetPosition(8, 200).SetSize(97, 17);
    Result.Label11.Font.FontStyle := [];
    Result.Label11.TextAlign := taRight;
    Result.Label11.Color := $E8D6CE;
    Result.Label12 := NewLabel(Result.GRushPanel2, 'Border ellipse').SetPosition(8, 248).SetSize(185, 17);
    Result.Label12.TextAlign := taCenter;
    Result.Label12.Color := $E8D6CE;
    Result.Label13 := NewLabel(Result.GRushPanel2, 'width:').SetPosition(8, 272).SetSize(65, 17);
    Result.Label13.Font.FontStyle := [];
    Result.Label13.TextAlign := taRight;
    Result.Label13.Color := $E8D6CE;
    Result.Label14 := NewLabel(Result.GRushPanel2, 'height:').SetPosition(128, 272).SetSize(65, 17);
    Result.Label14.Font.FontStyle := [];
    Result.Label14.TextAlign := taRight;
    Result.Label14.Color := $E8D6CE;
    Result.Label15 := NewLabel(Result.GRushPanel2, 'by Y:').SetPosition(128, 320).SetSize(65, 17);
    Result.Label15.Font.FontStyle := [];
    Result.Label15.TextAlign := taRight;
    Result.Label15.Color := $E8D6CE;
    Result.Label2 := NewLabel(Result.GRushPanel2, 'From color:').SetPosition(8, 56).SetSize(97, 17);
    Result.Label2.Font.FontStyle := [];
    Result.Label2.TextAlign := taRight;
    Result.Label2.Color := $E8D6CE;
    Result.Label3 := NewLabel(Result.GRushPanel2, 'To color:').SetPosition(8, 80).SetSize(97, 17);
    Result.Label3.Font.FontStyle := [];
    Result.Label3.TextAlign := taRight;
    Result.Label3.Color := $E8D6CE;
    Result.Label4 := NewLabel(Result.GRushPanel2, 'Outer color:').SetPosition(8, 32).SetSize(97, 17);
    Result.Label4.Font.FontStyle := [];
    Result.Label4.TextAlign := taRight;
    Result.Label4.Color := $E8D6CE;
    Result.Label5 := NewLabel(Result.GRushPanel2, 'Text color:').SetPosition(8, 128).SetSize(97, 17);
    Result.Label5.Font.FontStyle := [];
    Result.Label5.TextAlign := taRight;
    Result.Label5.Color := $E8D6CE;
    Result.Label6 := NewLabel(Result.GRushPanel2, 'Shadow color:').SetPosition(8, 152).SetSize(97, 17);
    Result.Label6.Font.FontStyle := [];
    Result.Label6.TextAlign := taRight;
    Result.Label6.Color := $E8D6CE;
    Result.Label7 := NewLabel(Result.GRushPanel2, 'Gradient style:').SetPosition(8, 176).SetSize(97, 17);
    Result.Label7.Font.FontStyle := [];
    Result.Label7.TextAlign := taRight;
    Result.Label7.Color := $E8D6CE;
    Result.Label8 := NewLabel(Result.GRushPanel2, 'Shadow offset:').SetPosition(8, 224).SetSize(97, 17);
    Result.Label8.Font.FontStyle := [];
    Result.Label8.TextAlign := taRight;
    Result.Label8.Color := $E8D6CE;
    Result.Label9 := NewLabel(Result.GRushPanel2, 'Glyph item').SetPosition(8, 296).SetSize(185, 17);
    Result.Label9.TextAlign := taCenter;
    Result.Label9.Color := $E8D6CE;
    Result.BorderHe := NewEditBox(Result.GRushPanel2, []).SetPosition(200, 272).SetSize(41, 17);
    Result.BorderHe.Ctl3D := False;
    Result.BorderHe.Font.FontStyle := [];
    Result.BorderHe.Font.FontHeight := 8;
    Result.BorderWi := NewEditBox(Result.GRushPanel2, []).SetPosition(80, 272).SetSize(41, 17);
    Result.BorderWi.Ctl3D := False;
    Result.BorderWi.Font.FontStyle := [];
    Result.BorderWi.Font.FontHeight := 8;
    Result.BorderWidth := NewEditBox(Result.GRushPanel2, []).SetPosition(112, 200).SetSize(81, 17);
    Result.BorderWidth.Ctl3D := False;
    Result.BorderWidth.Font.FontStyle := [];
    Result.BorderWidth.Font.FontHeight := 8;
    Result.GlyphX := NewEditBox(Result.GRushPanel2, []).SetPosition(80, 320).SetSize(41, 17);
    Result.GlyphX.Ctl3D := False;
    Result.GlyphX.Font.FontStyle := [];
    Result.GlyphX.Font.FontHeight := 8;
    Result.GlyphY := NewEditBox(Result.GRushPanel2, []).SetPosition(200, 320).SetSize(41, 17);
    Result.GlyphY.Ctl3D := False;
    Result.GlyphY.Font.FontStyle := [];
    Result.GlyphY.Font.FontHeight := 8;
    Result.ShadowOffset := NewEditBox(Result.GRushPanel2, []).SetPosition(112, 224).SetSize(81, 17);
    Result.ShadowOffset.Ctl3D := False;
    Result.ShadowOffset.Font.FontStyle := [];
    Result.ShadowOffset.Font.FontHeight := 8;
    Result.GRushButton1 := PGRushControl(NewGRushButton(Result.GRushPanel2, 'Default').SetPosition(200, 32).SetSize(41, 17));
    Result.GRushButton1.Font.FontStyle := [];
    Result.GRushButton1.Font.FontHeight := 8;
    Result.GRushButton1.All_BorderRoundWidth := 0;
    Result.GRushButton1.All_BorderRoundHeight := 0;
    Result.GRushButton1.Down_BorderWidth := 1;
    Result.GRushButton1.All_ContentOffsets := MakeRect(0, 0, 0, 0);
    Result.GRushButton10 := PGRushControl(NewGRushButton(Result.GRushPanel2, 'Default').SetPosition(200, 296).SetSize(41, 17));
    Result.GRushButton10.Font.FontStyle := [];
    Result.GRushButton10.Font.FontHeight := 8;
    Result.GRushButton10.All_BorderRoundWidth := 0;
    Result.GRushButton10.All_BorderRoundHeight := 0;
    Result.GRushButton10.Down_BorderWidth := 1;
    Result.GRushButton10.All_ContentOffsets := MakeRect(0, 0, 0, 0);
    Result.GRushButton14 := PGRushControl(NewGRushButton(Result.GRushPanel2, 'Reset state').SetPosition(176, 4).SetSize(65, 17));
    Result.GRushButton14.Font.FontStyle := [];
    Result.GRushButton14.Font.FontHeight := 8;
    Result.GRushButton14.All_BorderRoundWidth := 0;
    Result.GRushButton14.All_BorderRoundHeight := 0;
    Result.GRushButton14.Down_BorderWidth := 1;
    Result.GRushButton14.All_ContentOffsets := MakeRect(0, 0, 0, 0);
    Result.GRushButton18 := PGRushControl(NewGRushButton(Result.GRushPanel2, 'Default').SetPosition(200, 176).SetSize(41, 17));
    Result.GRushButton18.Font.FontStyle := [];
    Result.GRushButton18.Font.FontHeight := 8;
    Result.GRushButton18.All_BorderRoundWidth := 0;
    Result.GRushButton18.All_BorderRoundHeight := 0;
    Result.GRushButton18.Down_BorderWidth := 1;
    Result.GRushButton18.All_ContentOffsets := MakeRect(0, 0, 0, 0);
    Result.GRushButton2 := PGRushControl(NewGRushButton(Result.GRushPanel2, 'Default').SetPosition(200, 56).SetSize(41, 17));
    Result.GRushButton2.Font.FontStyle := [];
    Result.GRushButton2.Font.FontHeight := 8;
    Result.GRushButton2.All_BorderRoundWidth := 0;
    Result.GRushButton2.All_BorderRoundHeight := 0;
    Result.GRushButton2.Down_BorderWidth := 1;
    Result.GRushButton2.All_ContentOffsets := MakeRect(0, 0, 0, 0);
    Result.GRushButton3 := PGRushControl(NewGRushButton(Result.GRushPanel2, 'Default').SetPosition(200, 80).SetSize(41, 17));
    Result.GRushButton3.Font.FontStyle := [];
    Result.GRushButton3.Font.FontHeight := 8;
    Result.GRushButton3.All_BorderRoundWidth := 0;
    Result.GRushButton3.All_BorderRoundHeight := 0;
    Result.GRushButton3.Down_BorderWidth := 1;
    Result.GRushButton3.All_ContentOffsets := MakeRect(0, 0, 0, 0);
    Result.GRushButton4 := PGRushControl(NewGRushButton(Result.GRushPanel2, 'Default').SetPosition(200, 104).SetSize(41, 17));
    Result.GRushButton4.Font.FontStyle := [];
    Result.GRushButton4.Font.FontHeight := 8;
    Result.GRushButton4.All_BorderRoundWidth := 0;
    Result.GRushButton4.All_BorderRoundHeight := 0;
    Result.GRushButton4.Down_BorderWidth := 1;
    Result.GRushButton4.All_ContentOffsets := MakeRect(0, 0, 0, 0);
    Result.GRushButton5 := PGRushControl(NewGRushButton(Result.GRushPanel2, 'Default').SetPosition(200, 128).SetSize(41, 17));
    Result.GRushButton5.Font.FontStyle := [];
    Result.GRushButton5.Font.FontHeight := 8;
    Result.GRushButton5.All_BorderRoundWidth := 0;
    Result.GRushButton5.All_BorderRoundHeight := 0;
    Result.GRushButton5.Down_BorderWidth := 1;
    Result.GRushButton5.All_ContentOffsets := MakeRect(0, 0, 0, 0);
    Result.GRushButton6 := PGRushControl(NewGRushButton(Result.GRushPanel2, 'Default').SetPosition(200, 152).SetSize(41, 17));
    Result.GRushButton6.Font.FontStyle := [];
    Result.GRushButton6.Font.FontHeight := 8;
    Result.GRushButton6.All_BorderRoundWidth := 0;
    Result.GRushButton6.All_BorderRoundHeight := 0;
    Result.GRushButton6.Down_BorderWidth := 1;
    Result.GRushButton6.All_ContentOffsets := MakeRect(0, 0, 0, 0);
    Result.GRushButton7 := PGRushControl(NewGRushButton(Result.GRushPanel2, 'Default').SetPosition(200, 200).SetSize(41, 17));
    Result.GRushButton7.Font.FontStyle := [];
    Result.GRushButton7.Font.FontHeight := 8;
    Result.GRushButton7.All_BorderRoundWidth := 0;
    Result.GRushButton7.All_BorderRoundHeight := 0;
    Result.GRushButton7.Down_BorderWidth := 1;
    Result.GRushButton7.All_ContentOffsets := MakeRect(0, 0, 0, 0);
    Result.GRushButton8 := PGRushControl(NewGRushButton(Result.GRushPanel2, 'Default').SetPosition(200, 224).SetSize(41, 17));
    Result.GRushButton8.Font.FontStyle := [];
    Result.GRushButton8.Font.FontHeight := 8;
    Result.GRushButton8.All_BorderRoundWidth := 0;
    Result.GRushButton8.All_BorderRoundHeight := 0;
    Result.GRushButton8.Down_BorderWidth := 1;
    Result.GRushButton8.All_ContentOffsets := MakeRect(0, 0, 0, 0);
    Result.GRushButton9 := PGRushControl(NewGRushButton(Result.GRushPanel2, 'Default').SetPosition(200, 248).SetSize(41, 17));
    Result.GRushButton9.Font.FontStyle := [];
    Result.GRushButton9.Font.FontHeight := 8;
    Result.GRushButton9.All_BorderRoundWidth := 0;
    Result.GRushButton9.All_BorderRoundHeight := 0;
    Result.GRushButton9.Down_BorderWidth := 1;
    Result.GRushButton9.All_ContentOffsets := MakeRect(0, 0, 0, 0);
    Result.GradStyles := NewComboBox(Result.GRushPanel2, [coReadOnly, coNoIntegralHeight]).SetPosition(112, 174).SetSize(81, 0);
    Result.GradStyles.Font.FontStyle := [];
    Result.GradStyles.Font.FontHeight := 8;
    Result.GradStyles.Color := clWindow;
    Result.GradStyles.Items[0] := 'Solid';
    Result.GradStyles.Items[1] := 'Vertical';
    Result.GradStyles.Items[2] := 'Horizontal';
    Result.GradStyles.Items[3] := 'Double vertical';
    Result.GradStyles.Items[4] := 'Double horizontal';
    Result.GradStyles.Items[5] := 'From top left';
    Result.GradStyles.Items[6] := 'From top right';
    Result.GradStyles.CurIndex := 0;
    Result.GRushPanel3 := PGRushControl(NewGRushPanel(Result.Form).SetPosition(7, 8).SetSize(249, 193));
    Result.GRushPanel3.Font.FontStyle := [fsBold];
    Result.GRushPanel3.Border := 2;
    Result.GRushPanel3.Caption := 'Sample control';
    Result.GRushPanel3.Def_ColorFrom := -2147483633;
    Result.GRushPanel3.Def_ColorTo := 15259600;
    Result.GRushPanel3.Def_BorderRoundWidth := 8;
    Result.GRushPanel3.Def_BorderRoundHeight := 9;
    Result.GRushPanel3.Def_GradientStyle := gsSolid;
    Result.GRushPanel3.All_ShadowOffset := 0;
    Result.GRushPanel3.All_ContentOffsets := MakeRect(12, 4, -4, -4);
    Result.GRushPanel3.All_TextHAlign := haLeft;
    Result.Caption := NewEditBox(Result.GRushPanel3, []).SetPosition(8, 168).SetSize(233, 17);
    Result.Caption.Ctl3D := False;
    Result.Caption.Font.FontStyle := [];
    Result.Control := PGRushControl(NewGRushButton(Result.GRushPanel3, '').SetPosition(8, 24).SetSize(233, 113));
    Result.Control.Font.FontStyle := [];
    Result.Control.Cursor := LoadCursor(0, IDC_SIZENS);
    Result.Control.Def_ColorFrom := -2147483633;
    Result.Control.Over_ColorFrom := 16777215;
    Result.Control.Down_ColorFrom := 16777215;
    Result.Control.Over_ColorTo := 13675925;
    Result.Control.Down_ColorTo := 12817525;
    Result.Control.All_BorderRoundWidth := 0;
    Result.Control.All_BorderRoundHeight := 0;
    Result.Control.All_BorderWidth := 0;
    Result.Control.Def_GradientStyle := gsSolid;
    Result.Control.Over_GradientStyle := gsVertical;
    Result.Control.Down_GradientStyle := gsVertical;
    Result.Control.Dis_GradientStyle := gsVertical;
    Result.Control.All_DrawFocusRect := FALSE;
    Result.Control.All_UpdateSpeed := usVeryFast;
    Result.CheckEnabled := PGRushControl(NewGRushCheckBox(Result.GRushPanel3, 'Enabled').SetPosition(8, 144).SetSize(113, 17));
    Result.CheckEnabled.Font.FontStyle := [];
    Result.CheckEnabled.Checked := TRUE;
    Result.CheckEnabled.Down_ColorFrom := 14798527;
    Result.CheckEnabled.Down_ColorTo := 16777215;
    Result.CheckEnabled.All_ColorShadow := 12632256;
    Result.CheckEnabled.Over_BorderColor := 8421504;
    Result.CheckEnabled.Down_BorderWidth := 1;
    Result.CheckEnabled.Down_ShadowOffset := 1;
    Result.CheckEnabled.Dis_ShadowOffset := 1;
    Result.CheckTransparent := PGRushControl(NewGRushCheckBox(Result.GRushPanel3, 'Transparent').SetPosition(128, 144).SetSize(113, 17));
    Result.CheckTransparent.Font.FontStyle := [];
    Result.CheckTransparent.Down_ColorFrom := 14798527;
    Result.CheckTransparent.Down_ColorTo := 16777215;
    Result.CheckTransparent.All_ColorShadow := 12632256;
    Result.CheckTransparent.Over_BorderColor := 8421504;
    Result.CheckTransparent.Down_BorderWidth := 1;
    Result.CheckTransparent.Down_ShadowOffset := 1;
    Result.CheckTransparent.Dis_ShadowOffset := 1;
    Result.Down1 := PGRushControl(NewGRushButton(Result.StatesList, '').SetPosition(94, 1).SetSize(18, 19));
    Result.Down1.All_BorderRoundWidth := 0;
    Result.Down1.All_BorderRoundHeight := 0;
    Result.Down1.Down_BorderWidth := 1;
    Result.Down1.Dis_BorderWidth := 1;
    Result.Down1.Def_ShadowOffset := 0;
    Result.Down1.Over_ShadowOffset := 0;
    Result.Down1.Down_ShadowOffset := 255;
    Result.Down1.Dis_ShadowOffset := 0;
    Result.Down1.Over_GlyphItemY := 1;
    Result.Down1.Down_GlyphItemY := 2;
    Result.Down1.Dis_GlyphItemY := 3;
    Result.Down1.All_ContentOffsets := MakeRect(0, 0, 0, 0);
    Result.Down1.All_GlyphBitmap := Result.GRushImageCollection1;
    Result.Down1.All_GlyphWidth := 12;
    Result.Down1.All_GlyphHeight := 12;
    Result.Down1.All_GlyphHAlign := haCenter;
    Result.Down1.All_Spacing := 0;
    Result.Down1.All_DrawFocusRect := FALSE;
    Result.Down2 := PGRushControl(NewGRushButton(Result.GradStyles, '').SetPosition(62, 1).SetSize(18, 19));
    Result.Down2.All_BorderRoundWidth := 0;
    Result.Down2.All_BorderRoundHeight := 0;
    Result.Down2.Down_BorderWidth := 1;
    Result.Down2.Dis_BorderWidth := 1;
    Result.Down2.Def_ShadowOffset := 0;
    Result.Down2.Over_ShadowOffset := 0;
    Result.Down2.Down_ShadowOffset := 255;
    Result.Down2.Dis_ShadowOffset := 0;
    Result.Down2.Over_GlyphItemY := 1;
    Result.Down2.Down_GlyphItemY := 2;
    Result.Down2.Dis_GlyphItemY := 3;
    Result.Down2.All_ContentOffsets := MakeRect(0, 0, 0, 0);
    Result.Down2.All_GlyphBitmap := Result.GRushImageCollection1;
    Result.Down2.All_GlyphWidth := 12;
    Result.Down2.All_GlyphHeight := 12;
    Result.Down2.All_GlyphHAlign := haCenter;
    Result.Down2.All_Spacing := 0;
    Result.Down2.All_DrawFocusRect := FALSE;
    Result.Down3 := PGRushControl(NewGRushButton(Result.UpdateSpeed, '').SetPosition(62, 1).SetSize(18, 19));
    Result.Down3.All_BorderRoundWidth := 0;
    Result.Down3.All_BorderRoundHeight := 0;
    Result.Down3.Down_BorderWidth := 1;
    Result.Down3.Dis_BorderWidth := 1;
    Result.Down3.Def_ShadowOffset := 0;
    Result.Down3.Over_ShadowOffset := 0;
    Result.Down3.Down_ShadowOffset := 255;
    Result.Down3.Dis_ShadowOffset := 0;
    Result.Down3.Over_GlyphItemY := 1;
    Result.Down3.Down_GlyphItemY := 2;
    Result.Down3.Dis_GlyphItemY := 3;
    Result.Down3.All_ContentOffsets := MakeRect(0, 0, 0, 0);
    Result.Down3.All_GlyphBitmap := Result.GRushImageCollection1;
    Result.Down3.All_GlyphWidth := 12;
    Result.Down3.All_GlyphHeight := 12;
    Result.Down3.All_GlyphHAlign := haCenter;
    Result.Down3.All_Spacing := 0;
    Result.Down3.All_DrawFocusRect := FALSE;
    Result.Down4 := PGRushControl(NewGRushButton(Result.GlyphHorz, '').SetPosition(38, 1).SetSize(18, 19));
    Result.Down4.All_BorderRoundWidth := 0;
    Result.Down4.All_BorderRoundHeight := 0;
    Result.Down4.Down_BorderWidth := 1;
    Result.Down4.Dis_BorderWidth := 1;
    Result.Down4.Def_ShadowOffset := 0;
    Result.Down4.Over_ShadowOffset := 0;
    Result.Down4.Down_ShadowOffset := 255;
    Result.Down4.Dis_ShadowOffset := 0;
    Result.Down4.Over_GlyphItemY := 1;
    Result.Down4.Down_GlyphItemY := 2;
    Result.Down4.Dis_GlyphItemY := 3;
    Result.Down4.All_ContentOffsets := MakeRect(0, 0, 0, 0);
    Result.Down4.All_GlyphBitmap := Result.GRushImageCollection1;
    Result.Down4.All_GlyphWidth := 12;
    Result.Down4.All_GlyphHeight := 12;
    Result.Down4.All_GlyphHAlign := haCenter;
    Result.Down4.All_Spacing := 0;
    Result.Down4.All_DrawFocusRect := FALSE;
    Result.Down5 := PGRushControl(NewGRushButton(Result.GlyphVert, '').SetPosition(38, 1).SetSize(18, 19));
    Result.Down5.All_BorderRoundWidth := 0;
    Result.Down5.All_BorderRoundHeight := 0;
    Result.Down5.Down_BorderWidth := 1;
    Result.Down5.Dis_BorderWidth := 1;
    Result.Down5.Def_ShadowOffset := 0;
    Result.Down5.Over_ShadowOffset := 0;
    Result.Down5.Down_ShadowOffset := 255;
    Result.Down5.Dis_ShadowOffset := 0;
    Result.Down5.Over_GlyphItemY := 1;
    Result.Down5.Down_GlyphItemY := 2;
    Result.Down5.Dis_GlyphItemY := 3;
    Result.Down5.All_ContentOffsets := MakeRect(0, 0, 0, 0);
    Result.Down5.All_GlyphBitmap := Result.GRushImageCollection1;
    Result.Down5.All_GlyphWidth := 12;
    Result.Down5.All_GlyphHeight := 12;
    Result.Down5.All_GlyphHAlign := haCenter;
    Result.Down5.All_Spacing := 0;
    Result.Down5.All_DrawFocusRect := FALSE;
    Result.Down6 := PGRushControl(NewGRushButton(Result.TextHorz, '').SetPosition(38, 1).SetSize(18, 19));
    Result.Down6.All_BorderRoundWidth := 0;
    Result.Down6.All_BorderRoundHeight := 0;
    Result.Down6.Down_BorderWidth := 1;
    Result.Down6.Dis_BorderWidth := 1;
    Result.Down6.Def_ShadowOffset := 0;
    Result.Down6.Over_ShadowOffset := 0;
    Result.Down6.Down_ShadowOffset := 255;
    Result.Down6.Dis_ShadowOffset := 0;
    Result.Down6.Over_GlyphItemY := 1;
    Result.Down6.Down_GlyphItemY := 2;
    Result.Down6.Dis_GlyphItemY := 3;
    Result.Down6.All_ContentOffsets := MakeRect(0, 0, 0, 0);
    Result.Down6.All_GlyphBitmap := Result.GRushImageCollection1;
    Result.Down6.All_GlyphWidth := 12;
    Result.Down6.All_GlyphHeight := 12;
    Result.Down6.All_GlyphHAlign := haCenter;
    Result.Down6.All_Spacing := 0;
    Result.Down6.All_DrawFocusRect := FALSE;
    Result.Down7 := PGRushControl(NewGRushButton(Result.TextVert, '').SetPosition(38, 1).SetSize(18, 19));
    Result.Down7.All_BorderRoundWidth := 0;
    Result.Down7.All_BorderRoundHeight := 0;
    Result.Down7.Down_BorderWidth := 1;
    Result.Down7.Dis_BorderWidth := 1;
    Result.Down7.Def_ShadowOffset := 0;
    Result.Down7.Over_ShadowOffset := 0;
    Result.Down7.Down_ShadowOffset := 255;
    Result.Down7.Dis_ShadowOffset := 0;
    Result.Down7.Over_GlyphItemY := 1;
    Result.Down7.Down_GlyphItemY := 2;
    Result.Down7.Dis_GlyphItemY := 3;
    Result.Down7.All_ContentOffsets := MakeRect(0, 0, 0, 0);
    Result.Down7.All_GlyphBitmap := Result.GRushImageCollection1;
    Result.Down7.All_GlyphWidth := 12;
    Result.Down7.All_GlyphHeight := 12;
    Result.Down7.All_GlyphHAlign := haCenter;
    Result.Down7.All_Spacing := 0;
    Result.Down7.All_DrawFocusRect := FALSE;

    Result.Prop := Prop;
    with Result^ do begin
        Styles := TKOLGRushSplitterStyles(Prop.GetOrdValue);
        Component := (Styles.Owner as MCKGRushControls.TKOLGRushSplitter);
        TryResize(Control, Component.Width, Component.Height);
        if Assigned(Component.imagecollection) then begin
            Control.All_GlyphBitmap := Component.imagecollection.LoadBitmap;
            Control.All_GlyphBitmap.Free;
        end;
        Control.Caption := Component.Caption;
        Caption.Caption := Component.Caption;
        Control.Font.FontHeight := Component.Font.FontHeight;
        Control.Font.FontWidth := Component.Font.FontWidth;
        //Control.Font.FontPitch := Component.Font.FontPitch;
        Control.Font.FontStyle := KOL.TFontStyle(Component.Font.FontStyle);
        Control.Font.FontCharset := Component.Font.FontCharset;
        //Control.Font.FontQuality := Component.Font.FontQuality;
        Control.Font.FontOrientation := Component.Font.FontOrientation;
        Control.Font.FontWeight := Component.Font.FontWeight;
        Control.Font.FontName := Component.Font.FontName;
    end;

    Result.Col1.OnClick := Result.Col1Click;
    Result.Col1.OnPaint := Result.Col1Paint;
    Result.Col2.OnClick := Result.Col2Click;
    Result.Col2.OnPaint := Result.Col1Paint;
    Result.Col3.OnClick := Result.Col3Click;
    Result.Col3.OnPaint := Result.Col1Paint;
    Result.Col4.OnClick := Result.Col4Click;
    Result.Col4.OnPaint := Result.Col1Paint;
    Result.Col5.OnClick := Result.Col5Click;
    Result.Col5.OnPaint := Result.Col1Paint;
    Result.Col6.OnClick := Result.Col6Click;
    Result.Col6.OnPaint := Result.Col1Paint;
    Result.B.Color := clWindow;
    Result.B.OnEnter := Result.BorderWiEnter;
    Result.B.OnLeave := Result.BLeave;
    Result.BorderHe.Color := clWindow;
    Result.BorderHe.OnEnter := Result.BorderWiEnter;
    Result.BorderHe.OnLeave := Result.BorderHeLeave;
    Result.BorderWi.Color := clWindow;
    Result.BorderWi.OnEnter := Result.BorderWiEnter;
    Result.BorderWi.OnLeave := Result.BorderWiLeave;
    Result.BorderWidth.Color := clWindow;
    Result.BorderWidth.OnEnter := Result.BorderWiEnter;
    Result.BorderWidth.OnLeave := Result.BorderWidthLeave;
    Result.ButtonCancel.OnClick := Result.ButtonCancelClick;
    Result.GRushImageCollection1.Free;
    Result.ButtonOK.OnClick := Result.ButtonOKClick;
    Result.Caption.Color := clWindow;
    Result.Caption.OnChange := Result.CaptionChange;
    Result.DotsCount.Color := clWindow;
    Result.DotsCount.OnEnter := Result.BorderWiEnter;
    Result.DotsCount.OnLeave := Result.DotsCountLeave;
    Result.GlyphHeight.Color := clWindow;
    Result.GlyphHeight.OnEnter := Result.BorderWiEnter;
    Result.GlyphHeight.OnLeave := Result.GlyphHeightLeave;
    Result.GlyphWidth.Color := clWindow;
    Result.GlyphWidth.OnEnter := Result.BorderWiEnter;
    Result.GlyphWidth.OnLeave := Result.GlyphWidthLeave;
    Result.GlyphX.Color := clWindow;
    Result.GlyphX.OnEnter := Result.BorderWiEnter;
    Result.GlyphX.OnLeave := Result.GlyphXLeave;
    Result.GlyphY.Color := clWindow;
    Result.GlyphY.OnEnter := Result.BorderWiEnter;
    Result.GlyphY.OnLeave := Result.GlyphYLeave;
    Result.L.Color := clWindow;
    Result.L.OnEnter := Result.BorderWiEnter;
    Result.L.OnLeave := Result.LLeave;
    Result.R.Color := clWindow;
    Result.R.OnEnter := Result.BorderWiEnter;
    Result.R.OnLeave := Result.RLeave;
    Result.ShadowOffset.Color := clWindow;
    Result.ShadowOffset.OnEnter := Result.BorderWiEnter;
    Result.ShadowOffset.OnLeave := Result.ShadowOffsetLeave;
    Result.Spacing.Color := clWindow;
    Result.Spacing.OnEnter := Result.BorderWiEnter;
    Result.Spacing.OnLeave := Result.SpacingLeave;
    Result.T.Color := clWindow;
    Result.T.OnEnter := Result.BorderWiEnter;
    Result.T.OnLeave := Result.TLeave;
    Result.Down1.OnClick := Result.Down1Click;
    Result.Down2.OnClick := Result.Down2Click;
    Result.Down3.OnClick := Result.Down3Click;
    Result.Down4.OnClick := Result.Down4Click;
    Result.Down5.OnClick := Result.Down5Click;
    Result.Down6.OnClick := Result.Down6Click;
    Result.Down7.OnClick := Result.Down7Click;
    Result.GRushButton1.OnClick := Result.GRushButton1Click;
    Result.GRushButton10.OnClick := Result.GRushButton10Click;
    Result.GRushButton11.OnClick := Result.GRushButton11Click;
    Result.GRushButton12.OnClick := Result.GRushButton12Click;
    Result.GRushButton13.OnClick := Result.GRushButton13Click;
    Result.GRushButton14.OnClick := Result.GRushButton14Click;
    Result.GRushButton15.OnClick := Result.GRushButton15Click;
    Result.GRushButton16.OnClick := Result.GRushButton16Click;
    Result.GRushButton17.OnClick := Result.GRushButton17Click;
    Result.GRushButton18.OnClick := Result.GRushButton18Click;
    Result.GRushButton19.OnClick := Result.GRushButton19Click;
    Result.GRushButton2.OnClick := Result.GRushButton2Click;
    Result.GRushButton20.OnClick := Result.GRushButton20Click;
    Result.GRushButton21.OnClick := Result.GRushButton21Click;
    Result.GRushButton3.OnClick := Result.GRushButton3Click;
    Result.GRushButton4.OnClick := Result.GRushButton4Click;
    Result.GRushButton5.OnClick := Result.GRushButton5Click;
    Result.GRushButton6.OnClick := Result.GRushButton6Click;
    Result.GRushButton7.OnClick := Result.GRushButton7Click;
    Result.GRushButton8.OnClick := Result.GRushButton8Click;
    Result.GRushButton9.OnClick := Result.GRushButton9Click;
    Result.GlyphHorz.OnSelChange := Result.GlyphHorzSelChange;
    Result.GlyphVert.OnSelChange := Result.GlyphVertSelChange;
    Result.GradStyles.OnSelChange := Result.GradStylesSelChange;
    Result.StatesList.OnSelChange := Result.StatesListSelChange;
    Result.TextHorz.OnSelChange := Result.TextHorzSelChange;
    Result.TextVert.OnSelChange := Result.TextVertSelChange;
    Result.UpdateSpeed.OnSelChange := Result.UpdateSpeedSelChange;
    Result.AntiAliasing.OnClick := Result.AntiAliasingClick;
    Result.AntiAliasing.OnRecalcRects := Result.CheckEnabledRecalcRects;
    Result.CheckEnabled.OnClick := Result.CheckEnabledClick;
    Result.CheckEnabled.OnRecalcRects := Result.CheckEnabledRecalcRects;
    Result.CheckTransparent.OnClick := Result.CheckTransparentClick;
    Result.CheckTransparent.OnRecalcRects := Result.CheckEnabledRecalcRects;
    Result.CropTopFirst.OnClick := Result.CropTopFirstClick;
    Result.CropTopFirst.OnRecalcRects := Result.CheckEnabledRecalcRects;
    Result.DrawGlyph.OnClick := Result.DrawGlyphClick;
    Result.DrawGlyph.OnRecalcRects := Result.CheckEnabledRecalcRects;
    Result.DrawText.OnClick := Result.DrawTextClick;
    Result.DrawText.OnRecalcRects := Result.CheckEnabledRecalcRects;
    Result.GlyphAttached.OnClick := Result.GlyphAttachedClick;
    Result.GlyphAttached.OnRecalcRects := Result.CheckEnabledRecalcRects;
    Result.GRushPanel3.OnMouseDown := Result.GRushPanel3MouseDown;
    Result.WordWrap.OnClick := Result.WordWrapClick;
    Result.WordWrap.OnRecalcRects := Result.CheckEnabledRecalcRects;
    Result.Form.CenterOnParent.CanResize := False;
    Result.GRushButton15Click(Result.GRushButton15);
    Result.Form.Icon := THandle(-1);

end;

procedure TSplitterEditor.KOLForm1BeforeCreateWindow(Sender: PObj);
begin
    Form.Font;
end;

procedure TSplitterEditor.KOLForm1FormCreate(Sender: PObj);
begin

    StatesList.CurIndex := 1;
    StatesListSelChange(StatesList);

    Antialiasing.Checked := Control.All_AntiAliasing;
    CropTopFirst.Checked := Control.All_CropTopFirst;
    GlyphAttached.Checked := Control.All_GlyphAttached;
    DrawGlyph.Checked := Control.All_DrawGlyph;
    DrawText.Checked := Control.All_DrawText;
    WordWrap.Checked := TRUE;
    GlyphHorz.CurIndex := Integer(Control.All_GlyphHAlign);
    GlyphVert.CurIndex := Integer(Control.All_GlyphVAlign);
    TextHorz.CurIndex := Integer(Control.All_TextHAlign);
    TextVert.CurIndex := Integer(Control.All_TextVAlign);
    GlyphWidth.Text := int2str(Control.All_GlyphWidth);
    GlyphHeight.Text := int2str(Control.All_GlyphHeight);
    L.Text := int2str(Control.All_ContentOffsets.Left);
    T.Text := int2str(Control.All_ContentOffsets.Top);
    R.Text := int2str(Control.All_ContentOffsets.Right);
    B.Text := int2str(Control.All_ContentOffsets.Bottom);
    Spacing.Text := int2str(Control.All_Spacing);
    DotsCount.Text := int2str(Control.All_SplitterDotsCount);
    UpdateSpeed.CurIndex := Integer(Control.All_UpdateSpeed);
end;

procedure TSplitterEditor.Down1Click(Sender: PObj);
begin
    StatesList.DroppedDown := TRUE;
end;

procedure TSplitterEditor.Down2Click(Sender: PObj);
begin
    GradStyles.DroppedDown := TRUE;
end;

procedure TSplitterEditor.CheckEnabledClick(Sender: PObj);
begin
    Control.Enabled := CheckEnabled.Checked;
end;

procedure TSplitterEditor.CheckTransparentClick(Sender: PObj);
begin
    Control.Transparent := CheckTransparent.Checked;
    Control.Invalidate;
end;

procedure TSplitterEditor.Down3Click(Sender: PObj);
begin
    UpdateSpeed.DroppedDown := TRUE;
end;

procedure TSplitterEditor.Down4Click(Sender: PObj);
begin
    GlyphHorz.DroppedDown := TRUE;
end;

procedure TSplitterEditor.Down5Click(Sender: PObj);
begin
    GlyphVert.DroppedDown := TRUE;
end;

procedure TSplitterEditor.Down6Click(Sender: PObj);
begin
    TextHorz.DroppedDown := TRUE;
end;

procedure TSplitterEditor.Down7Click(Sender: PObj);
begin
    TextVert.DroppedDown := TRUE;
end;

procedure TSplitterEditor.GradStylesSelChange(Sender: PObj);
begin
    case StatesList.CurIndex of
        1:
            begin
                Control.Def_GradientStyle := TGRushGradientStyle(GradStyles.CurIndex);
            end;
        2:
            begin
                Control.Over_GradientStyle := TGRushGradientStyle(GradStyles.CurIndex);
            end;
        3:
            begin
                Control.Down_GradientStyle := TGRushGradientStyle(GradStyles.CurIndex);
            end;
        4:
            begin
                Control.Dis_GradientStyle := TGRushGradientStyle(GradStyles.CurIndex);
            end;
        0:
            begin
                Control.All_GradientStyle := TGRushGradientStyle(GradStyles.CurIndex);
            end;
    end;
    Control.SetAllNeedUpdate;
    Control.Invalidate;
end;

procedure TSplitterEditor.Col1Click(Sender: PObj);
begin
    case StatesList.CurIndex of
        1:
            begin
                CD1.Color := Control.Def_ColorOuter;
                if CD1.Execute then
                    Control.Def_ColorOuter := CD1.Color;
            end;
        2:
            begin
                CD1.Color := Control.Over_ColorOuter;
                if CD1.Execute then
                    Control.Over_ColorOuter := CD1.Color;
            end;
        3:
            begin
                CD1.Color := Control.Down_ColorOuter;
                if CD1.Execute then
                    Control.Down_ColorOuter := CD1.Color;
            end;
        4:
            begin
                CD1.Color := Control.Dis_ColorOuter;
                if CD1.Execute then
                    Control.Dis_ColorOuter := CD1.Color;
            end;
        0:
            begin
                CD1.Color := Control.Def_ColorOuter;
                if CD1.Execute then
                    Control.All_ColorOuter := CD1.Color;
            end;
    end;
    if StatesList.CurIndex <> 0 then
        Col1.Color := CD1.Color;
    Control.SetAllNeedUpdate;
    Control.Invalidate;
end;

procedure TSplitterEditor.Col2Click(Sender: PObj);
begin
    case StatesList.CurIndex of
        1:
            begin
                CD1.Color := Control.Def_ColorFrom;
                if CD1.Execute then
                    Control.Def_ColorFrom := CD1.Color;
            end;
        2:
            begin
                CD1.Color := Control.Over_ColorFrom;
                if CD1.Execute then
                    Control.Over_ColorFrom := CD1.Color;
            end;
        3:
            begin
                CD1.Color := Control.Down_ColorFrom;
                if CD1.Execute then
                    Control.Down_ColorFrom := CD1.Color;
            end;
        4:
            begin
                CD1.Color := Control.Dis_ColorFrom;
                if CD1.Execute then
                    Control.Dis_ColorFrom := CD1.Color;
            end;
        0:
            begin
                CD1.Color := Control.Def_ColorFrom;
                if CD1.Execute then
                    Control.All_ColorFrom := CD1.Color;
            end;
    end;
    if StatesList.CurIndex <> 0 then
        Col2.Color := CD1.Color;
    Control.SetAllNeedUpdate;
    Control.Invalidate;
end;

procedure TSplitterEditor.Col3Click(Sender: PObj);
begin
    case StatesList.CurIndex of
        1:
            begin
                CD1.Color := Control.Def_ColorTo;
                if CD1.Execute then
                    Control.Def_ColorTo := CD1.Color;
            end;
        2:
            begin
                CD1.Color := Control.Over_ColorTo;
                if CD1.Execute then
                    Control.Over_ColorTo := CD1.Color;
            end;
        3:
            begin
                CD1.Color := Control.Down_ColorTo;
                if CD1.Execute then
                    Control.Down_ColorTo := CD1.Color;
            end;
        4:
            begin
                CD1.Color := Control.Dis_ColorTo;
                if CD1.Execute then
                    Control.Dis_ColorTo := CD1.Color;
            end;
        0:
            begin
                CD1.Color := Control.Def_ColorTo;
                if CD1.Execute then
                    Control.All_ColorTo := CD1.Color;
            end;
    end;
    if StatesList.CurIndex <> 0 then
        Col3.Color := CD1.Color;
    Control.SetAllNeedUpdate;
    Control.Invalidate;
end;

procedure TSplitterEditor.Col4Click(Sender: PObj);
begin
    case StatesList.CurIndex of
        1:
            begin
                CD1.Color := Control.Def_BorderColor;
                if CD1.Execute then
                    Control.Def_BorderColor := CD1.Color;
            end;
        2:
            begin
                CD1.Color := Control.Over_BorderColor;
                if CD1.Execute then
                    Control.Over_BorderColor := CD1.Color;
            end;
        3:
            begin
                CD1.Color := Control.Down_BorderColor;
                if CD1.Execute then
                    Control.Down_BorderColor := CD1.Color;
            end;
        4:
            begin
                CD1.Color := Control.Dis_BorderColor;
                if CD1.Execute then
                    Control.Dis_BorderColor := CD1.Color;
            end;
        0:
            begin
                CD1.Color := Control.Def_BorderColor;
                if CD1.Execute then
                    Control.All_BorderColor := CD1.Color;
            end;
    end;
    if StatesList.CurIndex <> 0 then
        Col4.Color := CD1.Color;
    Control.SetAllNeedUpdate;
    Control.Invalidate;
end;

procedure TSplitterEditor.Col5Click(Sender: PObj);
begin
    case StatesList.CurIndex of
        1:
            begin
                CD1.Color := Control.Def_ColorText;
                if CD1.Execute then
                    Control.Def_ColorText := CD1.Color;
            end;
        2:
            begin
                CD1.Color := Control.Over_ColorText;
                if CD1.Execute then
                    Control.Over_ColorText := CD1.Color;
            end;
        3:
            begin
                CD1.Color := Control.Down_ColorText;
                if CD1.Execute then
                    Control.Down_ColorText := CD1.Color;
            end;
        4:
            begin
                CD1.Color := Control.Dis_ColorText;
                if CD1.Execute then
                    Control.Dis_ColorText := CD1.Color;
            end;
        0:
            begin
                CD1.Color := Control.Def_ColorText;
                if CD1.Execute then
                    Control.All_ColorText := CD1.Color;
            end;
    end;
    if StatesList.CurIndex <> 0 then
        Col5.Color := CD1.Color;
    Control.SetAllNeedUpdate;
    Control.Invalidate;
end;

procedure TSplitterEditor.Col6Click(Sender: PObj);
begin
    case StatesList.CurIndex of
        1:
            begin
                CD1.Color := Control.Def_ColorShadow;
                if CD1.Execute then
                    Control.Def_ColorShadow := CD1.Color;
            end;
        2:
            begin
                CD1.Color := Control.Over_ColorShadow;
                if CD1.Execute then
                    Control.Over_ColorShadow := CD1.Color;
            end;
        3:
            begin
                CD1.Color := Control.Down_ColorShadow;
                if CD1.Execute then
                    Control.Down_ColorShadow := CD1.Color;
            end;
        4:
            begin
                CD1.Color := Control.Dis_ColorShadow;
                if CD1.Execute then
                    Control.Dis_ColorShadow := CD1.Color;
            end;
        0:
            begin
                CD1.Color := Control.Def_ColorShadow;
                if CD1.Execute then
                    Control.All_ColorShadow := CD1.Color;
            end;
    end;
    if StatesList.CurIndex <> 0 then
        Col6.Color := CD1.Color;
    Control.SetAllNeedUpdate;
    Control.Invalidate;
end;

procedure TSplitterEditor.StatesListSelChange(Sender: PObj);
begin
    case StatesList.CurIndex of
        1:
            begin
                Col1.Color := Control.Def_ColorOuter;
                Col2.Color := Control.Def_ColorFrom;
                Col3.Color := Control.Def_ColorTo;
                Col4.Color := Control.Def_BorderColor;
                Col5.Color := Control.Def_ColorText;
                Col6.Color := Control.Def_ColorShadow;
                GradStyles.CurIndex := Integer(Control.Def_GradientStyle);
                BorderWidth.Text := int2str(Control.Def_BorderWidth);
                ShadowOffset.Text := int2str(Control.Def_ShadowOffset);
                BorderWi.Text := int2str(Control.Def_BorderRoundWidth);
                BorderHe.Text := int2str(Control.Def_BorderRoundHeight);
                GlyphX.Text := int2str(Control.Def_GlyphItemX);
                GlyphY.Text := int2str(Control.Def_GlyphItemY);
            end;
        2:
            begin
                Col1.Color := Control.Over_ColorOuter;
                Col2.Color := Control.Over_ColorFrom;
                Col3.Color := Control.Over_ColorTo;
                Col4.Color := Control.Over_BorderColor;
                Col5.Color := Control.Over_ColorText;
                Col6.Color := Control.Over_ColorShadow;
                GradStyles.CurIndex := Integer(Control.Over_GradientStyle);
                BorderWidth.Text := int2str(Control.Over_BorderWidth);
                ShadowOffset.Text := int2str(Control.Over_ShadowOffset);
                BorderWi.Text := int2str(Control.Over_BorderRoundWidth);
                BorderHe.Text := int2str(Control.Over_BorderRoundHeight);
                GlyphX.Text := int2str(Control.Over_GlyphItemX);
                GlyphY.Text := int2str(Control.Over_GlyphItemY);
            end;
        3:
            begin
                Col1.Color := Control.Down_ColorOuter;
                Col2.Color := Control.Down_ColorFrom;
                Col3.Color := Control.Down_ColorTo;
                Col4.Color := Control.Down_BorderColor;
                Col5.Color := Control.Down_ColorText;
                Col6.Color := Control.Down_ColorShadow;
                GradStyles.CurIndex := Integer(Control.Down_GradientStyle);
                BorderWidth.Text := int2str(Control.Down_BorderWidth);
                ShadowOffset.Text := int2str(Control.Down_ShadowOffset);
                BorderWi.Text := int2str(Control.Down_BorderRoundWidth);
                BorderHe.Text := int2str(Control.Down_BorderRoundHeight);
                GlyphX.Text := int2str(Control.Down_GlyphItemX);
                GlyphY.Text := int2str(Control.Down_GlyphItemY);
            end;
        4:
            begin
                Col1.Color := Control.Dis_ColorOuter;
                Col2.Color := Control.Dis_ColorFrom;
                Col3.Color := Control.Dis_ColorTo;
                Col4.Color := Control.Dis_BorderColor;
                Col5.Color := Control.Dis_ColorText;
                Col6.Color := Control.Dis_ColorShadow;
                GradStyles.CurIndex := Integer(Control.Dis_GradientStyle);
                BorderWidth.Text := int2str(Control.Dis_BorderWidth);
                ShadowOffset.Text := int2str(Control.Dis_ShadowOffset);
                BorderWi.Text := int2str(Control.Dis_BorderRoundWidth);
                BorderHe.Text := int2str(Control.Dis_BorderRoundHeight);
                GlyphX.Text := int2str(Control.Dis_GlyphItemX);
                GlyphY.Text := int2str(Control.Dis_GlyphItemY);
            end;
        0:
            begin
                Col1.Color := clLtGray;
                Col2.Color := clLtGray;
                Col3.Color := clLtGray;
                Col4.Color := clLtGray;
                Col5.Color := clLtGray;
                Col6.Color := clLtGray;
                GradStyles.CurIndex := 0;
                BorderWidth.Text := '0';
                ShadowOffset.Text := '0';
                BorderWi.Text := '0';
                BorderHe.Text := '0';
                GlyphX.Text := '0';
                GlyphY.Text := '0';
            end;
    end;
end;

procedure TSplitterEditor.UpdateSpeedSelChange(Sender: PObj);
begin
    Control.All_UpdateSpeed := TGRushSpeed(UpdateSpeed.CurIndex);
end;

procedure TSplitterEditor.AntiAliasingClick(Sender: PObj);
begin
    Control.All_AntiAliasing := AntiAliasing.Checked;
    Control.SetAllNeedUpdate;
    Control.Invalidate;
end;

procedure TSplitterEditor.DrawGlyphClick(Sender: PObj);
begin
    Control.All_DrawGlyph := DrawGlyph.Checked;
    Control.SetAllNeedUpdate;
    Control.Invalidate;
end;

procedure TSplitterEditor.DrawTextClick(Sender: PObj);
begin
    Control.All_DrawText := DrawText.Checked;
    Control.SetAllNeedUpdate;
    Control.Invalidate;
end;

procedure TSplitterEditor.CaptionChange(Sender: PObj);
begin
    Control.Caption := Caption.Text;
end;

procedure TSplitterEditor.GlyphHorzSelChange(Sender: PObj);
begin
    Control.All_GlyphHAlign := TGRushHAlign(GlyphHorz.CurIndex);
    Control.Invalidate;
end;

procedure TSplitterEditor.GlyphVertSelChange(Sender: PObj);
begin
    Control.All_GlyphVAlign := TGRushVAlign(GlyphVert.CurIndex);
    Control.Invalidate;
end;

procedure TSplitterEditor.TextHorzSelChange(Sender: PObj);
begin
    Control.All_TextHAlign := TGRushHAlign(TextHorz.CurIndex);
    Control.Invalidate;
end;

procedure TSplitterEditor.TextVertSelChange(Sender: PObj);
begin
    Control.All_TextVAlign := TGRushVAlign(TextVert.CurIndex);
    Control.Invalidate;
end;

procedure TSplitterEditor.Col1Paint(Sender: PControl; DC: HDC);
var TR: TRect;
    BR: HBRUSH;
begin
    Rectangle(DC, 0, 0, Sender.Width, Sender.Height);
    TR := MakeRect(1, 1, Sender.Width - 1, Sender.Height - 1);
    BR := CreateSolidBrush(Color2RGB(Sender.Color));
    FillRect(DC, TR, BR);
    DeleteObject(BR);
end;

procedure TSplitterEditor.CheckEnabledRecalcRects(Sender: PGRushControl;
    var Rects: TGrushRects);
begin
    OffsetRect(Rects.DownBorderRect, 1, 1);
end;

procedure TSplitterEditor.BorderWiEnter(Sender: PObj);
begin
    Sender.Tag := DWORD(str2int(PControl(Sender).Text));
end;

procedure TSplitterEditor.BorderWiLeave(Sender: PObj);
var Val: DWORD;
begin
    Val := str2int(PControl(Sender).Text);
    if Val = Sender.tag then exit;
    case StatesList.CurIndex of
        1:
            begin
                Control.Def_BorderRoundWidth := Val;
            end;
        2:
            begin
                Control.Over_BorderRoundWidth := Val;
            end;
        3:
            begin
                Control.Down_BorderRoundWidth := Val;
            end;
        4:
            begin
                Control.Dis_BorderRoundWidth := Val;
            end;
        0:
            begin
                Control.All_BorderRoundWidth := Val;
                PControl(Sender).Text := '0';
            end;
    end;
    Control.SetAllNeedUpdate;
    Control.Invalidate;
end;

procedure TSplitterEditor.BorderHeLeave(Sender: PObj);
var Val: DWORD;
begin
    Val := str2int(PControl(Sender).Text);
    if Val = Sender.tag then exit;
    case StatesList.CurIndex of
        1:
            begin
                Control.Def_BorderRoundHeight := Val;
            end;
        2:
            begin
                Control.Over_BorderRoundHeight := Val;
            end;
        3:
            begin
                Control.Down_BorderRoundHeight := Val;
            end;
        4:
            begin
                Control.Dis_BorderRoundHeight := Val;
            end;
        0:
            begin
                Control.All_BorderRoundHeight := Val;
                PControl(Sender).Text := '0';
            end;
    end;
    Control.SetAllNeedUpdate;
    Control.Invalidate;
end;

procedure TSplitterEditor.GlyphXLeave(Sender: PObj);
var Val: DWORD;
begin
    Val := str2int(PControl(Sender).Text);
    if Val = Sender.tag then exit;
    case StatesList.CurIndex of
        1:
            begin
                Control.Def_GlyphItemX := Val;
            end;
        2:
            begin
                Control.Over_GlyphItemX := Val;
            end;
        3:
            begin
                Control.Down_GlyphItemX := Val;
            end;
        4:
            begin
                Control.Dis_GlyphItemX := Val;
            end;
        0:
            begin
                Control.All_GlyphItemX := Val;
                PControl(Sender).Text := '0';
            end;
    end;
    Control.SetAllNeedUpdate;
    Control.Invalidate;
end;

procedure TSplitterEditor.GlyphYLeave(Sender: PObj);
var Val: DWORD;
begin
    Val := str2int(PControl(Sender).Text);
    if Val = Sender.tag then exit;
    case StatesList.CurIndex of
        1:
            begin
                Control.Def_GlyphItemY := Val;
            end;
        2:
            begin
                Control.Over_GlyphItemY := Val;
            end;
        3:
            begin
                Control.Down_GlyphItemY := Val;
            end;
        4:
            begin
                Control.Dis_GlyphItemY := Val;
            end;
        0:
            begin
                Control.All_GlyphItemY := Val;
                PControl(Sender).Text := '0';
            end;
    end;
    Control.SetAllNeedUpdate;
    Control.Invalidate;
end;

procedure TSplitterEditor.GlyphWidthLeave(Sender: PObj);
var Val: DWORD;
begin
    Val := str2int(PControl(Sender).Text);
    if Val = Sender.tag then exit;
    Control.All_GlyphWidth := Val;
    Control.SetAllNeedUpdate;
    Control.Invalidate;
end;

procedure TSplitterEditor.GlyphHeightLeave(Sender: PObj);
var Val: DWORD;
begin
    Val := str2int(PControl(Sender).Text);
    if Val = Sender.tag then exit;
    Control.All_GlyphHeight := Val;
    Control.SetAllNeedUpdate;
    Control.Invalidate;
end;

procedure TSplitterEditor.SpacingLeave(Sender: PObj);
var Val: DWORD;
begin
    Val := str2int(PControl(Sender).Text);
    if Val = Sender.tag then exit;
    Control.All_Spacing := Val;
    Control.SetAllNeedUpdate;
    Control.Invalidate;
end;

procedure TSplitterEditor.LLeave(Sender: PObj);
var TR: TRect;
    Val: integer;
begin
    Val := str2int(PControl(Sender).Text);
    if Val = Integer(Sender.tag) then exit;
    TR := Control.All_ContentOffsets;
    TR.Left := Val;
    Control.All_ContentOffsets := TR;
    Control.SetAllNeedUpdate;
    Control.Invalidate;
end;

procedure TSplitterEditor.TLeave(Sender: PObj);
var TR: TRect;
    Val: integer;
begin
    Val := str2int(PControl(Sender).Text);
    if Val = Integer(Sender.tag) then exit;
    TR := Control.All_ContentOffsets;
    TR.Top := Val;
    Control.All_ContentOffsets := TR;
    Control.SetAllNeedUpdate;
    Control.Invalidate;
end;

procedure TSplitterEditor.RLeave(Sender: PObj);
var TR: TRect;
    Val: integer;
begin
    Val := str2int(PControl(Sender).Text);
    if Val = Integer(Sender.tag) then exit;
    TR := Control.All_ContentOffsets;
    TR.Right := Val;
    Control.All_ContentOffsets := TR;
    Control.SetAllNeedUpdate;
    Control.Invalidate;
end;

procedure TSplitterEditor.BLeave(Sender: PObj);
var TR: TRect;
    Val: integer;
begin
    Val := str2int(PControl(Sender).Text);
    if Val = Integer(Sender.tag) then exit;
    TR := Control.All_ContentOffsets;
    TR.Bottom := Val;
    Control.All_ContentOffsets := TR;
    Control.SetAllNeedUpdate;
    Control.Invalidate;
end;

procedure TSplitterEditor.ShadowOffsetLeave(Sender: PObj);
var Val: integer;
begin
    Val := str2int(PControl(Sender).Text);
    if Val = Integer(Sender.tag) then exit;
    case StatesList.CurIndex of
        1:
            begin
                Control.Def_ShadowOffset := Val;
            end;
        2:
            begin
                Control.Over_ShadowOffset := Val;
            end;
        3:
            begin
                Control.Down_ShadowOffset := Val;
            end;
        4:
            begin
                Control.Dis_ShadowOffset := Val;
            end;
        0:
            begin
                Control.All_ShadowOffset := Val;
                PControl(Sender).Text := '0';
            end;
    end;
    Control.SetAllNeedUpdate;
    Control.Invalidate;
end;

procedure TSplitterEditor.BorderWidthLeave(Sender: PObj);
var Val: DWORD;
begin
    Val := str2int(PControl(Sender).Text);
    if Val = Sender.tag then exit;
    case StatesList.CurIndex of
        1:
            begin
                Control.Def_BorderWidth := Val;
            end;
        2:
            begin
                Control.Over_BorderWidth := Val;
            end;
        3:
            begin
                Control.Down_BorderWidth := Val;
            end;
        4:
            begin
                Control.Dis_BorderWidth := Val;
            end;
        0:
            begin
                Control.All_BorderWidth := Val;
                PControl(Sender).Text := '0';
            end;
    end;
    Control.SetAllNeedUpdate;
    Control.Invalidate;
end;

procedure TSplitterEditor.GRushButton11Click(Sender: PObj);
begin
    GlyphHorz.CurIndex := 0;
    Control.All_GlyphHAlign := haLeft;
    GlyphVert.CurIndex := 1;
    Control.All_GlyphVAlign := vaCenter;
    Control.Invalidate;
end;

procedure TSplitterEditor.GRushButton12Click(Sender: PObj);
begin
    TextHorz.CurIndex := 1;
    Control.All_TextHAlign := haCenter;
    TextVert.CurIndex := 1;
    Control.All_TextVAlign := vaCenter;
    Control.Invalidate;
end;

procedure TSplitterEditor.GRushButton16Click(Sender: PObj);
begin
    L.Text := '4';
    T.Text := '4';
    R.Text := '-4';
    B.Text := '-4';
    Control.All_ContentOffsets := MakeRect(4, 4, -4, -4);
    Control.Invalidate;
end;

procedure TSplitterEditor.GRushButton17Click(Sender: PObj);
begin
    Spacing.Text := '5';
    Control.All_Spacing := 5;
    Control.Invalidate;
end;

procedure TSplitterEditor.GRushButton19Click(Sender: PObj);
begin
    UpdateSpeed.CurIndex := 2;
    Control.All_UpdateSpeed := usVeryFast;
    Control.Invalidate;
end;

procedure TSplitterEditor.GRushButton13Click(Sender: PObj);
begin
    GlyphWidth.Text := '0';
    Control.All_GlyphWidth := 0;
    GlyphHeight.Text := '0';
    Control.All_GlyphHeight := 0;
    Control.Invalidate;
end;

procedure TSplitterEditor.GRushButton10Click(Sender: PObj);
begin
    GlyphX.Text := '0';
    GlyphY.Text := '0';
    case StatesList.CurIndex of
        1:
            begin
                Control.Def_GlyphItemX := 0;
                Control.Def_GlyphItemY := 0;
            end;
        2:
            begin
                Control.Over_GlyphItemX := 0;
                Control.Over_GlyphItemY := 0;
            end;
        3:
            begin
                Control.Down_GlyphItemX := 0;
                Control.Down_GlyphItemY := 0;
            end;
        4:
            begin
                Control.Dis_GlyphItemX := 0;
                Control.Dis_GlyphItemY := 0;
            end;
        0:
            begin
                Control.All_GlyphItemX := 0;
                Control.All_GlyphItemY := 0;
            end;
    end;
    Control.Invalidate;
end;

procedure TSplitterEditor.GRushPanel3MouseDown(Sender: PControl;
    var Mouse: TMouseEventData);
begin
    CD1.Color := GRushPanel3.Def_ColorFrom;
    if CD1.Execute then begin
        GRushPanel3.Def_ColorFrom := CD1.Color;
        CheckEnabled.All_ColorOuter := CD1.Color;
        CheckTransparent.All_ColorOuter := CD1.Color;
        GRushPanel3.InvalidateEx;
    end;
end;

procedure TSplitterEditor.GRushButton9Click(Sender: PObj);
begin
    case StatesList.CurIndex of
        1:
            begin
                Control.Def_BorderRoundWidth := 0;
                Control.Def_BorderRoundHeight := 0;
                BorderWi.Text := '0';
                BorderHe.Text := '0';
            end;
        2:
            begin
                Control.Over_BorderRoundWidth := 0;
                Control.Over_BorderRoundHeight := 0;
                BorderWi.Text := '0';
                BorderHe.Text := '0';
            end;
        3:
            begin
                Control.Down_BorderRoundWidth := 0;
                Control.Down_BorderRoundHeight := 0;
                BorderWi.Text := '0';
                BorderHe.Text := '0';
            end;
        4:
            begin
                Control.Dis_BorderRoundWidth := 0;
                Control.Dis_BorderRoundHeight := 0;
                BorderWi.Text := '0';
                BorderHe.Text := '0';
            end;
        0:
            begin
                Control.All_BorderRoundWidth := 0;
                Control.All_BorderRoundHeight := 0;
                BorderWi.Text := '0';
                BorderHe.Text := '0';
            end;
    end;
    Control.Invalidate;
end;

procedure TSplitterEditor.GRushButton8Click(Sender: PObj);
begin
    case StatesList.CurIndex of
        1:
            begin
                Control.Def_ShadowOffset := 1;
                ShadowOffset.Text := '1';
            end;
        2:
            begin
                Control.Over_ShadowOffset := 1;
                ShadowOffset.Text := '1';
            end;
        3:
            begin
                Control.Down_ShadowOffset := -1;
                ShadowOffset.Text := '-1';
            end;
        4:
            begin
                Control.Dis_ShadowOffset := 2;
                ShadowOffset.Text := '2';
            end;
        0:
            begin
                Control.Def_ShadowOffset := 1;
                Control.Over_ShadowOffset := 1;
                Control.Down_ShadowOffset := -1;
                Control.Dis_ShadowOffset := 2;
                ShadowOffset.Text := '0';
            end;
    end;
    Control.Invalidate;
end;

procedure TSplitterEditor.GRushButton7Click(Sender: PObj);
begin
    case StatesList.CurIndex of
        1:
            begin
                Control.Def_BorderWidth := 0;
                BorderWidth.Text := '0';
            end;
        2:
            begin
                Control.Over_BorderWidth := 0;
                BorderWidth.Text := '0';
            end;
        3:
            begin
                Control.Down_BorderWidth := 0;
                BorderWidth.Text := '0';
            end;
        4:
            begin
                Control.Dis_BorderWidth := 0;
                BorderWidth.Text := '0';
            end;
        0:
            begin
                Control.Def_BorderWidth := 0;
                Control.Over_BorderWidth := 0;
                Control.Down_BorderWidth := 0;
                Control.Dis_BorderWidth := 0;
                BorderWidth.Text := '0';
            end;
    end;
    Control.Invalidate;
end;

procedure TSplitterEditor.GRushButton18Click(Sender: PObj);
begin
    case StatesList.CurIndex of
        1:
            begin
                Control.Def_GradientStyle := gsSolid;
                GradStyles.CurIndex := 0;
            end;
        2:
            begin
                if Control.Align in [caLeft, caRight] then begin
                    Control.Over_GradientStyle := gsHorizontal;
                    GradStyles.CurIndex := 2;
                end else begin
                    Control.Over_GradientStyle := gsVertical;
                    GradStyles.CurIndex := 3;
                end;
            end;
        3:
            begin
                if Control.Align in [caLeft, caRight] then begin
                    Control.Down_GradientStyle := gsHorizontal;
                    GradStyles.CurIndex := 2;
                end else begin
                    Control.Down_GradientStyle := gsVertical;
                    GradStyles.CurIndex := 3;
                end;
            end;
        4:
            begin
                if Control.Align in [caLeft, caRight] then begin
                    Control.Dis_GradientStyle := gsHorizontal;
                    GradStyles.CurIndex := 2;
                end else begin
                    Control.Dis_GradientStyle := gsVertical;
                    GradStyles.CurIndex := 3;
                end;
            end;
        0:
            begin
                if Control.Align in [caLeft, caRight] then begin
                    Control.All_GradientStyle := gsHorizontal;
                end else begin
                    Control.All_GradientStyle := gsVertical;
                end;
                Control.Def_GradientStyle := gsSolid;
                GradStyles.CurIndex := 0;
            end;
    end;
    Control.Invalidate;
end;

procedure TSplitterEditor.GRushButton1Click(Sender: PObj);
begin
    case StatesList.CurIndex of
        1:
            begin
                Control.Def_ColorOuter := clBtnFace;
                Col1.Color := clBtnFace;
            end;
        2:
            begin
                Control.Over_ColorOuter := clBtnFace;
                Col1.Color := clBtnFace;
            end;
        3:
            begin
                Control.Down_ColorOuter := clBtnFace;
                Col1.Color := clBtnFace;
            end;
        4:
            begin
                Control.Dis_ColorOuter := clBtnFace;
                Col1.Color := clBtnFace;
            end;
        0:
            begin
                Control.All_ColorOuter := clBtnFace;
            end;
    end;
    Control.Invalidate;
end;

procedure TSplitterEditor.GRushButton2Click(Sender: PObj);
begin
    case StatesList.CurIndex of
        1:
            begin
                Control.Def_ColorFrom := clBtnFace;
                Col2.Color := clBtnFace;
            end;
        2:
            begin
                Control.Over_ColorFrom := clWhite;
                Col2.Color := clWhite;
            end;
        3:
            begin
                Control.Down_ColorFrom := clWhite;
                Col2.Color := clWhite;
            end;
        4:
            begin
                Control.Dis_ColorFrom := clWhite;
                Col2.Color := clWhite;
            end;
        0:
            begin
                Control.Def_ColorFrom := clBtnFace;
                Control.Over_ColorFrom := clWhite;
                Control.Down_ColorFrom := clWhite;
                Control.Dis_ColorFrom := clWhite;
            end;
    end;
    Control.Invalidate;
end;

procedure TSplitterEditor.GRushButton3Click(Sender: PObj);
begin
    case StatesList.CurIndex of
        1:
            begin
                Control.Def_ColorTo := $00D1BEAF;
                Col3.Color := $00D1BEAF;
            end;
        2:
            begin
                Control.Over_ColorTo := $D0AD95;
                Col3.Color := $D0AD95;
            end;
        3:
            begin
                Control.Down_ColorTo := $C39475;
                Col3.Color := $C39475;
            end;
        4:
            begin
                Control.Dis_ColorTo := $009EACB4;
                Col3.Color := $009EACB4;
            end;
        0:
            begin
                Control.Def_ColorTo := $00D1BEAF;
                Control.Over_ColorTo := $D0AD95;
                Control.Down_ColorTo := $C39475;
                Control.Dis_ColorTo := $009EACB4;
            end;
    end;
    Control.Invalidate;
end;

procedure TSplitterEditor.GRushButton4Click(Sender: PObj);
begin
    case StatesList.CurIndex of
        1:
            begin
                Control.Def_BorderColor := $00A4A0A0;
                Col4.Color := $00A4A0A0;
            end;
        2:
            begin
                Control.Over_BorderColor := $00A4A0A0;
                Col4.Color := $00A4A0A0;
            end;
        3:
            begin
                Control.Down_BorderColor := clGray;
                Col4.Color := clGray;
            end;
        4:
            begin
                Control.Dis_BorderColor := clGray;
                Col4.Color := clGray;
            end;
        0:
            begin
                Control.Def_BorderColor := $00A4A0A0;
                Control.Over_BorderColor := $00A4A0A0;
                Control.Down_BorderColor := clGray;
                Control.Dis_BorderColor := clGray;
            end;
    end;
    Control.Invalidate;
end;

procedure TSplitterEditor.GRushButton5Click(Sender: PObj);
begin
    case StatesList.CurIndex of
        1:
            begin
                Control.Def_ColorText := clBlack;
                Col5.Color := clBlack;
            end;
        2:
            begin
                Control.Over_ColorText := clBlack;
                Col5.Color := clBlack;
            end;
        3:
            begin
                Control.Down_ColorText := clBlack;
                Col5.Color := clBlack;
            end;
        4:
            begin
                Control.Dis_ColorText := clBlack;
                Col5.Color := clBlack;
            end;
        0:
            begin
                Control.All_ColorText := clBlack;
            end;
    end;
    Control.Invalidate;
end;

procedure TSplitterEditor.GRushButton6Click(Sender: PObj);
begin
    case StatesList.CurIndex of
        1:
            begin
                Control.Def_ColorShadow := clWhite;
                Col5.Color := clWhite;
            end;
        2:
            begin
                Control.Over_ColorShadow := clGray;
                Col5.Color := clGray;
            end;
        3:
            begin
                Control.Down_ColorShadow := clGray;
                Col5.Color := clGray;
            end;
        4:
            begin
                Control.Dis_ColorShadow := clGray;
                Col5.Color := clGray;
            end;
        0:
            begin
                Control.All_ColorShadow := clGray;
                Control.Def_ColorShadow := clWhite;
            end;
    end;
    Control.Invalidate;
end;

procedure TSplitterEditor.GRushButton14Click(Sender: PObj);
begin
    GRushButton1Click(GRushButton1);
    GRushButton2Click(GRushButton2);
    GRushButton3Click(GRushButton3);
    GRushButton4Click(GRushButton4);
    GRushButton5Click(GRushButton5);
    GRushButton6Click(GRushButton6);
    GRushButton18Click(GRushButton18);
    GRushButton7Click(GRushButton7);
    GRushButton8Click(GRushButton8);
    GRushButton9Click(GRushButton9);
    GRushButton10Click(GRushButton10);
end;

procedure TSplitterEditor.GRushButton20Click(Sender: PObj);
begin
    StatesList.CurIndex := 0;
    GRushButton14Click(GRushButton14);
    GRushButton11Click(GRushButton11);
    GRushButton12Click(GRushButton12);
    GRushButton13Click(GRushButton13);
    GRushButton16Click(GRushButton16);
    GRushButton17Click(GRushButton17);
    GRushButton19Click(GRushButton19);
    Control.All_AntiAliasing := TRUE;
    Control.All_DrawFocusRect := FALSE;
    Control.All_CropTopFirst := TRUE;
    Control.All_GlyphAttached := FALSE;
    Control.All_DrawGlyph := TRUE;
    Control.All_DrawText := TRUE;
    KOLForm1FormCreate(SplitterEditor);
    Control.Invalidate;
end;

procedure TSplitterEditor.KOLForm1Close(Sender: PObj; var Accept: Boolean);
begin
    Accept := TRUE;
    
    EnableTaskWindows(WindowList);
    SetActiveWindow(ActiveWindow);
    TerminateExecution(KOL.Applet);
end;

procedure TSplitterEditor.GRushButton15Click(Sender: PObj);
var     Data: PGRushData;
begin
    Data := PGRushData(Control.CustomObj);

    if Component.Align in [mirror.caLeft, mirror.caRight] then begin
        TControlAlign((@Control.Align)^) := caLeft;
        Control.Cursor := LoadCursor(0, IDC_SIZEWE);
    end;

    Data.fPSDef.ColorFrom :=            Styles.DefPaintState.ColorFrom;
    Data.fPSDef.ColorTo :=              Styles.DefPaintState.ColorTo;
    Data.fPSDef.ColorOuter :=           Styles.DefPaintState.ColorOuter;
    Data.fPSDef.ColorText :=            Styles.DefPaintState.ColorText;
    Data.fPSDef.ColorShadow :=          Styles.DefPaintState.ColorShadow;
    Data.fPSDef.BorderColor :=          Styles.DefPaintState.BorderColor;
    Data.fPSDef.BorderRoundWidth :=     Styles.DefPaintState.BorderRoundWidth;
    Data.fPSDef.BorderRoundHeight :=    Styles.DefPaintState.BorderRoundHeight;
    Data.fPSDef.BorderWidth :=          Styles.DefPaintState.BorderWidth;
    Data.fPSDef.GradientStyle :=        Styles.DefPaintState.GradientStyle;
    Data.fPSDef.ShadowOffset :=         Styles.DefPaintState.ShadowOffset;
    Data.fPSDef.GlyphItemX :=           Styles.DefPaintState.GlyphItemX;
    Data.fPSDef.GlyphItemY :=           Styles.DefPaintState.GlyphItemY;

    Data.fPSOver.ColorFrom :=           Styles.OverPaintState.ColorFrom;
    Data.fPSOver.ColorTo :=             Styles.OverPaintState.ColorTo;
    Data.fPSOver.ColorOuter :=          Styles.OverPaintState.ColorOuter;
    Data.fPSOver.ColorText :=           Styles.OverPaintState.ColorText;
    Data.fPSOver.ColorShadow :=         Styles.OverPaintState.ColorShadow;
    Data.fPSOver.BorderColor :=         Styles.OverPaintState.BorderColor;
    Data.fPSOver.BorderRoundWidth :=    Styles.OverPaintState.BorderRoundWidth;
    Data.fPSOver.BorderRoundHeight :=   Styles.OverPaintState.BorderRoundHeight;
    Data.fPSOver.BorderWidth :=         Styles.OverPaintState.BorderWidth;
    Data.fPSOver.GradientStyle :=       Styles.OverPaintState.GradientStyle;
    Data.fPSOver.ShadowOffset :=        Styles.OverPaintState.ShadowOffset;
    Data.fPSOver.GlyphItemX :=          Styles.OverPaintState.GlyphItemX;
    Data.fPSOver.GlyphItemY :=          Styles.OverPaintState.GlyphItemY;

    Data.fPSDown.ColorFrom :=           Styles.DownPaintState.ColorFrom;
    Data.fPSDown.ColorTo :=             Styles.DownPaintState.ColorTo;
    Data.fPSDown.ColorOuter :=          Styles.DownPaintState.ColorOuter;
    Data.fPSDown.ColorText :=           Styles.DownPaintState.ColorText;
    Data.fPSDown.ColorShadow :=         Styles.DownPaintState.ColorShadow;
    Data.fPSDown.BorderColor :=         Styles.DownPaintState.BorderColor;
    Data.fPSDown.BorderRoundWidth :=    Styles.DownPaintState.BorderRoundWidth;
    Data.fPSDown.BorderRoundHeight :=   Styles.DownPaintState.BorderRoundHeight;
    Data.fPSDown.BorderWidth :=         Styles.DownPaintState.BorderWidth;
    Data.fPSDown.GradientStyle :=       Styles.DownPaintState.GradientStyle;
    Data.fPSDown.ShadowOffset :=        Styles.DownPaintState.ShadowOffset;
    Data.fPSDown.GlyphItemX :=          Styles.DownPaintState.GlyphItemX;
    Data.fPSDown.GlyphItemY :=          Styles.DownPaintState.GlyphItemY;

    Data.fPSDis.ColorFrom :=            Styles.DisPaintState.ColorFrom;
    Data.fPSDis.ColorTo :=              Styles.DisPaintState.ColorTo;
    Data.fPSDis.ColorOuter :=           Styles.DisPaintState.ColorOuter;
    Data.fPSDis.ColorText :=            Styles.DisPaintState.ColorText;
    Data.fPSDis.ColorShadow :=          Styles.DisPaintState.ColorShadow;
    Data.fPSDis.BorderColor :=          Styles.DisPaintState.BorderColor;
    Data.fPSDis.BorderRoundWidth :=     Styles.DisPaintState.BorderRoundWidth;
    Data.fPSDis.BorderRoundHeight :=    Styles.DisPaintState.BorderRoundHeight;
    Data.fPSDis.BorderWidth :=          Styles.DisPaintState.BorderWidth;
    Data.fPSDis.GradientStyle :=        Styles.DisPaintState.GradientStyle;
    Data.fPSDis.ShadowOffset :=         Styles.DisPaintState.ShadowOffset;
    Data.fPSDis.GlyphItemX :=           Styles.DisPaintState.GlyphItemX;
    Data.fPSDis.GlyphItemY :=           Styles.DisPaintState.GlyphItemY;

    Data.fContentOffsets.Left :=        Styles.ContentOffsets.Left;
    Data.fContentOffsets.Top :=         Styles.ContentOffsets.Top;
    Data.fContentOffsets.Right :=       Styles.ContentOffsets.Right;
    Data.fContentOffsets.Bottom :=      Styles.ContentOffsets.Bottom;

    if Styles.GlyphWidth <> 0 then
        Data.fGlyphWidth :=             Styles.GlyphWidth
    else if Assigned(Component.imagecollection) then
        if (Component.imagecollection.ItemWidth <> 0) then
            Data.fGlyphWidth :=         Component.imagecollection.ItemWidth;
    if Styles.GlyphHeight <> 0 then
        Data.fGlyphHeight :=            Styles.GlyphHeight
    else if Assigned(Component.imagecollection) then
        if (Component.imagecollection.ItemHeight <> 0) then
            Data.fGlyphHeight :=        Component.imagecollection.ItemHeight;

    Data.fSplitterDotsCount :=          Styles.SplitterDotsCount;
    Data.fCheckMetric :=                0;//Styles.CheckMetric;
    Data.fColorCheck :=                 0;//Styles.ColorCheck;
    Data.fGlyphVAlign :=                Styles.GlyphVAlign;
    Data.fGlyphHAlign :=                Styles.GlyphHAlign;
    Data.fTextVAlign :=                 Styles.TextVAlign;
    Data.fTextHAlign :=                 Styles.TextHAlign;
    Data.fDrawGlyph :=                  Styles.DrawGlyph;
    Data.fDrawText :=                   Styles.DrawText;
    Data.fDrawFocusRect :=              FALSE;//Styles.DrawFocusRect;
    Data.fDrawProgress :=               FALSE;//Styles.DrawProgress;
    Data.fDrawProgressRect :=           FALSE;//Styles.DrawProgressRect;
    Data.fGlyphAttached :=              FALSE;//Styles.GlyphAttached;
    Data.fCropTopFirst :=               TRUE;//Styles.CropTopFirst;
    Data.fAntiAliasing :=               Styles.AntiAliasing;
    Data.fProgressVertical :=           FALSE;//Styles.ProgressVertical;
    Data.fUpdateSpeed :=                Styles.UpdateSpeed;
    Data.fSpacing :=                    Styles.Spacing;

    KOLForm1FormCreate(SplitterEditor);

    Control.SetAllNeedUpdate;
    Control.Invalidate;
end;

procedure TSplitterEditor.ButtonOKClick(Sender: PObj);
var     Data: PGRushData;
begin
    Data := PGRushData(Control.CustomObj);

    Styles.DefPaintState.ColorFrom :=               Data.fPSDef.ColorFrom;
    Styles.DefPaintState.ColorTo :=                 Data.fPSDef.ColorTo;
    Styles.DefPaintState.ColorOuter :=              Data.fPSDef.ColorOuter;
    Styles.DefPaintState.ColorText :=               Data.fPSDef.ColorText;
    Styles.DefPaintState.ColorShadow :=             Data.fPSDef.ColorShadow;
    Styles.DefPaintState.BorderColor :=             Data.fPSDef.BorderColor;
    Styles.DefPaintState.BorderRoundWidth :=        Data.fPSDef.BorderRoundWidth;
    Styles.DefPaintState.BorderRoundHeight :=       Data.fPSDef.BorderRoundHeight;
    Styles.DefPaintState.BorderWidth :=             Data.fPSDef.BorderWidth;
    Styles.DefPaintState.GradientStyle :=           Data.fPSDef.GradientStyle;
    Styles.DefPaintState.ShadowOffset :=            Data.fPSDef.ShadowOffset;
    Styles.DefPaintState.GlyphItemX :=              Data.fPSDef.GlyphItemX;
    Styles.DefPaintState.GlyphItemY :=              Data.fPSDef.GlyphItemY;

    Styles.OverPaintState.ColorFrom :=              Data.fPSOver.ColorFrom;
    Styles.OverPaintState.ColorTo :=                Data.fPSOver.ColorTo;
    Styles.OverPaintState.ColorOuter :=             Data.fPSOver.ColorOuter;
    Styles.OverPaintState.ColorText :=              Data.fPSOver.ColorText;
    Styles.OverPaintState.ColorShadow :=            Data.fPSOver.ColorShadow;
    Styles.OverPaintState.BorderColor :=            Data.fPSOver.BorderColor;
    Styles.OverPaintState.BorderRoundWidth :=       Data.fPSOver.BorderRoundWidth;
    Styles.OverPaintState.BorderRoundHeight :=      Data.fPSOver.BorderRoundHeight;
    Styles.OverPaintState.BorderWidth :=            Data.fPSOver.BorderWidth;
    Styles.OverPaintState.GradientStyle :=          Data.fPSOver.GradientStyle;
    Styles.OverPaintState.ShadowOffset :=           Data.fPSOver.ShadowOffset;
    Styles.OverPaintState.GlyphItemX :=             Data.fPSOver.GlyphItemX;
    Styles.OverPaintState.GlyphItemY :=             Data.fPSOver.GlyphItemY;

    Styles.DownPaintState.ColorFrom :=              Data.fPSDown.ColorFrom;
    Styles.DownPaintState.ColorTo :=                Data.fPSDown.ColorTo;
    Styles.DownPaintState.ColorOuter :=             Data.fPSDown.ColorOuter;
    Styles.DownPaintState.ColorText :=              Data.fPSDown.ColorText;
    Styles.DownPaintState.ColorShadow :=            Data.fPSDown.ColorShadow;
    Styles.DownPaintState.BorderColor :=            Data.fPSDown.BorderColor;
    Styles.DownPaintState.BorderRoundWidth :=       Data.fPSDown.BorderRoundWidth;
    Styles.DownPaintState.BorderRoundHeight :=      Data.fPSDown.BorderRoundHeight;
    Styles.DownPaintState.BorderWidth :=            Data.fPSDown.BorderWidth;
    Styles.DownPaintState.GradientStyle :=          Data.fPSDown.GradientStyle;
    Styles.DownPaintState.ShadowOffset :=           Data.fPSDown.ShadowOffset;
    Styles.DownPaintState.GlyphItemX :=             Data.fPSDown.GlyphItemX;
    Styles.DownPaintState.GlyphItemY :=             Data.fPSDown.GlyphItemY;

    Styles.DisPaintState.ColorFrom :=               Data.fPSDis.ColorFrom;
    Styles.DisPaintState.ColorTo :=                 Data.fPSDis.ColorTo;
    Styles.DisPaintState.ColorOuter :=              Data.fPSDis.ColorOuter;
    Styles.DisPaintState.ColorText :=               Data.fPSDis.ColorText;
    Styles.DisPaintState.ColorShadow :=             Data.fPSDis.ColorShadow;
    Styles.DisPaintState.BorderColor :=             Data.fPSDis.BorderColor;
    Styles.DisPaintState.BorderRoundWidth :=        Data.fPSDis.BorderRoundWidth;
    Styles.DisPaintState.BorderRoundHeight :=       Data.fPSDis.BorderRoundHeight;
    Styles.DisPaintState.BorderWidth :=             Data.fPSDis.BorderWidth;
    Styles.DisPaintState.GradientStyle :=           Data.fPSDis.GradientStyle;
    Styles.DisPaintState.ShadowOffset :=            Data.fPSDis.ShadowOffset;
    Styles.DisPaintState.GlyphItemX :=              Data.fPSDis.GlyphItemX;
    Styles.DisPaintState.GlyphItemY :=              Data.fPSDis.GlyphItemY;

    Styles.ContentOffsets.Left :=                   Data.fContentOffsets.Left;
    Styles.ContentOffsets.Top :=                    Data.fContentOffsets.Top;
    Styles.ContentOffsets.Right :=                  Data.fContentOffsets.Right;
    Styles.ContentOffsets.Bottom :=                 Data.fContentOffsets.Bottom;

    Styles.GlyphWidth :=                            Data.fGlyphWidth;
    if Assigned(Component.imagecollection) then begin
        if Component.imagecollection.ItemWidth = Data.fGlyphWidth then
            Styles.GlyphWidth := 0;
        if (Component.imagecollection.ItemWidth = 0) and Assigned(Data.fGlyphBitmap) then
            if (DWORD(Data.fGlyphBitmap.Width) = Data.fGlyphWidth) then
                Styles.GlyphWidth := 0;
    end;
    Styles.GlyphHeight :=                            Data.fGlyphHeight;
    if Assigned(Component.imagecollection) then begin
        if Component.imagecollection.ItemHeight = Data.fGlyphHeight then
            Styles.GlyphHeight := 0;
        if (Component.imagecollection.ItemHeight = 0) and Assigned(Data.fGlyphBitmap) then
            if (DWORD(Data.fGlyphBitmap.Height) = Data.fGlyphHeight) then
                Styles.GlyphHeight := 0;
    end;

    Styles.GlyphVAlign :=                           Data.fGlyphVAlign;
    Styles.GlyphHAlign :=                           Data.fGlyphHAlign;
    Styles.TextVAlign :=                            Data.fTextVAlign;
    Styles.TextHAlign :=                            Data.fTextHAlign;
    Styles.DrawGlyph :=                             Data.fDrawGlyph;
    Styles.DrawText :=                              Data.fDrawText;
    Styles.GlyphAttached :=                         FALSE;//Data.fGlyphAttached;
    Styles.CropTopFirst :=                          TRUE;//Data.fCropTopFirst;
    Styles.AntiAliasing :=                          Data.fAntiAliasing;
    Styles.UpdateSpeed :=                           Data.fUpdateSpeed;
    Styles.Spacing :=                               Data.fSpacing;
    Styles.SplitterDotsCount :=                     Data.fSplitterDotsCount;

    Prop.SetOrdValue( Integer(Styles) );
    Form.Close;
end;

procedure TSplitterEditor.ButtonCancelClick(Sender: PObj);
begin
    Form.Close;
end;

procedure TSplitterEditor.CropTopFirstClick(Sender: PObj);
begin
end;

procedure TSplitterEditor.GlyphAttachedClick(Sender: PObj);
begin
end;

procedure TSplitterEditor.WordWrapClick(Sender: PObj);
begin
end;

procedure TSplitterEditor.GRushButton21Click(Sender: PObj);
begin
    DotsCount.Text := '16';
    Control.All_SplitterDotsCount := 16;
    Control.Invalidate;
end;

procedure TSplitterEditor.DotsCountLeave(Sender: PObj);
var Val: DWORD;
begin
    Val := str2int(PControl(Sender).Text);
    if Val = Sender.tag then exit;
    Control.All_SplitterDotsCount := Val;
    Control.Invalidate;
end;






function TSplitterStylesProp.GetAttributes: TPropertyAttributes;
begin
    Result := [paMultiSelect, paSubProperties, paDialog, paReadOnly];
end;

procedure TSplitterStylesProp.Edit;
var     Styles: TKOLGRushSplitterStyles;
begin
    Styles := TKOLGRushSplitterStyles(GetOrdValue);
    if Styles = nil then exit;
    if not (Styles is TKOLGRushSplitterStyles) then exit;

    SplitterEditor := nil;
    AppletTerminated := FALSE;
    try
        NewSplitterEditor(SplitterEditor, Self);
        SplitterEditor.ActiveWindow := GetActiveWindow;
        SplitterEditor.WindowList := DisableTaskWindows(0);
        KOL.Run(KOL.Applet);
    finally
    end;
end;


end.

