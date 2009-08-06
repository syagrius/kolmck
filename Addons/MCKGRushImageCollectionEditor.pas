unit MCKGRushImageCollectionEditor;

//  file: MCKGRushImageCollectionEditor.pas
//  file version: 0.35
//  last modified: 06.02.06
//  package: GRushControls
//  author: Karpinskyj Alexandr aka homm
//      mailto: homm86@mail.ru
//      My humble Web-Page: http://www.homm86.narod.ru

interface

{$I KOLDEF.INC}

uses    Windows,
        Messages,
        ShellAPI,
        KOL,
        KOLGRushControls,
        tinyJPGGIFBMP,
        tinyPNG,
        mirror,
        Classes,
        Controls,
        mckObjs,
        Graphics,
        mckCtrls,
        MCKGRushControls,
        Forms,
        {$IFDEF _D6orHigher}
        DesignEditors,
        DesignIntf;
        {$ELSE}
        DsgnIntf;
        {$ENDIF}


type
    TKOLGRushImageCollectionEditor = class( TComponentEditor )
    private
    protected
    public
        procedure Edit; override;
        procedure ExecuteVerb(Index: Integer); override;
        function GetVerb(Index: Integer): string; override;
        function GetVerbCount: Integer; override;
    end;

    ImageCollectionData= record
        fImageType: TKOLGRushImageCollectionImageType;
        fItemWidth: DWORD;
        fItemHeight: DWORD;
        fDataStream: TMemoryStream;
    end;

    PImageCollectionEditor = ^TImageCollectionEditor;
    TImageCollectionEditor = object (TObj)
        Form: KOL.PControl;
        ScrollBox: PControl;
        ImageShow: PControl;
        ButtonOK: PGRushControl;
        ButtonCancel: PGRushControl;
        ButtonOpen_Close: PGRushControl;
        ButtonSave: PGRushControl;
        OSD: KOL.POpenSaveDialog;
        Collection: ImageCollectionData;
        Comp: TKOLGRushImageCollection;
        Bitmap: KOL.PBitmap;
        ///////////////////
        ActiveWindow: HWnd;
        WindowList: Pointer;
        procedure OKClick(Self_: KOL.PObj);
        procedure CancelClick(Self_: KOL.PObj);
        procedure CloseClick(Self_: KOL.PObj);
        procedure OpenClick(Self_: KOL.PObj);
        procedure SaveClick(Self_: KOL.PObj);
        procedure CalcRects (Sender: PGRushControl; var Rects: TGRushRects);
        procedure DoClose ( Sender: PObj; var Accept: Boolean );
        procedure ImageShowPaint ( Sender: PControl; DC: HDC );
        procedure SetControls;
    end;

procedure Register;

var
  ImageCollectionEditor: PImageCollectionEditor;

procedure NewImageCollectionEditor( var Result: PImageCollectionEditor; Component: TKOLGRushImageCollection );

implementation

procedure Register;
begin
    RegisterComponentEditor( TKOLGRushImageCollection, TKOLGRushImageCollectionEditor );
end;

procedure NewImageCollectionEditor( var Result: PImageCollectionEditor; Component: TKOLGRushImageCollection );
begin
    New(Result, Create);
    with Result^ do begin
        Form := NewForm(nil, Component.Name + ': Edit').SetClientSize(440, 256).CenterOnParent;
        KOL.Applet := Form;
        Form.ExStyle := Form.ExStyle or WS_EX_DLGMODALFRAME or WS_EX_WINDOWEDGE;
        Form.Style := Form.Style and not (WS_MINIMIZEBOX or WS_MAXIMIZEBOX);
        Form.CanResize := FALSE;
        Form.OnClose := DoClose;
        Form.Add2AutoFree(Result);

        ScrollBox := NewScrollBoxEx(Form, esLowered).SetPosition(8, 8).SetSize(320, 240).SetBorder(0);

        ImageShow := NewPanel(ScrollBox, esNone);
        ImageShow.OnPaint := ImageShowPaint;

        ButtonOpen_Close := PGRushControl(NewGRushButton(Result.Form, '').SetSize(96, 24).SetPosition(336, 8));
        ButtonOpen_Close.OnRecalcRects := CalcRects;

        ButtonSave := PGRushControl(NewGRushButton(Result.Form, 'Save as').SetSize(96, 24).SetPosition(336, 40));
        ButtonSave.OnClick := Result.SaveClick;
        ButtonSave.OnRecalcRects := CalcRects;

        ButtonOK := PGRushControl(NewGRushButton(Result.Form, 'OK').SetSize(96, 24).SetPosition(336, 192));
        ButtonOK.OnClick := Result.OKClick;
        ButtonOK.OnRecalcRects := CalcRects;
        ButtonOK.Focused := TRUE;

        ButtonCancel := PGRushControl(NewGRushButton(Result.Form, 'Cancel').SetSize(96, 24).SetPosition(336, 224));
        ButtonCancel.OnClick := Result.CancelClick;
        ButtonCancel.OnRecalcRects := CalcRects;

        OSD := NewOpenSaveDialog('chose file to open', ProjectSourcePath
            , [OSFileMustExist, OSHideReadonly, OSPathMustExist, OSOverwritePrompt] );
        OSD.Filter := 'Jpeg files|*.jpg;*.jpeg|Png files|*.png|Gif files|*.gif|Bmp files|*.bmp|'
            + 'All suported files|*.jpg;*.jpeg;*.png;*.gif;*.bmp|All files|*.*|';
        OSD.FilterIndex := 5;
        OSD.WndOwner := Form.Handle;




        Comp := Component;
        Collection.fImageType := Component.ImageType;
        if assigned(Component.DataStream) then begin
            Collection.fDataStream := TMemoryStream.Create;
            Collection.fDataStream.LoadFromStream(Component.DataStream);
        end;
        Bitmap := Component.LoadBitmap;
        SetControls;
    end;
end;

procedure TKOLGRushImageCollectionEditor.Edit;
begin
    if Component = nil then Exit;
    if not(Component is TKOLGRushImageCollection) then Exit;

    ImageCollectionEditor := nil;
    AppletTerminated := FALSE;
    try
        NewImageCollectionEditor(ImageCollectionEditor, Component as TKOLGRushImageCollection);
        ImageCollectionEditor.ActiveWindow := GetActiveWindow;
        ImageCollectionEditor.WindowList := DisableTaskWindows(0);
        KOL.Run(KOL.Applet);
    finally

    end;
    (Component as TKOLGRushImageCollection).Change;
end;

procedure TKOLGRushImageCollectionEditor.ExecuteVerb(Index: Integer);
begin
    if Index = 0 then
        Edit;
end;

function TKOLGRushImageCollectionEditor.GetVerb(Index: Integer): string;
begin
    if Index = 0 then
        Result := 'Edit component';
end;

function TKOLGRushImageCollectionEditor.GetVerbCount: Integer;
begin
    Result := 1;
end;


procedure TImageCollectionEditor.SetControls;
begin
    if Bitmap = nil then begin
        try
            Collection.fDataStream.Free;
        finally
            Collection.fDataStream := nil;
        end;
        Collection.fImageType := None;
        ButtonOpen_Close.Caption := 'Open';
        ButtonOpen_Close.OnClick := OpenClick;
        ButtonSave.Enabled := FALSE;
        ImageShow.Visible := FALSE;
    end else begin
        ButtonOpen_Close.Caption := 'Free';
        ButtonOpen_Close.OnClick := CloseClick;
        ButtonSave.Enabled := TRUE;
        ImageShow.SetSize(Bitmap.Width, Bitmap.Height);
        ImageShow.Visible := TRUE;
    end;
end;

procedure TImageCollectionEditor.OKClick(Self_: KOL.PObj);
begin
    try
        Comp.DataStream.Free;
    finally
        Comp.DataStream := nil;
    end;
    TKOLGRushImageCollectionImageType((@Comp.ImageType)^) := Collection.fImageType;
    if Comp.ImageType <> None then begin
        Comp.DataStream := TMemoryStream.Create;
        Comp.DataStream.LoadFromStream(Collection.fDataStream);
        Comp.DataStream.Position := 0;
    end;
    Form.Close;
end;

procedure TImageCollectionEditor.CancelClick(Self_: KOL.PObj);
begin
    Form.Close;
end;

procedure TImageCollectionEditor.OpenClick(Self_: KOL.PObj);
var     KOLStream: KOL.PStream;
begin
    OSD.OpenDialog := TRUE;
    if OSD.Execute then begin
        Collection.fDataStream := TMemoryStream.Create;
        Collection.fDataStream.LoadFromFile(OSD.FileName);

        KOLStream := NewExMemoryStream(Collection.fDataStream.Memory, Collection.fDataStream.Size);
        try
            tinyLoadPNG(Bitmap, KOLStream);
        except
            ShowMessage('Произошла ошибка во время попытки декадировать файл как *.png'
                + '. Пожалуста сообщите об этом автору (homm86@mail.ru) и прекрепите'
                + ' проблемный файл если его размер менее мегабайта.');
            try
                Bitmap.Free;
            finally
                Bitmap := nil;
            end;
        end;
        KOLStream.Free;
        if Bitmap <> nil then begin
            Collection.fImageType := PNG;
        end else begin // maybe JPG?
            tinyLoadJPGGIFBMPStream(Bitmap, Collection.fDataStream);
            if Bitmap <> nil then begin
                Collection.fImageType := BMP_GIF_JPG;
            end else begin // not suported
                Collection.fImageType := None;
                ShowMessage ('This file type not suported.');
                try
                    Collection.fDataStream.Free;
                finally
                    Collection.fDataStream := nil;
                end;
                try
                    Bitmap.Free;
                finally
                    Bitmap := nil;
                end;
            end;
        end;
        SetControls;
    end;
end;

procedure TImageCollectionEditor.CloseClick(Self_: KOL.PObj);
begin
    ImageShow.Visible := FALSE;
    ButtonOpen_Close.OnClick := OpenClick;
    ButtonOpen_Close.Caption := 'Open';
    ButtonSave.Enabled := FALSE;
    Collection.fImageType := None;
    try
        Collection.fDataStream.Free;
    finally
        Collection.fDataStream := nil;
    end;
    try
        Bitmap.Free;
    finally
        Bitmap := nil;
    end;
end;

procedure TImageCollectionEditor.SaveClick(Self_: KOL.PObj);
begin
    try
        OSD.OpenDialog := FALSE;
        if OSD.Execute then begin
            Collection.fDataStream.SaveToFile(OSD.FileName);
        end;
    except
        ShowMessage('Не удается сохранить рисунок.');
    end;
end;

procedure TImageCollectionEditor.DoClose ( Sender: PObj; var Accept: Boolean );
begin
    Accept := TRUE;
    try
        Collection.fDataStream.Free;
    finally
        Collection.fDataStream := nil;
    end;
    try
        Bitmap.Free;
    finally
        Bitmap := nil;
    end;
    EnableTaskWindows(WindowList);
    SetActiveWindow(ActiveWindow);
    TerminateExecution(KOL.Applet);
end;

procedure TImageCollectionEditor.CalcRects (Sender: PGRushControl; var Rects: TGRushRects);
begin
    InflateRect(Rects.AlphaRect, -4, -3);
end;

procedure TImageCollectionEditor.ImageShowPaint ( Sender: PControl; DC: HDC );
begin
    if Bitmap <> nil then begin
        BitBlt(DC, 0, 0, Bitmap.Width, Bitmap.Height, Bitmap.Canvas.Handle, 0, 0, SRCCOPY);
    end;
end;

end.
