unit KOLEcmListEdit;
{$DEFINE rbutton_sel}
{* TKOLEcmListEdit - расширение стандартного ListView возможностью редактирования
|=================================================================================
| Version 1.17
|Copyright (C) by ECM, 2004..2006   <ecm@ua.fm>
|This unit conains objects TEcmListEdit.
|<pre>
  Принцип:        ListEdit "жестко" переводится в lvsDetail(lvsDetailNoHeader) и по DBLCLICK либо
                  по Enter или по "редактируемому" символу с клавиатуры
                  (либо программно  StartEdit)переводится в режим
                  "редактирования" - сверху на текущаю строку ListEdit накладывается
                  "встроенный редактор" (по-умолчанию EditBox)
                  В этом режиме можно перемещать фокус ввода между ячейками почти
                  как в StringGrid-е. Выход из режима редактирования - Esc, StopEdit
                  или смена фокуса

  Это конечно не KOLmdvXLGrid - но "по весу" значительно легче ( ~3.9 кБ)

  Прототип(Идея): KOLListEdit - Александр Шахайло - thanks! (Код переписан заново
                  практически полностью)
  Плюсы по сравнению с прототипом (KOLListEdit):

  - Внешний вид более приближен к StringGrid за счет отсутсвия у встроенного
  редактора рамки и более точного выравнивания  редактора внутри ячейки (см. OnColAdjust);

  - Автоматический скроллинг ListView в режиме редактирования (как по вертикали
  так и по горизонтали)+ перемещение между ячейками по горизонтали стрелками влево-вправо;

  - Дополнительные события: OnGetEditText,OnPutEditText,OnStopEdit,OnEditChar,
  OnColAdjust,OnCreateEdit

  позволяющие настроить поведение встроенного редактора и получить новую функциональность
|</pre>
}

// Version 1.17 (29.06.2006)
// Исправлен баг установки Align для Inplace Editor - спасибо Matveev Dmitry
// Добавлено событие OnDrawCell (by Matveev Dmitry)
// Подсветка всей строки теперь только при lvoRowSelect = True
// Исправлена ошибка перехода в режим редактирования по нажатию Ctrl - спасибо Matveev Dmitry

// Version 1.16 (27.02.2006)
// Несколько исправлений в коде потери-получения фокуса ввода

// Version 1.15 (3.1.2006)
// Адаптация кода по $DEFINE USE_PROP всвязи с изменениями в KOL.PAS(лучше поздно чем никогда!)

// Version 1.14 (19.10.2005)
// МСК: Dcr-файл переименован и ссылка на него перенесена из pas-зеркала в
// dpk-файлы

// Version 1.13 (2.10.2005)
// Добавлено свойство AutoHide в TOnCreateEdit
// AutoHide = True (default) - автоматически закрывает Inplace Editor
// при потере ячейкой фокуса ввода. False - оставляет его открытым

// Version 1.12 (14.07.2005)
// Добавлена функция WndProcListViewWOResizeFlicks

// Version 1.11 (24.01.2005)
// Убраны лишние прокрутки при вызове SetCurLVPos
// thanx Sphinxx for bugreport

// Version 1.10 (18.01.2005)
// При вызове DrawText добавлен атрибут DT_NOPREFIX

// Version 1.09 (24.12.2004)
// [+] Добавлена функция UpdateRow

// Version 1.08 (17.11.2004)
// [-] Исправлена установка цвета фона для встроенного редактора. (Где-то
//     глюк в зеркале для ListView - в испекторе Color = clWindow а реально
//     после создания он получается другим )

// Version 1.07 (25.10.2004)
// [+] Добавлена подсветка текущей линии (Переделан метод LVDrawItem - старый
//     закомментировал - если надо можно самостоятельно восстановить старый)
// [-] Исправлена ошибка неверной отрисовки  - после смены текущей строки при
//     помощи клавиатуры и последующей смене фокуса
// [-] Исправлен метод InternalStopEdit (by Dmitry Matveev)
// [*] Несколько мелких изменений (в основном изменение области видимости
//     разных функций)

// Version 1.06 (4.10.2004)
// [*] Коррекция всвязи с выходом KOL 1.96 - теперь NO_ITEMHEIGHT не нужен

// Version 1.05 (28.09.2004)
// [+] Добавил в OnCreateEdit параметр ReadOnly. При установке его в True
//     редактор не создается (by Matveev Dmitry)
// [+] DEFINE NO_ITEMHEIGHT - для отключения кода регулирующего высоту строк
//     (необходимо установливать здесь и в зеркале если используется версия
//     KOL 1.95+ - в ней управление высотой встроено в ListView)
//     Патч KOL+MCK от 1.95 до версии 1.95+ исправляющий ситуацию с установкой
//     высоты строк для ListView лежит здесь http://kolibdb.100free.com/kolmck195Plus.zip

// Version 1.04 (16.09.2004)
// [+] Добавил возможность установки lvsDetailNoHeader (by Matveev Dmitry)

// Version 1.03 (15.09.2004)
// [-] Убрал "наползание" внешних контролов на заголовок при прокрутке ScrollBar-ом
// [*] Убрал смену активной ячейки при кликах на пустых местах списка (by Matveev Dmitry)

// Version 1.02 (14.09.2004)
// [*] Оптимизировал отрисовку - меньше лишних миганий
// [+] Упорядочил поведение при смене фокуса
// [*] Оптимизировал выравнивание в ячейке встроенного редактора теперь
//     (по крайней мере для taLeft) настраивать Indent в OnColAdjust нет необходимости
// [+] Добавил событие OnCreateEdit - теперь можно попробовать лепить вместо "встроенного"
//     EditBox-а - другие контролы (пока проверил на CheckBox и ComboBox)
//     Пример использования в демке ... Экспериментальная фича - могут быть
//     проблемы... :(

// Version 1.01 (10.09.2004)
// [+] Сделал режим отрисовки OwnerDraw  - получилось почти точная копия VCL-StringGrid
//    (в режиме FixedCols = 0 ; Options= [..,goEditing,goDrawFocusSelect,..])
//     В этом режиме почти отпала необходимость корректировать ColOptions.Indent - InPlaceEditor
//     по-умолчанию будет попадать точно на тоже место что и строки в ListView
//     (по крайней мере для столбцов taLeft.
// [+] Добавил возможность устанавливать высоту строк(Только для варианта OwnerDraw)
//     устанавливается в дополнительном параметре NewEcmListEdit, в MCK - ItemHeight
// [+] StartEdit теперь происходит еще и при WM_CHAR (кроме Enter) на ListView-е /by SeM/
// [*] Много мелких исправлений

// Version 1.00 (6.09.2004)
// Первая реализация
//
{$I KOLDEF.INC}
//{$DEFINE _LE_DEBUG_}

interface

uses
  Windows, Messages, KOL;

type
  PEditorOptions = ^TEditorOptions;
  TEditorOptions = packed record
    Indent:    TRect;
    TextAlign: TTextAlign;
    Options:   TEditOptions;
  end;

  TOnEditText  = procedure(Sender: PControl; ACol, ARow: Integer; var Value: string) of object;
  TOnEditChar  = procedure(Sender: PControl; ACol, ARow: Integer; var Key: KOLChar; Shift: DWORD) of object;
  TOnEndEdit   = procedure(Sender: PControl; ACol, ARow: Integer; CellChanged: Boolean) of object;
  TOnCreateEdit = procedure(Sender: PControl; ACol: Integer; var Editor: PControl; var ReadOnly: Boolean; var AutoHide: Boolean) of object;
  TOnColAdjust  = procedure(Sender: PControl; ACol: Integer; var ColOption: TEditorOptions) of object;
  TOnDrawCell   = function(Sender: PObj; DC: HDC; const Rect: TRect; ACol, ARow: Integer; DrawAction: TDrawAction; ItemState: TDrawState): Boolean of object;

  PEcmListEdit = ^TEcmListEdit;
  TEcmListEdit = object(TObj)
  {* TEcmListEdit реализует всю функциональность KOLEcmListEdit.
|<p>
     KOLEcmListEdit создается при помощи NewEcmListEdit, которая пристраивает
     TEcmListEdit к свойству KOLEcmListEdit.CustomObj и подключает новый обработчик
     событий. Поэтому для доступа к TEcmListEdit в необходимо использовать такую
     конструкцию:
!       PEcmListEdit(KOLEcmListEdit1.CustomObj)
     в MCK это генерируется автоматически.
|</p>
}
  private
    fOnCreateEd: TOnCreateEdit;
    FOnDrawCell: TOnDrawCell;
    procedure EditOnKeyDown(Sender: PControl; var Key: Longint; Shift: DWORD);
    procedure EditOnChar(Sender: PControl; var Key: KOLChar; Shift: DWORD);
    procedure SetCurIdx(const Value: Integer);
  protected
    fOwner: PControl;
    fColOptions: PList;
    fCurIdx: Integer;
    fCurLine: Integer;
    fScroll: Integer;
    fOnPutText: TOnEditText;
    fOnGetText: TOnEditText;
    fOnEndEdit: TOnEndEdit;
    FOnColAdjust: TOnColAdjust;
    fStarted: Boolean;
    fOnEditChar: TOnEditChar;
    fShift: Integer;
    fEmbedEd: Boolean;
    fAutoHide: Boolean;
    function NewInPlaceEd(Options: TEditOptions; Align: TTextAlign): PControl;
    procedure DestroyInPlaceEditor;
    procedure SetEditPos;
    procedure LoadEditValues;
    function GetLVItemAtPos(Pt: TSmallPoint; var SubItem: Integer): Integer;
    procedure DoColAdjust(ColCount: Integer);
    procedure InternalStopEdit(const Store: Boolean);
    procedure HideInplaceEd(ActivateOwner: Boolean);
    function LVDrawItem(Sender: PObj; DC: HDC; const Rect: TRect; ItemIdx: Integer; DrawAction: TDrawAction; ItemState: TDrawState): Boolean;
    procedure ComboBox_CloseUp(Sender: PObj);
  public
    fInPlaceEd:     PControl;
    IsComboEditor:  Boolean;
    ComboOptions:   TComboOptions;
    ComboText:      KOLString;
    destructor Destroy; virtual; // Do not call this destructor. Use Free method instead.
    procedure SetCurLVPos(ALine, AIdx: Integer);
    procedure StartEdit;
    {*
|<p>
       Переводит объект в состояние редактирования Editing=True. Встроенный
       редактор появляется на текущей (LVCurItem) строке списка.
       Если в списке нет выбранной строки (LVCurItem=-1), автоматически
       выбирается первая(0) строка списка ListView.
       Вызывается автоматически при DblClick-е мышкой или по клавише Enter.
       Можно вызвать программно:
!             PEcmListEdit(KOLEcmListEdit1.CustomObj).StartEdit

|</p>
    }
    procedure StopEdit(Store: Boolean);
    {*
|<p>
       Выводит объект из состояние редактирования Editing=False.
       Параметр Store определяет будут ли сохраняться данные из
       встроенного редактора в ListView.
       Вызывается автоматически при нажатии клавиши Esc (Store=False),
       смене фокуса (в режиме OwnerDraw!)
       Можно вызвать программно:
!             PEcmListEdit(KOLEcmListEdit1.CustomObj).StopEdit(True)
|</p>
    }
    procedure SelectCell(ACol, ARow: Integer);
    {* Подсвечивает текущую ячейку   }
    procedure UpdateRow(ARow: Integer);
    {* Перерисовка (Invalidate) указанной строки    }
    property Editing: Boolean read fStarted;
    {* True - встроенный редактор активен. }
    property OnGetEditText: TOnEditText read fOnGetText write fOnGetText;
    {* Вызывается при загрузке текста во встроенный редактор. (Отдельно
    для кажого столбца). }
    property OnPutEditText: TOnEditText read fOnPutText write fOnPutText;
    {* Вызывается при выгрузке текста из встроенного редактора. (Отдельно
    для кажого столбца). }
    property OnStopEdit: TOnEndEdit read fOnEndEdit write fOnEndEdit;
    {* Вызывается при смене строки редактирования и при выполнении StopEdit. }
    property OnEditChar: TOnEditChar read fOnEditChar write fOnEditChar;
    {* Вызывается при получении встроенным редактором событий WM_CHAR. Может
    использоваться для фильрации ввода}
//---------------------------------------------------------------------------
    property OnColAdjust: TOnColAdjust read FOnColAdjust write fOnColAdjust;
    {*
|<p>
    Вызывается при создании встроенного редактора. (Отдельно для каждого
    столбца). Используется для задания парметров редактора.

|</p>
}
    property CurIdx: Integer read fCurIdx write SetCurIdx;
    {*
|<p>
    Устанавливает фокус на указанный столбец.
|</p>
}
    property OnCreateEdit: TOnCreateEdit read fOnCreateEd write fOnCreateEd;
    {* Вызывается при создании редактора ячейки. Может использоваться для
    перекрытия встроенного EditBox-а другими компонентами. }

    property OnDrawCell: TOnDrawCell read FOnDrawCell write FOnDrawCell;
    {*
|<p>
    Позволяет реализовать собственную отрисовку ячеек (Не в режиме редактирования)
    Вызывается отдельно для каждой ячейки. Если обработчик возвращает False - ячейка
    рисуется стандартно.
|</p>
}
  end;
  // mck class
  TKOLEcmListEdit = PControl;

function NewEcmListEdit(AParent: PControl; Style: TListViewStyle; Options: TListViewOptions; ImageListSmall, ImageListNormal, ImageListState: PImageList): PControl;
function WndProcEcmListEdit(Sender: PControl; var Msg: TMsg; var Rslt: Integer): Boolean;
function WndProcListViewWOResizeFlicks(Sender: PControl; var Msg: TMsg; var Rslt: Integer): Boolean;

implementation

const
  LEN_COL_ADJUST = WM_USER + 223;

{$IFDEF _LE_DEBUG_}
procedure AddLog(Addr: Pointer; const S: string);
var
  TS: TSystemTime;
  SS: String;
begin
  GetSystemTime(TS);
  SS := Format(' %2d:%.2d:%.2d:%.3d | %.8x ', [TS.wHour, TS.wMinute, TS.wSecond, TS.wMilliseconds, Integer(Addr)]);
  LogFileOutput('.\LE_Log.txt', SS + S);
end;
{$ENDIF}

function WndProcEcmListEdit(Sender: PControl; var Msg: TMsg; var Rslt: Integer): Boolean;
var
  R: TRect;
  NMhdr: PNMHdr;
  NewLine: Integer;
  NewCurIdx: Integer;
begin
  Result := False;
  with PEcmListEdit(Sender.CustomObj)^ do begin
    case Msg.message of
      LVM_INSERTCOLUMNA, LVM_INSERTCOLUMNW, LVM_DELETECOLUMN:
        PostMessage(Msg.hwnd, LEN_COL_ADJUST, 0, 0);

      LEN_COL_ADJUST:
        DoColAdjust(Sender.LVColCount);

      WM_LBUTTONDOWN{$IFDEF rbutton_sel}, WM_RBUTTONDOWN{$ENDIF}:
      begin
        NewLine := GetLVItemAtPos(TSmallPoint(Msg.lParam), NewCurIdx);
        SetCurLVPos(NewLine, NewCurIdx);
        Sender.Focused := True;
        Result := True;
      end;

      WM_LBUTTONDBLCLK{$IFDEF rbutton_sel}, WM_RBUTTONDBLCLK{$ENDIF}:
      begin
        NewLine := GetLVItemAtPos(TSmallPoint(Msg.lParam), NewCurIdx);
        SetCurLVPos(NewLine, NewCurIdx);
        if (NewLine <> -1) and (NewCurIdx <> -1) then StartEdit;
        Sender.Tabstop := False;
        Result := True;
      end;

      WM_KEYDOWN:
      begin
        if (Msg.WParam = VK_RETURN) then
          StartEdit
        else begin
          case Msg.WParam of
            VK_LEFT, VK_RIGHT:
            begin
              SetCurLVPos(Sender.LVCurItem, fCurIdx + Msg.wParam - 38);
              Result := True;
            end;
          end;
          SetEditPos;
        end;
        //fInPlaceEd.Click; //.DroppedDown := True;
      end;

      // by SeM
      WM_CHAR:
      if (GetKeyState(VK_CONTROL) >= 0) then begin // ! by Matveev Dmitry
        case Msg.wParam of
          VK_ESCAPE, VK_RETURN, VK_TAB:
            ;
          else begin
            StartEdit;
            Sender.Tabstop := False;
            if Assigned(fInPlaceEd) then
              PostMessage(fInPlaceEd.Handle, Msg.message, Msg.wParam, Msg.lParam);
            Result := True;
          end;
        end;
      end;

      WM_NCPAINT, WM_PAINT:
      begin
{$IFDEF _LE_DEBUG_}
        AddLog(Sender, 'ListEdit:WM_PAINT');
{$ENDIF}
        SetEditPos();
      end;

//      WM_ERASEBKGND: begin
//        Result := True;
//      end;

      // Какая-то бяка с прорисовкой сетки в режиме lvoGridLines при использовании
      // темы XP - при прокрутке ScrollBar(только кнопками "вверх","вниз") происходит
      // лишняя прорисовка линий  - в результате некотрые строки получаются перечеркнутыми
      // Этот "маразм" позволяет слегка подправить ситуацию
      // Если кто знает как поправить - напишите мне ...
      WM_VSCROLL:
      begin
        if (Msg.wParam = SB_ENDSCROLL) then begin
          InvalidateRect(fOwner.Handle, nil, True);
          UpdateWindow(fOwner.Handle);
        end;
      end;
      WM_NOTIFY:
      begin
        NMHdr := Pointer(Msg.lParam);
        case NMHdr.code of
          NM_KILLFOCUS:
          begin
{$IFDEF _LE_DEBUG_}
              AddLog(Sender, 'ListEdit:NM_KILLFOCUS');
{$ENDIF}
              R := fOwner.ClientRect;
              InvalidateRect(fOwner.Handle, @R, False); //UpdateRow(fCurLine);
          end;
          NM_SETFOCUS:
          begin
{$IFDEF _LE_DEBUG_}
              AddLog(Sender, 'ListEdit:NM_SETFOCUS');
{$ENDIF}
          //SetCurLVPos(fOwner.LVCurItem,fCurIdx);
          end;
          LVN_ITEMCHANGED:
          begin
{$IFDEF _LE_DEBUG_}
            AddLog(Sender, 'ListEdit:LVN_ITEMCHANGED');
{$ENDIF}
            if (fCurLine <> fOwner.LVCurItem) then
              SetCurLVPos(fOwner.LVCurItem, fCurIdx);
          end;
        end;
      end;
    end;
  end;
end;

//by Matveev Dmitry
function WndProcInPlaceEd(Sender: PControl; var Msg: TMsg; var Rslt: Integer): Boolean;
var
  pLE: PEcmListEdit;
begin
  Result := False;
  case Msg.message of
    WM_KEYDOWN:
    begin
      if Msg.wParam = VK_ESCAPE then
        PEcmListEdit(Sender.Parent.CustomObj).StopEdit(False);
    end;
    WM_KILLFOCUS:
    begin
      pLE := PEcmListEdit(Sender.Parent.CustomObj);
      if Assigned(pLE) then begin
        with pLE^ do
          if (fEmbedEd and fAutoHide) then begin
            InternalStopEdit(True);
            HideInPlaceEd(True);
          end;
      end;
    end;
    // D[u]fa
    WM_CHAR:
    if (Msg.wParam = VK_RETURN) then begin
      Msg.message := WM_KILLFOCUS;
      WndProcInPlaceEd(Sender, Msg, Rslt);
      Result := True;
    end;
  end;
end;

// Позволяет в некорых случаях избавиться от лишнего "мограния" при изменении
// размеров. Может использоваться для стандартного KOLListView.
// Для применения после создания ListView-а (ListEdit-а) необходимо присоединить
// данную функцию вызовом ListViewXXX.AttachProc(@WndProcListViewWOResizeFlicks);
function WndProcListViewWOResizeFlicks(Sender: PControl; var Msg: TMsg; var Rslt: Integer): Boolean;
var
  rUnder:  TRect;
  rRight:  TRect;
  rClient: TRect;
begin
  Result := False;
  if (Msg.message = WM_ERASEBKGND) then begin
    rClient := Sender.ClientRect;
    if (Sender.LVCount > 0) then begin
      rUnder        := Sender.LVSubItemRect(Sender.LVCount - 1, 0);
      rUnder.Top    := rUnder.Bottom;
      rUnder.Bottom := rClient.Bottom;
      rRight.Left   := rUnder.Right;
      rRight.Right  := rClient.Right;
      rRight.Top    := rClient.Top;
      rRight.Bottom := rClient.Bottom;
      FillRect(Msg.wParam, rRight, Sender.Canvas.Brush.Handle);
    end else
      rUnder := rClient;
    FillRect(Msg.wParam, rUnder, Sender.Canvas.Brush.Handle);
    Result := True;
  end;
end;

// PEcmListEdit

function NewEcmListEdit;
var
  pLD: PEcmListEdit;
  mOpt: TListViewOptions;
begin
  mOpt := Options + [lvoHideSel, lvoOwnerDrawFixed];
  if ((Style <> lvsDetail) and (Style <> lvsDetailNoHeader)) then Style := lvsDetail;
  Result := NewListView(AParent, Style, mOpt, ImageListSmall, ImageListNormal, ImageListState);
  New(pLD, Create);
  pLD.fOwner        := Result;
  pLD.fEmbedEd      := False;
  pLD.fColOptions   := NewList;
  pLD.fCurLine      := -1;
  Result.CustomObj  := pLD;
  Result.OnDrawItem := pLD.LVDrawItem;
  Result.AttachProc(WndProcEcmListEdit);
  Result.AttachProc(WndProcListViewWOResizeFlicks);                             //beta, но на глаз супер
end;

destructor TEcmListEdit.Destroy;
begin
  InternalStopEdit(False);
  fColOptions.Release;
  inherited;
end;

procedure TEcmListEdit.ComboBox_CloseUp(Sender: PObj);
begin
  StopEdit(True);
end;

procedure TEcmListEdit.EditOnKeyDown(Sender: PControl; var Key: Longint; Shift: DWORD);
begin
  if (fScroll <> 0) then
    PostMessage(fOwner.Handle, LVM_SCROLL, fScroll, 0);
  case key of
//    VK_RETURN:
//      StoreEditValues;
//    VK_ESCAPE: StopEdit(False);
    VK_UP, VK_DOWN:
    begin
      SetCurLVPos(fCurLine + (Key - 39), fCurIdx);
      Key := 0;
    end;
    VK_LEFT:
    if (Sender.SelStart = 0) and (Sender.SelLength = 0) and (fCurIdx > 0) then begin
      SetCurLVPos(fCurLine, fCurIdx - 1);
      Key := 0;
    end;
    VK_RIGHT:
    if (Sender.SelStart = Length(Sender.Text)) and (fCurIdx < fOwner.LVColCount - 1) then begin
      SetCurLVPos(fCurLine, fCurIdx + 1);
      Key := 0;
    end;
  end;
end;

procedure TEcmListEdit.DestroyInPlaceEditor;
begin
{$IFDEF _LE_DEBUG_}
  AddLog(Self.fOwner, 'DestroyInPlaceEditor');
{$ENDIF}
  if fEmbedEd and Assigned(fInPlaceEd) then
    fInPlaceEd.Free;
  fInPlaceEd := nil;
end;

procedure TEcmListEdit.SetEditPos;
var
  R, Re: TRect;
  cw: Integer;
  pEO: PEditorOptions;
  Header: THandle;
  HeaderHeight: Integer;
begin
{$IFDEF _LE_DEBUG_}
  AddLog(Self.fOwner, 'SetEditPos');
{$ENDIF}
  with fOwner^ do begin
    R := LVSubItemRect(LVCurItem, fCurIdx);
    cw := LVColWidth[fCurIdx];
    R.Right := R.Left + cw;
    if Assigned(fInPlaceEd) then begin
      Header := Perform(LVM_GETHEADER, 0, 0);
      GetWindowRect(Header, Re);
      HeaderHeight := Re.Bottom - Re.Top;
      if R.Top >= HeaderHeight then begin
        if fEmbedEd and (fInPlaceEd.Perform(EM_GETRECT, 0, Integer(@Re)) > 0) then begin
          if (R.Bottom - R.Top) > (Re.Bottom - Re.Top) then begin
            cw := ((R.Bottom - R.Top) - (Re.Bottom - Re.Top)) div 2;
            Inc(R.Top, cw);
            Dec(R.Bottom, cw);
          end;
          Inc(R.Left, fShift - Re.Left);
          Dec(R.Right, fShift - Re.Left);
        end;
        pEO := fColOptions.Items[fCurIdx];
        with pEO.Indent do begin
          Inc(R.Left, Left);
          Dec(R.Right, Right);
          Inc(R.Top, Top);
          Dec(R.Bottom, Bottom);
          //
          if fEmbedEd then
            Dec(R.Left, 2);
        end;
      end else
        FillChar(R, SizeOf(R), 0);
      fInPlaceEd.BoundsRect := R;
    end;
    if (R.Left <= 0) then
      fScroll := R.Left
    else if (R.Right > fOwner.Width - 24) then
      fScroll := R.Right - (fOwner.Width - 24)
    else
      fScroll := 0;
  end;
end;

procedure TEcmListEdit.LoadEditValues;
var
  S: string;
begin
{$IFDEF _LE_DEBUG_}
  AddLog(Self.fOwner, 'LoadEditValues');
{$ENDIF}
  with fOwner^ do begin
    S := fOwner.LVItems[LVCurItem, fCurIdx];
    if Assigned(fOnGetText) then
      fOnGetText(fOwner, fCurIdx, LVCurItem, S);
    if IsComboEditor then begin
      IsComboEditor       := False; //
      fInPlaceEd.CurIndex := fInPlaceEd.IndexOf(S);
      //fInPlaceEd.DroppedDown := True;
    end else begin //if fEmbedEd then begin
      if (fInPlaceEd.SubClassName = 'obj_COMBOBOX') then
        fInPlaceEd.CurIndex := fInPlaceEd.IndexOf(S)
      else begin // 'obj_EDIT'
        fInPlaceEd.Text := S;
        fInPlaceEd.SelectAll;
      end;
    end;
  end;
end;

procedure TEcmListEdit.StartEdit;
var
  pEO: PEditorOptions;
begin
{$IFDEF _LE_DEBUG_}
  AddLog(Self.fOwner, 'StartEdit');
{$ENDIF}
  if (fOwner.LVColCount = 0) or (fOwner.LVCount = 0) or fStarted or (fCurIdx = -1) then Exit;
  fCurLine := fOwner.LVCurItem;
  if (fCurLine = -1) then begin
    fCurLine := 0;
    fOwner.LVCurItem := 0;
  end;
  //CreateInPlaceEditor(fOwner.LVColCount);
  if not fStarted then begin
    DestroyInPlaceEditor;
    if (fOwner.LVColCount > 0) then begin
      pEO := fColOptions.Items[fCurIdx];
      fInPlaceEd := NewInPlaceEd(pEO.Options, pEO.TextAlign);
    end;
  end;
  if Assigned(fInPlaceEd) then begin
    fStarted := True;
    SetEditPos;
    LoadEditValues;
    fOwner.Tabstop := False;
    fInPlaceEd.Visible := True;
    fInPlaceEd.Focused := True;
    UpdateRow(fCurLine);
  end;
end;

procedure TEcmListEdit.StopEdit(Store: Boolean);
begin
{$IFDEF _LE_DEBUG_}
  AddLog(Self.fOwner, 'StopEdit: ' + Int2Str(Integer(Store)));
{$ENDIF}
  InternalStopEdit(Store);
  HideInPlaceEd(True);
end;

function TEcmListEdit.GetLVItemAtPos(Pt: TSmallPoint; var SubItem: Integer): Integer;
var
  HTI: TLVHitTestInfo;
begin
{$IFDEF _LE_DEBUG_}
  AddLog(Self.fOwner, 'GetLVItemAtPos: ' + Int2Str(SubItem));
{$ENDIF}
  HTI.pt.x := Pt.X;
  HTI.pt.y := Pt.Y;
  fOwner.Perform(LVM_SUBITEMHITTEST, 0, Integer(@HTI));
  Result := HTI.iItem;
  SubItem := HTI.iSubItem;
end;

procedure TEcmListEdit.EditOnChar(Sender: PControl; var Key: KOLChar; Shift: DWORD);
begin
  case Key of
    #13:
    begin
      StopEdit(True);
      Key := #0
    end;
  end;
  if Assigned(fOnEditChar) then begin
    case Key of
      #08: // BackSpace! - всегда обрабатывать
    else
      fOnEditChar(fInPlaceEd, fCurIdx, fOwner.LVCurItem, Key, Shift);
    end;
  end;
end;

function TEcmListEdit.NewInPlaceEd(Options: TEditOptions; Align: TTextAlign): PControl;
var
  RO: Boolean;
  AH: Boolean;
begin
  Result := nil;
  RO := False;
  AH := True;
  if Assigned(fOnCreateEd) then
    fOnCreateEd(fOwner, fCurIdx, Result, RO, AH);
  if not RO then begin
    fEmbedEd := not Assigned(Result);
    if fEmbedEd then begin
      if IsComboEditor then begin
        Result := NewCombobox(fOwner, ComboOptions);
        Result.OnCloseUp := ComboBox_CloseUp;
        repeat
          Result.Add(Parse(ComboText, ';'));
        until (ComboText = '');
      end else
        Result := NewEditBox(fOwner, Options);
      Result.Font.Assign(fOwner.Font);
      Result.Color := fOwner.LVTextBkColor;
      Result.ExStyle := Result.ExStyle and not (WS_EX_DLGMODALFRAME or WS_EX_WINDOWEDGE or WS_EX_CLIENTEDGE);
      Result.OnKeyDown := EditOnKeyDown;
      Result.AttachProc(WndProcInPlaceEd); //by Matveev Dmitry
    end else begin
      Result.Parent := fOwner;
      //Result.Focused := True;
      Result.Visible := True;
    end;
    //Result.Tabstop          := True;
    fAutoHide        := AH;
    Result.OnChar    := EditOnChar;
    Result.TabOrder  := fOwner.TabOrder;
    Result.TextAlign := Align;
  end;
end;

function TEcmListEdit.LVDrawItem(Sender: PObj; DC: HDC; const Rect: TRect; ItemIdx: Integer; DrawAction: TDrawAction; ItemState: TDrawState): Boolean;
var
  fBr: HBRUSH;
  cBr: TColor;
  i:   Integer;
  S:   String;
  P:   TPoint;
  R:   TRect;
  dt:  DWORD;
  pEO: PEditorOptions;
begin
  with fOwner^ do begin
    fShift := 0;
    for i := 0 to LVColCount - 1 do begin
      R := LVSubItemRect(ItemIdx, i);
      P := LVItemPos[i];
      if (i = 0) then begin
        R.Right := R.Left + LVColWidth[0];
        fShift  := P.X - R.Left + 1; // dufa. 9.05.13, раньше было 2. с 1 копия ListView + LVSCW_AUTOSIZE работает как надо;
      end;
      if (Perform(LVM_GETEXTENDEDLISTVIEWSTYLE, 0, 0) and LVS_EX_GRIDLINES) <> 0 then begin
        Inc(R.Left);
        Dec(R.Bottom);
      end;
      if Assigned(FOnDrawCell) then
        if FOnDrawCell(Sender, DC, R, i, ItemIdx, DrawAction, ItemState) then Continue; //by Matveev Dmitry

      if fOwner.Enabled then
        cBr := fOwner.LVTextBkColor
      else
        cBr := clBtnFace;

      if (ItemIdx = fCurLine) then begin
        if (fOwner.Focused or (Assigned(fInPlaceEd) and fInPlaceEd.Visible)) and Enabled then begin
          if (i = fCurIdx) then begin
            if fStarted then
              cBr := fOwner.LVTextBkColor
            else
              cBr := clHighlight;
            SetTextColor(DC, Color2RGB(clHighlightText));
          end else begin
            if (Perform(LVM_GETEXTENDEDLISTVIEWSTYLE, 0, 0) and LVS_EX_FULLROWSELECT) <> 0 then
              cBr := $F3E6CD;
            SetTextColor(DC, Color2RGB(fOwner.Font.Color));
          end
        end else begin
          SetTextColor(DC, Color2RGB(fOwner.Font.Color));
          if Enabled then begin
            if (i = fCurIdx) then begin
              if fStarted then
                cBr := fOwner.LVTextBkColor
              else
                cBr := clInactiveBorder;
            end else begin
              cBr := $F0F0F0;
            end
          end;
        end;
      end else
        SetTextColor(DC, Color2RGB(fOwner.Font.Color));

      fBr := CreateSolidBrush(Color2RGB(cBr));
      FillRect(DC, R, fBr);
      DeleteObject(fBr);

      if not ((ItemIdx = LVCurItem) and (fStarted) and (i = fCurIdx)) then begin
        S := fOwner.LVItems[ItemIdx, i];
        dt := DT_END_ELLIPSIS or DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX;

        if (fColOptions.Count <> LVColCount) then
          DoColAdjust(LVColCount);
        pEO := fColOptions.Items[i];
        case pEO.TextAlign of
          taRight:
            dt := dt or DT_RIGHT;
          taCenter:
            dt := dt or DT_CENTER;
        end;
        Dec(R.Right, fShift);
        Inc(R.Left, fShift);
        DrawText(DC, @S[1], Length(S), R, dt);
      end;
    end;
  end;
  Result := True;
end;

procedure TEcmListEdit.DoColAdjust(ColCount: Integer);
var
  i:   Integer;
  pEO: PEditorOptions;
begin
  if (ColCount <> fColOptions.Count) then begin
    for i := fColOptions.Count - 1 downto 0 do                                  // downto - for what?
      FreeMem(fColOptions.Items[i]);
    fColOptions.Clear;

    for i := 0 to ColCount - 1 do begin
      New(pEO);
      ZeroMemory(pEO, SizeOf(TEditorOptions));
      pEO.TextAlign := fOwner.LVColAlign[i];
      if Assigned(fOnColAdjust) then
        fOnColAdjust(fOwner, i, pEO^);
      fColOptions.Add(pEO);
    end;
  end;
end;

procedure TEcmListEdit.SetCurLVPos(ALine, AIdx: Integer);
var
  NewIdx: Integer;
begin
//  NewIdx := AIdx;
  with fOwner^ do begin
//    if (ALine = LVCurItem) and (AIdx = fCurIdx) then Exit;
{$IFDEF _LE_DEBUG_}
    AddLog(Self.fOwner, 'SetCurLVPos: ' + Int2Str(ALine) + ',' + Int2Str(AIdx));
{$ENDIF}
    if (AIdx >= 0) and (AIdx < LVColCount) and (ALine >= 0) and (ALine < LVCount) then
      NewIdx := AIdx
    else
      NewIdx := fCurIdx;
    InternalStopEdit(True);

    fCurIdx := NewIdx;
    if (ALine >= 0) and (ALine < LVCount) then begin
      if ALine <> LVCurItem then begin
        NewIdx := LVCurItem;
        LVCurItem := ALine;
        UpdateRow(NewIdx);
      end;
      fCurLine := LVCurItem;
    end;
    HideInPlaceEd(True);
    SetEditPos;
    if (fScroll <> 0) then
      PostMessage(Handle, LVM_SCROLL, fScroll, 0);

    if (ALine <> -1) then
      PostMessage(Handle, LVM_ENSUREVISIBLE, fCurLine, 0);

    UpdateRow(fCurLine);
  end;
end;

procedure TEcmListEdit.InternalStopEdit(const Store: Boolean);
var
  s: String;
  fCellChanged: Boolean;
begin
  if fStarted then begin
{$IFDEF _LE_DEBUG_}
    AddLog(Self.fOwner, 'InternalStopEdit: ' + Int2Str(Integer(Store)));
{$ENDIF}
    fCellChanged := False;
    if Store then begin
      with fOwner^ do begin
        if (fOwner.LVItems[LVCurItem, fCurIdx] <> fInPlaceEd.Text) then begin
          S := fInPlaceEd.Text;
          if Assigned(fOnPutText) then
            fOnPutText(fOwner, fCurIdx, LVCurItem, S);
          if (S <> fOwner.LVItems[LVCurItem, fCurIdx]) then begin
            fCellChanged := True;
            fOwner.LVItems[LVCurItem, fCurIdx] := S;
          end;
          fInPlaceEd.Text := S;
        end;
      end;
    end;
    fStarted := False;
    if Assigned(fOnEndEdit) then
      fOnEndEdit(fOwner, fCurIdx, fOwner.LVCurItem, fCellChanged);
  end;
end;

procedure TEcmListEdit.HideInplaceEd(ActivateOwner: Boolean);
begin
{$IFDEF _LE_DEBUG_}
  AddLog(Self.fOwner, 'HideInplaceEd: ' + Int2Str(Integer(ActivateOwner)));
{$ENDIF}
  if Assigned(fInPlaceEd) then begin
//    fInPlaceEd.Tabstop := False;
    fOwner.TabOrder := fInPlaceEd.TabOrder;
    {if ActivateOwner then }fOwner.Focused := True;
    fOwner.Tabstop := True;
    fInPlaceEd.Visible := False;
    UpdateRow(fCurLine);
    //fOwner.Invalidate;
  end;
  //if fInPlaceEd <> nil then DestroyInPlaceEditor();
end;

procedure TEcmListEdit.SetCurIdx(const Value: Integer);
begin
  fOwner.Focused := True;
  SetCurLVPos(fOwner.LVCurItem, Value);
end;

procedure TEcmListEdit.SelectCell(ACol, ARow: Integer);
begin
{$IFDEF _LE_DEBUG_}
  AddLog(Self.fOwner, 'SelectCell: ' + Int2Str(ACol) + ',' + Int2Str(ARow));
{$ENDIF}
  fOwner.Focused := True;
  SetCurLVPos(ARow, ACol);
end;

procedure TEcmListEdit.UpdateRow(ARow: Integer);
var
  R: TRect;
begin
{$IFDEF _LE_DEBUG_}
  AddLog(Self.fOwner, 'UpdateRow": ' + Int2Str(ARow));
{$ENDIF}
  R := fOwner.LVSubItemRect(ARow, 0);
  InvalidateRect(fOwner.Handle, @R, False);
end;

end.

