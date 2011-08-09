unit KOLmdvDBF;
// Компонент mdvDBF - прямой доступ к DBF-файлам (с memo) без использования дополнительного ПО.
// E-Mail: dominiko-m@yandex.ru
// http://www.mdvkol.narod.ru/
// Автор: Матвеев Дмитрий

// - История -

// Дата: 01.03.2007 Версия: 1.03
{
[+] - добавил метод NewDbf
[*] - Поправил сигнатуру мемо-полей для dBaseIV
}

// Дата: 26.09.2005 Версия: 1.02
{
[+] - добавил несколько вспомогательных методов
}
// Дата: 07.12.2004 Версия: 1.01
{
[+] - добавил сигнатуру мемо-полей
}

// Дата: 02.12.2004 Версия: 1.00
   {Стартовая версия}


interface

uses Windows, KOL;

const
    DBF_FoxBASE             = $02;
    DBF_FoxBASE_            = $FB;
    DBF_dBaseIIIplus        = $03;
    DBF_dBaseIIIplusMemo    = $83;
    DBF_dBaseIV             = $03;
    DBF_dBaseIVSQLtable     = $43;
    DBF_dBaseIVSQLsystem    = $63;
    DBF_dBaseIVSQLtableMemo = $CB;
    DBF_dBaseIVMemo         = $8B;
    DBF_dBaseV              = $05;
    DBF_FoxPro2xMemo        = $F5;
    DBF_VisualFoxPro        = $30;
    DBF_VisualFoxProInc     = $31;

    MsDos_US_437                           = $01;
    MsDos_Mazovia_Polish_620               = $69;
    MsDos_Greek_737                        = $6A;
    MsDos_International_850                = $02;
    MsDos_Eastern_European_852             = $64;
    MsDos_Turkish_857                      = $6B;
    MsDos_Icelandic_861                    = $67;
    MsDos_Nordic_865                       = $66;
    MsDos_Russian_866                      = $65;
    MsDos_Kamenicky_Czech_895              = $68;
    Windows_Thai_874                       = $7C;
    Windows_Japanese_932                   = $7B;
    Windows_Chinese_PRC_Singapore_936      = $7A;
    Windows_Korean_949                     = $79;
    Windows_Chinese_HongKongSAR_Taiwan_950 = $78;
    Windows_Eastern_European_1250          = $C8;
    Windows_Russian_1251                   = $C9;
    Windows_ANSI_1252                      = $03;
    Windows_Greek_1253                     = $CB;
    Windows_Turkish_1254                   = $CA;
    Windows_Hebrew_1255                    = $7D;
    Windows_Arabic_1256                    = $7E;
    Windows_Standard_Macintosh_10000       = $04;
    Windows_Greek_Macintosh_10006          = $98;
    Windows_Russian_Macintosh_10007        = $96;
    Windows_Macintosh_EE_10029             = $97;

type
  TYYMMDD = array [1..3] of Byte;
  TDBFHeader = packed record
    DBFType: Byte;                      // Тип файла (DBF_xxx)
    LastUpdated: TYYMMDD;               // Дата последнего обновления в формате YYMMDD
    RecordCount: DWord;                 // Количество записей в таблице
    HeaderLength: Word;                 // Количество байт, занимаемых заголовком
    RecordLength: Word;                 // Количество байт, занимаемых записью
    Reserved_1: array [1..16] of Byte;  // 3-Зарезервированная область; 13 - Зарезервировано для сетевой версии dBASE III PLUS
    TableFlags: Byte;                   // $01 - file has a structural .cdx; $02 - file has a Memo field; $04 - file is a database (.dbc). This byte can contain the sum of any of the above values. For example, the value 0x03 indicates the table has a structural .cdx and a Memo field.
    CodePage: Byte;                     // Кодовая страница
    Reserved_2: Word;                   // Зарезервированная область
  end;

  TFieldName = array [1..11] of Char;
  TDBFField = packed record
    FieldName: TFieldName;              // Название поля
    FieldType: Char;                    // Тип поля (C – Character; Y – Currency; N – Numeric; F – Float; D – Date; T – DateTime; B – Double; I – Integer; L – Logical; M – Memo; G – General; C – Character (binary); M – Memo (binary); P – Picture;
    Address: DWord;                     // Адрес поля в записи
    FieldLength: Byte;                  // Длина поля (в байтах)
    Decimals: Byte;                     // Длина десятичной части
    FieldFlags: Byte;                   // Флаг поля ($01 - System Column (not visible to user); $02 - Column can store null values; $04 - Binary column (for CHAR and MEMO only); $06($02+$04) - When a field is NULL and binary (Integer, Currency, and Character/Memo fields); $0C - Column is autoincrementing;
    AutoIncNext: DWord;                 // Следующее значение для Автоинкримента
    AutoIncStep: Byte;                  // Шаг Автоинкримента
    Reserved3  : array[1..7] of Byte;   // Зарезервированная область
    IndexFlag  : Byte;                  // Флаг MDX-поля: $01 если поле имеет метку индекса в MDX-файле, $00 - нет.
  end;

  TFPTHeader = packed record
    NextFree: DWord;                    // * Location of next free block
    Unused: Word;                       // Unused
    BlockSize: Word;                    // * Block size (bytes per block)
    Unused_2: array [0..503] of Byte;
  end;

  TDBTHeader = packed record
    NextFree: DWord;                    // Location of next free block
    Reserved1: DWord;
    DbfFileName: array [1..9] of Char;	// Name parent DBF table
    reserved2: array [1..3] of Char;
    BlockSize: Word;
  end;

  TMemoBlockHeader = packed record
    BlockSignature: DWord;              // * Block signature (indicates the type of data in the block) (0 – picture (picture field type); 1 – text (memo field type))
    Length: DWord;                      // * Length of memo (in bytes)
    //08–n  Memo text (n = length)
  end;

  PRecordBuffer = ^TRecordBuffer;
  TRecordBuffer = array[0..0] of Byte;

  PDBFFields = ^TDBFFields;
  TDBFFields = array [0..0] of TDBFField;

  TFieldType = (ftUnknown, ftCharacter, ftCurrency, ftNumeric,
                ftFloat, ftDate, ftDateTime, ftBinary,
                ftInteger, ftLogical, ftMemo, ftGeneral,
                ftCharacterBin, ftMemoBin, ftPicture);

  TDbfErrors  = (eNoErrors, eUnknownError, eFileNotExist, eFileOpen, eDBFHeader, eAppendRecord, eReadRecord, eWriteRecord, eReadField, eWriteField, eInvalidValue);

  TOnDbfEvent = procedure(Sender: PObj; var Allowed: Boolean) of object;

  TMemoType = (mtUnknown, mtFoxPro, mtdBaseIV, mtdBase);

  PmdvDBF = ^TmdvDBF;
  TKOLmdvDBF = PmdvDBF;

  TmdvDBF = object(TObj)
  private
    FDBFHeader: TDBFHeader;
    FDBFFields: PDBFFields;
    FRecordBuffer: PRecordBuffer;
    FFieldsCount: Integer;
    FFPTHeader: TFPTHeader;
    FDBTHeader: TDBTHeader;

    FHasMemo: Boolean;
    FActive, FAutoUpdate, FReadOnly, FDBFModified, FRecordModified: Boolean;
    FFileName: String;
    FFileNameMemo: String;
    FDBFStream, FDBFMemoStream: PStream;
    FCurrentRecord: DWord;
    FBOF, FEOF: Boolean;
    FError: TDbfErrors;
    FOnScroll, FOnDelete, FOnAppend: TOnDbfEvent;

    function InvertDWord(var Value: DWord): DWord;

    function ReadFields: Boolean;
    procedure ReadRecord;
    procedure WriteRecord;
    procedure SetDate(var ADate: TYYMMDD);

    function GetLastUpdated: TDateTime;
    function GetHasMemo: Boolean;

    function GetMemoInfo(Index: Integer; var ABlockNum, ABlockSize, ABlockCount, ASize: DWord): TMemoType;

    procedure SetActive(const Value: Boolean);
    procedure SetFileName(const Value: String);
    function GetError: TDbfErrors;

    function GetFieldName(Index: Integer): String;
    function GetFieldNumber(NameField: String): Integer;
    function GetFieldType(Index: Integer): TFieldType;
    function GetFieldDecimals(Index: Integer): Byte;
    function GetFieldLength(Index: Integer): Byte;
    procedure SetCurrentRecord(const Value: DWord);

    function GetIsDelete: Boolean;
    procedure SetIsDelete(const Value: Boolean);

    function GetFieldIsString(Index: Integer): Boolean;
    function GetFieldIsBoolean(Index: Integer): Boolean;
    function GetFieldIsDateTime(Index: Integer): Boolean;
    function GetFieldIsFloat(Index: Integer): Boolean;
    function GetFieldIsInteger(Index: Integer): Boolean;

    function GetAsText(Index: Integer): String;
    function GetAsString(Index: Integer): String;
    function GetAsBoolean(Index: Integer): Boolean;
    function GetAsDateTime(Index: Integer): TDateTime;
    function GetAsFloat(Index: Integer): Double;
    function GetAsInteger(Index: Integer): Integer;

    procedure SetString(Index: Integer; Value: String);
    procedure SetAsText(Index: Integer; Value: String);
    procedure SetAsString(Index: Integer; const Value: String);
    procedure SetAsBoolean(Index: Integer; const Value: Boolean);
    procedure SetAsDateTime(Index: Integer; const Value: TDateTime);
    procedure SetAsFloat(Index: Integer; const Value: Double);
    procedure SetAsInteger(Index: Integer; const Value: Integer);

    function GetFieldIsNull(Index: Integer): Boolean;
    function GetFieldIsMemo(Index: Integer): Boolean;
    function GetMemoAsString(Index: Integer): String;
    procedure SetMemoAsString(Index: Integer; Value: String);

    function GetAsBooleanByName(AFieldName: String): Boolean;
    function GetAsDateTimeByName(AFieldName: String): TDateTime;
    function GetAsFloatByName(AFieldName: String): Double;
    function GetAsIntegerByName(AFieldName: String): Integer;
    function GetAsStringByName(AFieldName: String): String;
    function GetAsTextByName(AFieldName: String): String;
    procedure SetAsBooleanByName(AFieldName: String; const Value: Boolean);
    procedure SetAsDateTimeByName(AFieldName: String; const Value: TDateTime);
    procedure SetAsFloatByName(AFieldName: String; const Value: Double);
    procedure SetAsIntegerByName(AFieldName: String; const Value: Integer);
    procedure SetAsStringByName(AFieldName: String; const Value: String);
    procedure SetAsTextByName(AFieldName: String; const Value: String);
    function GetMemoAsStringByName(AFieldName: String): String;
    procedure SetMemoAsStringByName(AFieldName: String; const Value: String);

  public
    destructor  Destroy; virtual;

    property DBFType: Byte read FDBFHeader.DBFType;                      // Тип файла (DBF_xxx)
    property LastUpdated: TDateTime read GetLastUpdated;                 // Дата последнего обновления
    property RecordLength: Word read FDBFHeader.RecordLength;             // Количество байтов, занимаемых записью
    property HasMemo: Boolean read GetHasMemo;
    property CodePage: Byte read FDBFHeader.CodePage;

    property FieldsCount: Integer read FFieldsCount;
    property FieldName[Index: Integer]: String read GetFieldName;
    property FieldNumber[NameField: String]: Integer read GetFieldNumber;
    property FieldType[Index: Integer]: TFieldType read GetFieldType;
    property FieldLength[Index: Integer]: Byte read GetFieldLength;
    property FieldDecimals[Index: Integer]: Byte read GetFieldDecimals;

    property RecordCount: DWord read FDBFHeader.RecordCount;             // Количество записей в таблице
    property CurrentRecord: DWord read FCurrentRecord write SetCurrentRecord;
    property BOF: Boolean read FBOF;
    property EOF: Boolean read FEOF;
    procedure First;
    procedure Last;
    procedure Next;
    procedure Prev;
    function Locate(AFieldName, AValue: String): Boolean; overload;
    function Locate(AFieldNames, AValues: array of String): Boolean; overload;
    function MaxOfField(AFieldName: String): Integer;

    procedure Append;
    procedure Post;
    property  IsDelete: Boolean read GetIsDelete write SetIsDelete;
    procedure RefreshRecord;
    procedure PackDBF;
    {    DescriptionDBF format:
    <CodePage>#2<BlockSize>#1
    <FieldName1>#2<FieldType1>#2<FieldLength1>#2<Decimals1>#1
    <FieldName2>#2<FieldType2>#2<FieldLength2>#2<Decimals2>#1
    ...
    <FieldNameN>#2<FieldTypeN>#2<FieldLengthN>#2<DecimalsN>#1


    BlockSize: recommend values 64, 128, 256, 512, 1024, 2048;
    FieldName: max length: 10
    FieldType: 'C' - Character; 'N' - Numeric; 'D' - Date; 'B' - Binary; 'L' - Logical, 'M' - Memo
    FieldLength: 'C' - 1..254; 'N' - 1..20; 'D' - 8; 'L' - 1; 'M' - 10; 'B' - 10
    Decimals: 'C' - 0; 'N' - 0..FieldLength-1; 'D' - 0; 'L' - 0; 'M' - 0; 'B' - 0

    }
    function NewDbf(AFileName: String; DescriptionDBF: String): Boolean;

    property FieldIsString[Index: Integer]: Boolean read GetFieldIsString;
    property FieldIsInteger[Index: Integer]: Boolean read GetFieldIsInteger;
    property FieldIsFloat[Index: Integer]: Boolean read GetFieldIsFloat;
    property FieldIsDateTime[Index: Integer]: Boolean read GetFieldIsDateTime;
    property FieldIsBoolean[Index: Integer]: Boolean read GetFieldIsBoolean;
    property FieldIsNull[Index: Integer]: Boolean read GetFieldIsNull;

    property AsText[Index: Integer]: String read GetAsText write SetAsText;
    property AsString[Index: Integer]: String read GetAsString write SetAsString;
    property AsInteger[Index: Integer]: Integer read GetAsInteger write SetAsInteger;
    property AsFloat[Index: Integer]: Double read GetAsFloat write SetAsFloat;
    property AsDateTime[Index: Integer]: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsBoolean[Index: Integer]: Boolean read GetAsBoolean write SetAsBoolean;

    property AsTextByName[FieldName: String]: String read GetAsTextByName write SetAsTextByName;
    property AsStringByName[FieldName: String]: String read GetAsStringByName write SetAsStringByName;
    property AsIntegerByName[FieldName: String]: Integer read GetAsIntegerByName write SetAsIntegerByName;
    property AsFloatByName[FieldName: String]: Double read GetAsFloatByName write SetAsFloatByName;
    property AsDateTimeByName[FieldName: String]: TDateTime read GetAsDateTimeByName write SetAsDateTimeByName;
    property AsBooleanByName[FieldName: String]: Boolean read GetAsBooleanByName write SetAsBooleanByName;

    procedure GetValue(Index: Integer; var Value);
    procedure SetValue(Index: Integer; var Value);

    property FieldIsMemo[Index: Integer]: Boolean read GetFieldIsMemo;
    property MemoAsString[Index: Integer]: String read GetMemoAsString write SetMemoAsString;
    property MemoAsStringByName[FieldName: String]: String read GetMemoAsStringByName write SetMemoAsStringByName;

    function GetMemoValue(Index: Integer; var Value: Pointer): DWord;
    procedure SetMemoValue(Index: Integer; Value: Pointer; ACount: DWord; ASigna: DWord = 0);

{ ??????? ftUnknown, ftCharacterBin, ftMemoBin, ftCurrency, ftInteger, }

    property Active: Boolean read FActive write SetActive;
    property AutoUpdate: Boolean read FAutoUpdate write FAutoUpdate;
    Property DBFModified: boolean read FDBFModified;

    property FileName: String read FFileName write SetFileName;
    property FileNameMemo: String read FFileNameMemo write FFileNameMemo;
    property Error: TDbfErrors read GetError;

    property OnScroll: TOnDbfEvent read FOnScroll write FOnScroll;
    property OnDelete: TOnDbfEvent read FOnDelete write FOnDelete;
    property OnAppend: TOnDbfEvent read FOnAppend write FOnAppend;
  end;

function NewmdvDBF(AFileName: String; AutoUpdate: Boolean; ReadOnly: Boolean = False): TKOLmdvDBF;

implementation

{$RANGECHECKS OFF}

function NewmdvDBF(AFileName: String; AutoUpdate: Boolean; ReadOnly: Boolean = False): TKOLmdvDBF;
begin
    New(Result, Create);
    Result.FAutoUpdate:= AutoUpdate;
    Result.FFileName:= AFileName;
    Result.FReadOnly:= ReadOnly;
end;

{ TmdvDBF }

procedure TmdvDBF.Append;
var Allowed: Boolean;
begin
    if not FActive or FReadOnly then Exit;
    if FAutoUpdate then Post;
    Allowed:= True;
    if Assigned(FOnAppend) then FOnAppend(@Self, Allowed);
    if not Allowed then Exit;
    FDBFHeader.RecordCount:= FDBFHeader.RecordCount + 1;
    try
      FDBFStream.Seek(0, spBegin); FDBFStream.Write(FDBFHeader, SizeOf(FDBFHeader));
      FCurrentRecord:= FDBFHeader.RecordCount-1;
      FillMemory(FRecordBuffer, FDBFHeader.RecordLength, $20);
      FRecordModified:= True;
      Post;
    except
      FError:= eAppendRecord;
      FDBFHeader.RecordCount:= FDBFHeader.RecordCount-1;
    end;
end;

destructor TmdvDBF.Destroy;
begin
    Active:= False;
    inherited;
end;

procedure TmdvDBF.First;
begin
    SetCurrentRecord(0);
    FBOF:= FCurrentRecord=0; FEOF:= False;
end;

function TmdvDBF.GetAsBoolean(Index: Integer): Boolean;
begin
    Result:= False;
    try
      if not FieldIsString[Index] then Exit;
      Result:= (AsText[Index][1] in ['T', 't', 'Y', 'y'])
    except
      FError:= eReadField;
    end;
end;

function TmdvDBF.GetAsDateTime(Index: Integer): TDateTime;
var SS, S: String;
begin
    Result:= 0;
    try
      if not FieldIsDateTime[Index] then Exit;
      SetLength(S, 14);
      FillMemory(PChar(S), 14, Byte('0'));
      SS:= AsText[Index];
      Move(SS[1], S[1], Length(SS));
      Result:= Str2DateTimeFmt('yyyyMMddHHmmss', S);
    except
      FError:= eReadField;
    end;
end;

function TmdvDBF.GetAsFloat(Index: Integer): Double;
begin
    Result:= 0;
    try
      if not FieldIsFloat[Index] then Exit;
      Result:= Str2Double(Trim(AsText[Index]));
    except
      FError:= eReadField;
    end;
end;

function TmdvDBF.GetAsInteger(Index: Integer): Integer;
begin
    Result:= 0;
    try
      if not FieldIsInteger[Index] then Exit;
      Result:= Str2Int(Trim(AsText[Index]));
    except
      FError:= eReadField;
    end;
end;

function TmdvDBF.GetAsString(Index: Integer): String;
var S: String;
    D: TDateTime;
begin
    Result:= '';
    try
      if not FieldIsString[Index] then Exit;
      S:= AsText[Index];
      case FieldType[Index] of
        ftCharacter: Result:= TrimRight(S);
        ftDate, ftDateTime: begin
          D:= AsDateTime[Index];
          Result:= Date2StrFmt('dd.MM.yyyy', D);
          if FieldType[Index] = ftDateTime then
            Result:= Result + Time2StrFmt(' HH:mm:ss', D);
        end;
        ftLogical: if S[1] in ['T', 't', 'Y', 'y', 'F', 'f', 'N', 'n'] then Result:= S[1] else Result:= 'F';
        ftFloat, ftNumeric: begin
          Result:= Double2Str(AsFloat[Index]);
        end;
      end;
    except
      FError:= eReadField;
    end;
end;

function TmdvDBF.GetError: TDbfErrors;
begin
    Result:= FError;
    FError:= eNoErrors;
end;

function TmdvDBF.GetFieldDecimals(Index: Integer): Byte;
begin
    Result:= FDBFFields[Index].Decimals;
end;

function TmdvDBF.GetFieldIsBoolean(Index: Integer): Boolean;
begin
    Result:= FieldType[Index] = ftLogical;
end;

function TmdvDBF.GetFieldIsDateTime(Index: Integer): Boolean;
begin
    Result:= FieldType[Index] in [ftDate, ftDateTime];
end;

function TmdvDBF.GetFieldIsFloat(Index: Integer): Boolean;
begin
    Result:= FieldType[Index] in [ftFloat, ftNumeric];
end;

function TmdvDBF.GetFieldIsInteger(Index: Integer): Boolean;
begin
    Result:= (FieldType[Index] in [ftFloat, ftNumeric])and(FDBFFields[Index].Decimals = 0);
end;

function TmdvDBF.GetFieldIsMemo(Index: Integer): Boolean;
begin
    Result:= FieldType[Index] in [ftMemo, ftGeneral, ftBinary, ftPicture];
end;

function TmdvDBF.GetFieldIsNull(Index: Integer): Boolean;
begin
    Result:= ((FDBFFields[Index].FieldFlags and $02) = $02);
end;

function TmdvDBF.GetFieldIsString(Index: Integer): Boolean;
begin
    Result:= FieldType[Index] in [ftCharacter, ftDate, ftLogical, ftFloat, ftNumeric, ftDateTime];
end;

function TmdvDBF.GetFieldLength(Index: Integer): Byte;
begin
    Result:= FDBFFields[Index].FieldLength;
end;

function TmdvDBF.GetFieldName(Index: Integer): String;
begin
    Result:= FDBFFields[Index].FieldName;
end;

function TmdvDBF.GetFieldNumber(NameField: String): Integer;
var i: Integer;
begin
    Result:= -1;
    for i:= 0 to FFieldsCount-1 do
      if String(PChar(@FDBFFields[i].FieldName)) = NameField then begin
        Result:= i; Break;
      end;
end;

function TmdvDBF.GetFieldType(Index: Integer): TFieldType;
begin
    case FDBFFields[Index].FieldType of
      'C': if (FDBFFields[Index].FieldFlags and $04)=$04 then Result:= ftCharacterBin else Result:= ftCharacter;
      'Y': Result:= ftCurrency;
      'N': Result:= ftNumeric;
      'F': Result:= ftFloat;
      'D': Result:= ftDate;
      'T': Result:= ftDateTime;
      'B': Result:= ftBinary;
      'I': Result:= ftInteger;
      'L': Result:= ftLogical;
      'M': if (FDBFFields[Index].FieldFlags and $04)=$04 then Result:= ftMemoBin else Result:= ftMemo;
      'G': Result:= ftGeneral;
      'P': Result:= ftPicture;
      else Result:= ftUnknown;
    end;
end;

function TmdvDBF.GetHasMemo: Boolean;
begin
     Result:= FDBFHeader.TableFlags and $02 = $02;
end;

function TmdvDBF.InvertDWord(var Value: DWord): DWord;
type TB = array[0..3] of Byte;
begin
    TB(Result)[0]:= TB(Value)[3];
    TB(Result)[1]:= TB(Value)[2];
    TB(Result)[2]:= TB(Value)[1];
    TB(Result)[3]:= TB(Value)[0];
end;

function TmdvDBF.GetIsDelete: Boolean;
begin
    Result:= False;
    if not FActive then Exit;
    Result:= (FRecordBuffer^[0] = $2A)
end;

function TmdvDBF.GetLastUpdated: TDateTime;
begin
    EncodeDate(FDBFHeader.LastUpdated[1], FDBFHeader.LastUpdated[2], FDBFHeader.LastUpdated[3], Result);
end;

function TmdvDBF.GetMemoAsString(Index: Integer): String;
var P: Pointer;
    Sz: DWord;
begin
    Sz:= GetMemoValue(Index, P);
    SetLength(Result, Sz);
    if Sz > 0 then begin
      Move(P^, Result[1], Sz);
      DisposeMem(P);
    end;
end;

function TmdvDBF.GetMemoValue(Index: Integer; var Value: Pointer): DWord;
var BlockNum, BlockSize, BlockCount: DWord;
    k: Integer;
    MemoType: TMemoType;
begin
    Value:= nil; Result:= 0;
    MemoType:= GetMemoInfo(Index, BlockNum, BlockSize, BlockCount, Result);
    if (BlockCount = 0) or (BlockNum = 0) or (MemoType = mtUnknown) then Exit;

    case MemoType of
      mtFoxPro, mtdBaseIV: begin
        FDBFMemoStream.Seek(BlockNum * BlockSize+SizeOf(TMemoBlockHeader), spBegin);
        Value:= AllocMem(Result);
        FDBFMemoStream.Read(Value^, Result);
      end;
      mtdBase: begin
        FDBFMemoStream.Seek(BlockNum * BlockSize, spBegin);
        Result:= 0;
        repeat
          inc(Result, BlockSize); ReallocMem(Value, Result+1);
          FDBFMemoStream.Read(PChar(Value)[Result-512], BlockSize);
          PChar(Value)[Result+1]:= #0;
          k:= Pos(#$1A, PChar(Value));
          if k>0 then begin
            Result:= k-1;
            ReallocMem(Value, Result);
            Break;
          end;
        until FDBFMemoStream.Position >= FDBFMemoStream.Size;
      end;
    end;
end;

procedure TmdvDBF.GetValue(Index: Integer; var Value);
begin
    try
      Move(FRecordBuffer^[FDBFFields[Index].Address], Value, FDBFFields[Index].FieldLength);
    except
      FError:= eReadField;
    end;
end;

procedure TmdvDBF.Last;
begin
    SetCurrentRecord(FDBFHeader.RecordCount-1);
    FBOF:= False; FEOF:= FCurrentRecord = FDBFHeader.RecordCount-1;
end;

procedure TmdvDBF.Next;
begin
    SetCurrentRecord(FCurrentRecord+1);
    FBOF:= False; FEOF:= FCurrentRecord = FDBFHeader.RecordCount-1;
end;

procedure TmdvDBF.Post;
begin
    if FRecordModified then WriteRecord;
end;

procedure TmdvDBF.Prev;
begin
    SetCurrentRecord(FCurrentRecord-1);
    FBOF:= FCurrentRecord=0; FEOF:= False;
end;

function TmdvDBF.ReadFields: Boolean;
var DBFField: TDBFField;
    Addr: DWord;
begin
    try
      Result:= True;
      FFieldsCount:= 0;
      FDBFFields:= AllocMem(SizeOf(TDBFField)*((FDBFHeader.HeaderLength - SizeOf(TDBFHeader)) div SizeOf(TDBFField)));
      FDBFStream.Seek(SizeOf(TDBFHeader), spBegin); FHasMemo:= False;
      Addr:= 1;
      repeat
        FDBFStream.Read(DBFField, SizeOf(TDBFField));
        if DBFField.FieldName[1] <> #13 then begin
          if DBFField.Address = 0 then DBFField.Address:= Addr;
          FDBFFields[FFieldsCount]:= DBFField;
          FHasMemo:= FHasMemo or FieldIsMemo[FFieldsCount];
          inc(FFieldsCount);
        end;
        inc(Addr, DBFField.FieldLength);
      until DBFField.FieldName[1] = #13;
    except
      Result:= False;
      FError:= eDBFHeader;
    end;
end;

procedure TmdvDBF.ReadRecord;
Begin
    if not FActive then Exit;
    try
      FDBFStream.Seek(FDBFHeader.HeaderLength + FCurrentRecord*FDBFHeader.RecordLength, spBegin);
      FDBFStream.Read(FRecordBuffer^, FDBFHeader.RecordLength);
      FRecordModified := False;
    except
      FError:= eReadRecord;
    end;
end;

procedure TmdvDBF.RefreshRecord;
begin
    if not FActive then Exit;
    ReadRecord;
end;

procedure TmdvDBF.SetActive(const Value: Boolean);
const ReadWrite: array [Boolean] of DWord = (ofOpenReadWrite, ofOpenRead);
//    Stream: PStream;
begin
    if Value = FActive then Exit;
    if Value then begin
      if not FileExists(FFileName) then begin FError:= eFileNotExist; Exit; end;
      FDBFStream:= NewFileStream(FFileName, ReadWrite[FReadOnly] or ofShareDenyNone or{ofShareDenyWrite or }ofOpenExisting);
      if FDBFStream.Handle = 0 then begin FError:= eFileOpen; Exit; end;

{FDBFStream:= NewMemoryStream;
Stream:= NewReadFileStream(FFileName);
FDBFStream.Size:= Stream.Size;
Stream.Read(FDBFStream.Memory^, Stream.Size);
Stream.Free;}
      FDBFStream.Read(FDBFHeader, SizeOf(TDBFHeader));
      if not ReadFields then Exit;
      FRecordBuffer:= AllocMem(FDBFHeader.RecordLength);

      if FHasMemo then begin
        if not FileExists(FFileNameMemo) then begin
          FFileNameMemo:= ChangeFileExt(FFileName, '.fpt');
          if not FileExists(FFileNameMemo) then begin
            FFileNameMemo:= ChangeFileExt(FFileName, '.dbt');
            if not FileExists(FFileNameMemo) then begin
              FFileNameMemo:= '';
            end;
          end;
        end;
        if FFileNameMemo <> '' then begin

          FDBFMemoStream:= NewFileStream(FFileNameMemo, ReadWrite[FReadOnly] {or ofShareDenyWrite }or ofShareDenyNone or ofOpenExisting);
{FDBFMemoStream:= NewMemoryStream;
Stream:= NewReadFileStream(FFileNameMemo);
FDBFMemoStream.Size:= Stream.Size;
Stream.Read(FDBFMemoStream.Memory^, Stream.Size);
Stream.Free;}

          if FDBFHeader.DBFType in [DBF_FoxPro2xMemo, DBF_VisualFoxPro, DBF_VisualFoxProInc] then FDBFMemoStream.Read(FFPTHeader, SizeOf(TFPTHeader))
          else FDBFMemoStream.Read(FDBTHeader, SizeOf(TDBTHeader));
        end
        else FHasMemo:= False;
      end;

      FCurrentRecord:= 0; FDBFModified:= False; FRecordModified:= False;
      FEOF:= False; FBOF:= False;
      FActive := True;
    end
    else begin
       if FAutoUpdate then Post;
       if FDBFModified and not FReadOnly then begin
         SetDate(FDBFHeader.LastUpdated);
         FDBFStream.Seek(0, spBegin); FDBFStream.Write(FDBFHeader, SizeOf(TDBFHeader));
       end;
       DisposeMem(Pointer(FDBFFields));
       DisposeMem(Pointer(FRecordBuffer));
       FDBFStream.Free;
       if FHasMemo then FDBFMemoStream.Free;
       FFileNameMemo:= '';
       FActive := False;
    end
end;

procedure TmdvDBF.SetAsBoolean(Index: Integer; const Value: Boolean);
begin
    if Value then AsString[Index]:= 'T' else AsString[Index]:= 'F';
end;

procedure TmdvDBF.SetAsDateTime(Index: Integer; const Value: TDateTime);
begin
    AsString[Index]:= Date2StrFmt('yyyyMMdd', Value)+Time2StrFmt('HHmmss', Value);
end;

procedure TmdvDBF.SetAsFloat(Index: Integer; const Value: Double);
begin
    AsString[Index]:= Double2Str(Value);
end;

procedure TmdvDBF.SetAsInteger(Index: Integer; const Value: Integer);
begin
    AsString[Index]:= Int2Str(Value);
end;

procedure TmdvDBF.SetAsString(Index: Integer; const Value: String);
var S, SS, I: String;
    ValidValue: Boolean;
    lI, lp, lF, k: Integer;
    D: TDateTime;
begin
    try
      S:= Value;
      ValidValue:= False;
      case FieldType[Index] of
        ftCharacter: begin
          ValidValue:= True;
        end;
        ftDate, ftDateTime: begin
          D:= Str2DateTimeFmt('yyyyMMddHHmmss', Value); S:= Value;
          ValidValue:= (Value = Date2StrFmt('yyyyMMdd', D)+Time2StrFmt('HHmmss', D));
        end;
        ftLogical: begin
          ValidValue:= (Length(Value)=1);
          if ValidValue then ValidValue:= Value[1] in ['T', 't', 'Y', 'y', 'F', 'f', 'N', 'n'];
        end;
        ftFloat, ftNumeric: begin
          SS:= Trim(Value);
          ValidValue:= Double2Str((Str2Double(SS))) = SS;
          if ValidValue then begin
            lP:= FDBFFields[Index].FieldLength - FDBFFields[Index].Decimals;
            lI:= FDBFFields[Index].FieldLength - FDBFFields[Index].Decimals - 1;
            lF:= FDBFFields[Index].Decimals;
            if FDBFFields[Index].Decimals = 0 then begin
              lI:= FDBFFields[Index].FieldLength; lF:= 0; lP:= 0;
            end;
            if FDBFFields[Index].FieldLength <= FDBFFields[Index].Decimals then begin
              lI:= 0; lF:= FDBFFields[Index].FieldLength; lP:= 0;
            end;
            if lP > 0 then S:= '.' else S:='';

            I:= Parse(SS, '.');
            lP:= Min(Length(I), lI);
            SetLength(I, lP);
            for k:= lP to lI-1 do I:= ' '+I;

            lP:= Min(Length(SS), lF);
            SetLength(SS, lP);
            for k:= lP to lF-1 do SS:= SS+'0';
            S:= I+S+SS;
          end;
        end;
      end;
      if ValidValue then begin
        FillMemory(@(FRecordBuffer^[FDBFFields[Index].Address]), FDBFFields[Index].FieldLength, Byte(' '));
        SetString(Index, S);
      end
      else FError:= eInvalidValue;
   except
      FError:= eWriteField;
   end;
end;

procedure TmdvDBF.SetCurrentRecord(const Value: DWord);
var Allowed: Boolean;
begin
    if not FActive then Exit;
    Allowed:= True;
    if Assigned(FOnScroll) then FOnScroll(@Self, Allowed);
    if not Allowed then Exit;
    if FAutoUpdate then Post;
    FCurrentRecord := Max(0, Min(FDBFHeader.RecordCount-1, Value));
    ReadRecord;
end;

procedure TmdvDBF.SetFileName(const Value: String);
begin
    if FFileName <> Value then begin
      Active:= False;
      FFileName := Value;
    end;
end;

procedure TmdvDBF.SetIsDelete(const Value: Boolean);
const DelFlag: array [Boolean] of Byte = ($20, $2A);
var Allowed: Boolean;
begin
    if not FActive then Exit;
    Allowed:= True;
    if Assigned(FOnDelete) then FOnDelete(@Self, Allowed);
    if not Allowed then Exit;
    FRecordBuffer^[0] := DelFlag[Value];
    FRecordModified:= True;
end;

procedure TmdvDBF.SetMemoAsString(Index: Integer; Value: String);
begin
    SetMemoValue(Index, PChar(Value), Length(Value), 1);
end;

procedure TmdvDBF.SetMemoValue(Index: Integer; Value: Pointer; ACount: DWord; ASigna: DWord = 0);
var BSize, BlockNum, BlockSize, BlockCount, Size, mh: DWord;
    MemoType: TMemoType;
    MemoBlockHeader: TMemoBlockHeader;
    S: String;
begin
    if FReadOnly then Exit;
    
    MemoType:= GetMemoInfo(Index, BlockNum, BlockSize, BlockCount, Size);
    if (BlockCount = 0) or (MemoType = mtUnknown) then Exit;

    if MemoType <> mtdBase then mh:= SizeOf(TMemoBlockHeader) else mh:= 1;
    BSize:= Size+mh;
    BSize:= (BSize div BlockSize)*BlockSize + BlockSize*DWord(Ord((BSize mod BlockSize)>0));
    if (BlockNum = 0) or (BSize < ACount + mh) then begin
      BSize:= ACount + mh;
      BSize:= (BSize div BlockSize)*BlockSize + BlockSize*DWord(Ord((BSize mod BlockSize)>0));
      BlockNum:= BlockCount;
      BlockCount:= BlockCount + BSize div BlockSize;
    end;
    FDBFMemoStream.Size:= BlockCount * BlockSize;
    FDBFMemoStream.Seek(0, spBegin);

    if MemoType = mtFoxPro then begin
      MemoBlockHeader.Length:= InvertDWord(ACount);
      MemoBlockHeader.BlockSignature:= InvertDWord(ASigna);
      FFPTHeader.NextFree:= InvertDWord(BlockCount);
      FDBFMemoStream.Write(FFPTHeader, SizeOf(TFPTHeader));
    end
    else begin
      MemoBlockHeader.Length:= ACount + 8;
      if MemoType = mtdBaseIV then MemoBlockHeader.BlockSignature:= $08FFFF
      else MemoBlockHeader.BlockSignature:= ASigna;
      FDBTHeader.NextFree:= BlockCount;
      FDBFMemoStream.Write(FDBTHeader, SizeOf(TDBTHeader));
    end;

    if FDBFFields[Index].FieldLength = 4 then
      SetValue(Index, BlockNum)
    else begin
      S:= Int2Str(BlockNum);
      for mh:= Length(S)+1 to FDBFFields[Index].FieldLength do S:= ' '+S;
      SetValue(Index, S[1]);
    end;
    Post;

    case MemoType of
      mtFoxPro, mtdBaseIV: begin
        FDBFMemoStream.Seek(BlockNum * BlockSize, spBegin);
        FDBFMemoStream.Write(MemoBlockHeader, SizeOf(TMemoBlockHeader));
        FDBFMemoStream.Write(Value^, ACount);
      end;
      mtdBase: begin
        FDBFMemoStream.Seek(BlockNum * BlockSize, spBegin);
        FDBFMemoStream.Write(Value^, ACount);
        S:= #$1A;
        FDBFMemoStream.Write(S[1], 1);
      end;
    end;
end;

procedure TmdvDBF.SetString(Index: Integer; Value: String);
begin
    try
      Move(Value[1], FRecordBuffer^[FDBFFields[Index].Address], Min(Length(Value), FDBFFields[Index].FieldLength));
      FDBFModified:= True;
      FRecordModified:= True;
    except
      FError:= eWriteField;
    end;
end;

procedure TmdvDBF.SetValue(Index: Integer; var Value);
begin
    try
      Move(Value, FRecordBuffer^[FDBFFields[Index].Address], FDBFFields[Index].FieldLength);
      FDBFModified:= True;
      FRecordModified:= True;
    except
      FError:= eWriteField;
    end;
end;

procedure TmdvDBF.WriteRecord;
Begin
    if not FActive or FReadOnly then Exit;
    try
      FDBFStream.Seek(FDBFHeader.HeaderLength + FCurrentRecord*FDBFHeader.RecordLength, spBegin);
      FDBFStream.Write(FRecordBuffer^, FDBFHeader.RecordLength);
      FRecordModified := False;
      FDBFModified:= True;
    except
      FError:= eWriteRecord; 
    end;
end;

function TmdvDBF.GetAsText(Index: Integer): String;
begin
    Result:= '';
    try
      if not FieldIsString[Index] then Exit;
      SetLength(Result, FDBFFields[Index].FieldLength);
      Move(FRecordBuffer^[FDBFFields[Index].Address], Result[1], FDBFFields[Index].FieldLength);
    except
      FError:= eReadField;
    end;
end;

procedure TmdvDBF.SetAsText(Index: Integer; Value: String);
begin
    try
      FillMemory(@(FRecordBuffer^[FDBFFields[Index].Address]), FDBFFields[Index].FieldLength, Byte(' '));
      SetValue(Index, Value[1]);
   except
      FError:= eWriteField;
   end;
end;

function TmdvDBF.GetMemoInfo(Index: Integer; var ABlockNum, ABlockSize, ABlockCount, ASize: DWord): TMemoType;
var S: String;
    MemoBlockHeader: TMemoBlockHeader;
begin
    ABlockNum:=0; ABlockSize:=0; ABlockCount:=0; ASize:= 0;
    Result:= mtUnknown;
    if not FieldIsMemo[Index] then Exit;

    if FDBFFields[Index].FieldLength = 4 then
      Move(FRecordBuffer^[FDBFFields[Index].Address], ABlockNum, 4)
    else begin
      SetLength(S, FDBFFields[Index].FieldLength);
      Move(FRecordBuffer^[FDBFFields[Index].Address], S[1], FDBFFields[Index].FieldLength);
      ABlockNum:= Str2Int(Trim(S));
    end;

    if FDBFHeader.DBFType in [DBF_FoxPro2xMemo, DBF_VisualFoxPro, DBF_VisualFoxProInc] then Result:= mtFoxPro;
    if FDBFHeader.DBFType in [DBF_dBaseIV, DBF_dBaseIVSQLtable, DBF_dBaseIVSQLsystem, DBF_dBaseIVSQLtableMemo, DBF_dBaseIVMemo, DBF_dBaseV] then Result:= mtdBaseIV;
    if FDBFHeader.DBFType in [DBF_FoxBASE, DBF_FoxBASE_, DBF_dBaseIIIplus, DBF_dBaseIIIplusMemo] then Result:= mtdBase;

    ABlockSize:= 512;
    ABlockCount:= FDBTHeader.NextFree;
    ASize:= 0;

    if Result = mtFoxPro then begin
      ABlockSize:= System.Swap(FFPTHeader.BlockSize);
      ABlockCount:= InvertDWord(FFPTHeader.NextFree);
    end;
    if Result = mtdBaseIV then begin
      ABlockSize:= FDBTHeader.BlockSize;
    end;
    if ABlockSize = 0 then ABlockSize:= 512;

    if ABlockNum = 0 then Exit;
    FDBFMemoStream.Seek(ABlockNum * ABlockSize, spBegin);
    FDBFMemoStream.Read(MemoBlockHeader, SizeOf(TMemoBlockHeader));

    if Result = mtFoxPro then ASize:= InvertDWord(MemoBlockHeader.Length);
    if Result = mtdBaseIV then ASize:= MemoBlockHeader.Length - 8;
end;

procedure TmdvDBF.PackDBF;
var ReadPos, WritePos, Rec, RecCount: DWord;
S:string;
begin
  if FReadOnly then Exit;
  Post;
  ReadPos:= FDBFHeader.HeaderLength; WritePos:= FDBFHeader.HeaderLength;
  Rec:= 0; RecCount:= 0;
  while Rec < FDBFHeader.RecordCount do begin
     FDBFStream.Seek(ReadPos, spBegin);
     FDBFStream.Read(FRecordBuffer^, FDBFHeader.RecordLength);
     inc(ReadPos, FDBFHeader.RecordLength);
     inc(Rec);
     if FRecordBuffer^[0] <> $2A then begin
       FDBFStream.Seek(WritePos, spBegin);
       FDBFStream.Write(FRecordBuffer^, FDBFHeader.RecordLength);
       inc(WritePos, FDBFHeader.RecordLength);
       inc(RecCount);
     end
     else FDBFModified:= True;
  end;
  FDBFStream.Size:= WritePos;

  FDBFHeader.RecordCount := RecCount;
  FDBFStream.Seek(0, spBegin);
  FDBFStream.Write(FDBFHeader, SizeOf(TDBFHeader));
  FDBFStream.Seek(0, spEnd);
  S:= #$1A;
  FDBFStream.Write(S[1], 1);
  CurrentRecord:= 0;
end;

function TmdvDBF.Locate(AFieldName, AValue: String): Boolean;
var i, FieldNum: Integer;
begin
    Result:= False;
    FieldNum:= FieldNumber[AFieldName];
    if FieldNum<0 then Exit;
    for i:= 0 to RecordCount-1 do begin
      CurrentRecord:= i;
      if IsDelete then Continue;
      Result:= AsString[FieldNum] = AValue;
      if Result then Break;
    end;
end;

function TmdvDBF.Locate(AFieldNames, AValues: array of String): Boolean;
var i, j: Integer;
    FieldNums: array of Integer;
begin
    Result:= False;
    SetLength(FieldNums, Min(Length(AFieldNames), Length(AValues)));
    try
      for j:= Low(FieldNums) to High(FieldNums) do begin
        FieldNums[j]:= FieldNumber[AFieldNames[j]];
        if FieldNums[j]<0 then Exit;
      end;
      for i:= 0 to RecordCount-1 do begin
        CurrentRecord:= i;
        if IsDelete then Continue;
        Result:= True;
        for j:= Low(FieldNums) to High(FieldNums) do
          Result:= Result and (AsString[FieldNums[j]] = AValues[j]);
        if Result then Break;
      end;
    finally
      SetLength(FieldNums, 0);
    end;
end;

function TmdvDBF.GetAsBooleanByName(AFieldName: String): Boolean;
begin
    Result:= AsBoolean[FieldNumber[AFieldName]];
end;

function TmdvDBF.GetAsDateTimeByName(AFieldName: String): TDateTime;
begin
    Result:= AsDateTime[FieldNumber[AFieldName]];
end;

function TmdvDBF.GetAsFloatByName(AFieldName: String): Double;
begin
    Result:= AsFloat[FieldNumber[AFieldName]];
end;

function TmdvDBF.GetAsIntegerByName(AFieldName: String): Integer;
begin
    Result:= AsInteger[FieldNumber[AFieldName]];
end;

function TmdvDBF.GetAsStringByName(AFieldName: String): String;
begin
    Result:= AsString[FieldNumber[AFieldName]];
end;

function TmdvDBF.GetAsTextByName(AFieldName: String): String;
begin
    Result:= AsText[FieldNumber[AFieldName]];
end;

procedure TmdvDBF.SetAsBooleanByName(AFieldName: String; const Value: Boolean);
begin
    AsBoolean[FieldNumber[AFieldName]]:= Value;
end;

procedure TmdvDBF.SetAsDateTimeByName(AFieldName: String; const Value: TDateTime);
begin
    AsDateTime[FieldNumber[AFieldName]]:= Value;
end;

procedure TmdvDBF.SetAsFloatByName(AFieldName: String; const Value: Double);
begin
    AsFloat[FieldNumber[AFieldName]]:= Value;
end;

procedure TmdvDBF.SetAsIntegerByName(AFieldName: String; const Value: Integer);
begin
    AsInteger[FieldNumber[AFieldName]]:= Value;
end;

procedure TmdvDBF.SetAsStringByName(AFieldName: String; const Value: String);
begin
    AsString[FieldNumber[AFieldName]]:= Value;
end;

procedure TmdvDBF.SetAsTextByName(AFieldName: String; const Value: String);
begin
    AsText[FieldNumber[AFieldName]]:= Value;
end;

function TmdvDBF.GetMemoAsStringByName(AFieldName: String): String;
begin
    Result:= MemoAsString[FieldNumber[AFieldName]];
end;

procedure TmdvDBF.SetMemoAsStringByName(AFieldName: String; const Value: String);
begin
    MemoAsString[FieldNumber[AFieldName]]:= Value;
end;

function TmdvDBF.MaxOfField(AFieldName: String): Integer;
var i, FieldNum, k: Integer;
begin
    Result:= 0;
    FieldNum:= FieldNumber[AFieldName];
    k:= CurrentRecord;
    if FieldNum<0 then Exit;
    for i:= 0 to RecordCount-1 do begin
      CurrentRecord:= i;
      if IsDelete then Continue;
      Result:= Max(Result, AsInteger[FieldNum]);
    end;
    CurrentRecord:= k;
end;

function TmdvDBF.NewDbf(AFileName: String; DescriptionDBF: String): Boolean;
var IsMemo: Boolean;
    i, _FieldsCount, _RecordLength, _BlockSize: Word;
    _CodePage: Byte;
    _DBFHeader: TDBFHeader;
    _DBFFields: PDBFFields;
    _DBTHeader: TDBTHeader;
    S, SS: String;
    Addr: DWord;
    Stream: PStream;
begin
    Active:= False;
    Result:= False;

    S:= Parse(DescriptionDBF, #1);
    _CodePage:= Str2Int(Parse(S, #2));
    _BlockSize:= Str2Int(S);

    _FieldsCount:= 0; S:= DescriptionDBF;
    while S <> '' do begin
      Parse(S, #1); Inc(_FieldsCount);
    end;
    if (_FieldsCount = 0) or (_FieldsCount > 255) then Exit;

    GetMem(_DBFFields, _FieldsCount*SizeOf(TDBFField));
    try
      IsMemo:= False;
      i:= 0; Addr:= 1; _RecordLength:= 1;
      while DescriptionDBF <> '' do begin
        S:= Parse(DescriptionDBF, #1);
        FillChar(_DBFFields^[i], SizeOf(TDBFFields), 0);
        with _DBFFields[i] do begin
          // Название поля
          SS:= Parse(S, #2);
          if (SS = '') or (Length(SS) > 10) then Exit;
          Move(SS[1], FieldName, Length(SS));
          // Тип поля 'C' - Character; 'N' - Numeric; 'D' - Date; 'B' - Binary; 'L' - Logical, 'M' - Memo
          SS:= Parse(S, #2);
          if (SS = '') or (Length(SS) <> 1) then Exit;
          FieldType:= SS[1];
          // Длина поля (в байтах)
          FieldLength:= Str2Int(Parse(S, #2));
          case FieldType of
            'C': if (FieldLength = 0) or (FieldLength > 254) then Exit;
            'N': if (FieldLength = 0) or (FieldLength > 20) then Exit;
            'D': FieldLength:= 8;
            'L': FieldLength:= 1;
            'B', 'M': begin
              FieldLength:= 10;
              IsMemo:= True;
            end;
            else Exit;
          end;
          // Длина десятичной части
          SS:= Parse(S, #2);
          if FieldType = 'N' then
            Decimals:= Max(0, Min(FieldLength-1, Str2Int(SS)));
          // Флаг поля
//          if FieldType in ['B', 'M'] then FieldFlags:= $04;
//          if FieldType = 'C' then FieldFlags:= FieldFlags or $02;

          Address:= Addr;
          inc(_RecordLength, FieldLength);
          inc(Addr, FieldLength);
        end;

        inc(i);
      end;

      FillChar(_DBFHeader, SizeOf(_DBFHeader), 0);
      with _DBFHeader do begin
        if IsMemo then DBFType:= DBF_dBaseIVMemo else DBFType:= DBF_dBaseIV;   // Тип файла (DBF_xxx)
        SetDate(LastUpdated);                                                  // Дата последнего обновления в формате YYMMDD
        RecordCount:= 0;                                                       // Количество записей в таблице
        HeaderLength:= SizeOf(_DBFHeader) + _FieldsCount * SizeOf(TDBFField) + 1; // Количество байт, занимаемых заголовком
        RecordLength:= _RecordLength;                                          // Количество байт, занимаемых записью
        //Reserved_1: array [1..16] of Byte;                                   // 3-Зарезервированная область; 13 - Зарезервировано для сетевой версии dBASE III PLUS
        if IsMemo then TableFlags:= $02;                                       // $01 - file has a structural .cdx; $02 - file has a Memo field; $04 - file is a database (.dbc). This byte can contain the sum of any of the above values. For example, the value 0x03 indicates the table has a structural .cdx and a Memo field.
        CodePage:= _CodePage;                                                   // Кодовая страница
        //Reserved_2: Word;                                                    // Зарезервированная область
      end;

      Stream:= NewWriteFileStream(AFileName);
      Stream.Size:= 0;
      Stream.Write(_DBFHeader, SizeOf(_DBFHeader));
      for i:= 0 to _FieldsCount-1 do
        Stream.Write(_DBFFields[i], SizeOf(TDBFFields));

      S:= #$0D#$1A;
      Stream.Write(S[1], 2);
      Stream.Free;

      if IsMemo then begin
        _BlockSize:= Max(_BlockSize, SizeOf(_DBTHeader));
        FillChar(_DBTHeader, SizeOf(_DBTHeader), 0);

        with _DBTHeader do begin
          NextFree:= 512  div _BlockSize + Ord(512 mod _BlockSize > 0);
          BlockSize:= _BlockSize;
        end;
		  Stream:= NewWriteFileStream(ChangeFileExt(AFileName, '.dbt'));
		  Stream.Size:= 0;
		  Stream.Write(_DBTHeader , SizeOf(_DBTHeader));
		  Stream.Size:= _DBTHeader.NextFree*_DBTHeader.BlockSize;
		  Stream.Free;
      end;
    finally
      FreeMem(_DBFFields);
    end;


    FFileName:= AFileName;
    Active:= True;
    Result:= True;
end;

procedure TmdvDBF.SetDate(var ADate: TYYMMDD);
var Y, M, D: Word;
begin
    DecodeDate(Now, Y, M, D);
    ADate[1]:= Lo(Y mod 100); ADate[2]:= Lo(M); ADate[3]:= Lo(D);
end;

end.
