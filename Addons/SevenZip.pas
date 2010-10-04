unit SevenZip;

interface

uses KOL;

const //Property IDs
  kEnd                       = $00;

  kHeader                    = $01;

  kArchiveProperties         = $02;

  kAdditionalStreamsInfo     = $03;
  kMainStreamsInfo           = $04;
  kFilesInfo                 = $05;

  kPackInfo                  = $06;
  kUnPackInfo                = $07;
  kSubStreamsInfo            = $08;

  kSize                      = $09;
  kCRC                       = $0A;

  kFolder                    = $0B;

  kCodersUnPackSize          = $0C;
  kNumUnPackStream           = $0D;

  kEmptyStream               = $0E;
  kEmptyFile                 = $0F;
  kAnti                      = $10;

  kName                      = $11;
  kCreationTime              = $12;
  kLastAccessTime            = $13;
  kLastWriteTime             = $14;
  kWinAttributes             = $15;
  kComment                   = $16;

  kEncodedHeader             = $17;

const kSignature: array [0..5] of Char = ('7', 'z', #$BC, #$AF, #$27, #$1C);

type //7z format headers
  REAL_UINT64 = Int64;

  TArchiveVersion = packed record
    Major: Byte;   // now = 0
    Minor: Byte;   // now = 2
  end;

  TStartHeader = packed record
    NextHeaderOffset: REAL_UINT64;
    NextHeaderSize: REAL_UINT64;
    NextHeaderCRC: Cardinal;
  end;

  TSignatureHeader = packed record
    ArchiveVersion: TArchiveVersion;
    StartHeaderCRC: Cardinal;
    StartHeader: TStartHeader;
  end;

  PUInt32 = ^UInt32;
  UInt32 = Cardinal;
  PUInt64 = ^UInt64;
  UInt64 = Int64;

  CFileSize = UInt64;
  PCFileSize = PUInt64;

  CMethodID= UInt64;

  CSzByteBuffer = packed record
    Capacity: Cardinal;
    Items: PByte;
  end;

  PCCoderInfo = ^CCoderInfo;
  CCoderInfo = packed record
    NumInStreams: UInt32;
    NumOutStreams: UInt32;
    MethodID: CMethodID;
    Properties: CSzByteBuffer;
  end;

  PCBindPair = ^CBindPair;
  CBindPair = packed record
    InIndex: UInt32;
    OutIndex: UInt32;
  end;

  PCFolder = ^CFolder;
  CFolder = packed record
    NumCoders: UInt32;
    Coders: PCCoderInfo;
    NumBindPairs: UInt32;
    BindPairs: PCBindPair;
    NumPackStreams: UInt32;
    PackStreams: UInt32;
    UnPackSizes: PCFileSize;
    UnPackCRCDefined: Integer ;
    UnPackCRC: UInt32;
    NumUnPackStreams: UInt32;
  end;

  CArchiveFileTime = packed record
    Low:UInt32;
    High:UInt32;
  end;


  PCFileItem = ^CFileItem;
  CFileItem = packed record
    LastWriteTime: CArchiveFileTime;
  {
  CFileSize StartPos;
  UInt32 Attributes;
  }
    Size: CFileSize;
    FileCRC: UInt32;
    Name: PChar;

    IsFileCRCDefined: Byte;
    HasStream: Byte;
    IsDirectory: Byte;
    IsAnti: Byte;
    IsLastWriteTimeDefined: Byte;
  {
  int AreAttributesDefined;
  int IsLastWriteTimeDefined;
  int IsStartPosDefined;
  }
  end;

  CArchiveDatabase = packed record
    NumPackStreams: UInt32;
    PackSizes: PCFileSize;
    PackCRCsDefined: PByte;
    PackCRCs: PUInt32;
    NumFolders: PUInt32;
    Folders: PCFolder;
    NumFiles:UInt32;
    Files: PCFileItem;
  end;

  CInArchiveInfo = packed record
    StartPositionAfterHeader: CFileSize;
    DataStartPosition: CFileSize;
  end;

  PCArchiveDatabaseEx= ^CArchiveDatabaseEx;
  CArchiveDatabaseEx = packed record
    Database: CArchiveDatabase;
    ArchiveInfo: CInArchiveInfo;
    FolderStartPackStreamIndex: PUInt32;
    PackStreamStartPositions: PCFileSize;
    FolderStartFileIndex: PUInt32;
    FileIndexToFolderIndexMap: PUInt32;
  end;

  CSzData = packed record
    Data: PByte;
    Size: UInt32;
  end;


type SZ_RESULT = Integer;

const k7zMajorVersion = 0;
      k7zStartHeaderSize = $20;

function SzArchiveOpen2: SZ_RESULT;

implementation


function SzArchiveOpen2: SZ_RESULT;
var db: CArchiveDatabaseEx;
    InStream: PStream;

  signature: array [0..5] of Char;
  version: Byte;
  crcFromArchive: UInt32;
  nextHeaderOffset: UInt64;
  nextHeaderSize: UInt64;
  nextHeaderCRC: UInt32;
  crc: UInt32;
  pos: CFileSize;
  buffer: CSzByteBuffer;
  sd: CSzData;
  _type: UInt64;

begin
    crc:= 0; pos:= 0;

    InStream:= NewReadFileStream('D:\Work\Main\MDVReader\CE\_Books\_Books.7z');

    if InStream.Read(signature, 6) <> 6 then Exit;
    if signature <> kSignature then Exit;

    {
    db.Clear();
    db.ArchiveInfo.StartPosition = _arhiveBeginStreamPosition;
    }

    if InStream.Read(version, 1) <> 1 then Exit;
    if version <> k7zMajorVersion then Exit;
    if InStream.Read(version, 1) <> 1 then Exit;


    if InStream.Read(crcFromArchive, SizeOf(UInt32)) <> SizeOf(UInt32) then Exit; //RINOK(SafeReadDirectUInt32(inStream, &crcFromArchive, &crc));
//    crc:= CRC_INIT_VAL;

    if InStream.Read(nextHeaderOffset, SizeOf(UInt64)) <> SizeOf(UInt64) then Exit; //RINOK(SafeReadDirectUInt64(inStream, &nextHeaderOffset, &crc));
    if InStream.Read(nextHeaderSize, SizeOf(UInt64)) <> SizeOf(UInt64) then Exit; //RINOK(SafeReadDirectUInt64(inStream, &nextHeaderSize, &crc));
    if InStream.Read(nextHeaderCRC, SizeOf(UInt32)) <> SizeOf(UInt32) then Exit; //RINOK(SafeReadDirectUInt32(inStream, &nextHeaderCRC, &crc));

    pos:= k7zStartHeaderSize;
    db.ArchiveInfo.StartPositionAfterHeader:= pos;

    //if (CRC_GET_DIGEST(crc) != crcFromArchive) return SZE_ARCHIVE_ERROR;

    if nextHeaderSize = 0 then begin Result:= 0; Exit; end;

    InStream.Seek(pos + nextHeaderOffset, spBegin);

    buffer.Capacity:= nextHeaderSize;
    buffer.Items:= GetMemory(nextHeaderSize);


    if InStream.Read(buffer.Items, nextHeaderSize) <> nextHeaderSize then Exit; //Result:= SafeReadDirect(inStream, buffer.Items, (size_t)nextHeaderSize);
(*
//    if (CrcCalc(buffer.Items, (UInt32)nextHeaderSize) == nextHeaderCRC) then begin

      while true do begin

        sd.Data:= buffer.Items;
        sd.Size:= buffer.Capacity;
        Result:= SzReadID(&sd, &type);
        if (res != SZ_OK)
          break;
        if (type == k7zIdHeader)
        {
          res = SzReadHeader(&sd, db, allocMain, allocTemp);
          break;
        }
        if (type != k7zIdEncodedHeader)
        {
          res = SZE_ARCHIVE_ERROR;
          break;
        }
        {
          CSzByteBuffer outBuffer;
          res = SzReadAndDecodePackedStreams(inStream, &sd, &outBuffer,
              db->ArchiveInfo.StartPositionAfterHeader,
              allocTemp);
          if (res != SZ_OK)
          {
            SzByteBufferFree(&outBuffer, allocTemp->Free);
            break;
          }
          SzByteBufferFree(&buffer, allocTemp->Free);
          buffer.Items = outBuffer.Items;
          buffer.Capacity = outBuffer.Capacity;
        }
      end;
//    end;

  SzByteBufferFree(&buffer, allocTemp->Free);
  return res;
*)
end;

(*

7z Format description (2.30 Beta 25)
-----------------------------------

This file contains description of 7z archive format. 
7z archive can contain files compressed with any method.
See "Methods.txt" for description for defined compressing methods.


Format structure Overview
-------------------------

Some fields can be optional.

Archive structure
~~~~~~~~~~~~~~~~~  
SignatureHeader
[PackedStreams]
[PackedStreamsForHeaders]
[
  Header 
  or 
  {
    Packed Header
    HeaderInfo
  }
]



Header structure
~~~~~~~~~~~~~~~~  
{
  ArchiveProperties
  AdditionalStreams
  {
    PackInfo
    {
      PackPos
      NumPackStreams
      Sizes[NumPackStreams]
      CRCs[NumPackStreams]
    }
    CodersInfo
    {
      NumFolders
      Folders[NumFolders]
      {
        NumCoders
        CodersInfo[NumCoders]
        {
          ID
          NumInStreams;
          NumOutStreams;
          PropertiesSize
          Properties[PropertiesSize]
        }
        NumBindPairs
        BindPairsInfo[NumBindPairs]
        {
          InIndex;
          OutIndex;
        }
        PackedIndices
      }
      UnPackSize[Folders][Folders.NumOutstreams]
      CRCs[NumFolders]
    }
    SubStreamsInfo
    {
      NumUnPackStreamsInFolders[NumFolders];
      UnPackSizes[]
      CRCs[]
    }
  }
  MainStreamsInfo
  {
    (Same as in AdditionalStreams)
  }
  FilesInfo
  {
    NumFiles
    Properties[]
    {
      ID
      Size
      Data
    }
  }
}

HeaderInfo structure
~~~~~~~~~~~~~~~~~~~~
{
  (Same as in AdditionalStreams)
}



Notes about Notation and encoding
---------------------------------

7z uses little endian encoding.

7z archive format has optional headers that are marked as
[]
Header
[]

REAL_UINT64 means real UINT64.

UINT64 means real UINT64 encoded with the following scheme:

  Size of encoding sequence depends from first byte:
  First_Byte  Extra_Bytes        Value
  (binary)
  0xxxxxxx               : ( xxxxxxx           )
  10xxxxxx    BYTE y[1]  : (  xxxxxx << (8 * 1)) + y
  110xxxxx    BYTE y[2]  : (   xxxxx << (8 * 2)) + y
  ...
  1111110x    BYTE y[6]  : (       x << (8 * 6)) + y
  11111110    BYTE y[7]  :                         y
  11111111    BYTE y[8]  :                         y




...........................


ArchiveProperties
~~~~~~~~~~~~~~~~~
BYTE NID::kArchiveProperties (0x02)
while(true)
{
  BYTE PropertyType;
  if (aType == 0)
    break;
  UINT64 PropertySize;
  BYTE PropertyData[PropertySize];
}


Digests (NumStreams)
~~~~~~~~~~~~~~~~~~~~~
  BYTE AllAreDefined
  if (AllAreDefined == 0)
  {
    for(NumStreams)
      BIT Defined
  }
  UINT32 CRCs[NumDefined]


PackInfo
~~~~~~~~~~~~
  BYTE NID::kPackInfo  (0x06)
  UINT64 PackPos
  UINT64 NumPackStreams

  []
  BYTE NID::kSize    (0x09)
  UINT64 PackSizes[NumPackStreams]
  []

  []
  BYTE NID::kCRC      (0x0A)
  PackStreamDigests[NumPackStreams]
  []

  BYTE NID::kEnd


Folder
~~~~~~
  UINT64 NumCoders;
  for (NumCoders)
  {
    BYTE 
    {
      0:3 DecompressionMethod.IDSize
      4:
        0 - IsSimple
        1 - Is not simple
      5:
        0 - No Attributes
        1 - There Are Attributes
      7:
        0 - Last Method in Alternative_Method_List
        1 - There are more alternative methods
    } 
    BYTE DecompressionMethod.ID[DecompressionMethod.IDSize]
    if (!IsSimple)
    {
      UINT64 NumInStreams;
      UINT64 NumOutStreams;
    }
    if (DecompressionMethod[0] != 0)
    {
      UINT64 PropertiesSize
      BYTE Properties[PropertiesSize]
    }
  }
    
  NumBindPairs = NumOutStreamsTotal - 1;

  for (NumBindPairs)
  {
    UINT64 InIndex;
    UINT64 OutIndex;
  }

  NumPackedStreams = NumInStreamsTotal - NumBindPairs;
  if (NumPackedStreams > 1)
    for(NumPackedStreams)
    {
      UINT64 Index;
    };




Coders Info
~~~~~~~~~~~

  BYTE NID::kUnPackInfo  (0x07)


  BYTE NID::kFolder  (0x0B)
  UINT64 NumFolders
  BYTE External
  switch(External)
  {
    case 0:
      Folders[NumFolders]
    case 1:
      UINT64 DataStreamIndex
  }


  BYTE ID::kCodersUnPackSize  (0x0C)
  for(Folders)
    for(Folder.NumOutStreams)
     UINT64 UnPackSize;


  []
  BYTE NID::kCRC   (0x0A)
  UnPackDigests[NumFolders]
  []

  

  BYTE NID::kEnd



SubStreams Info
~~~~~~~~~~~~~~
  BYTE NID::kSubStreamsInfo; (0x08)

  []
  BYTE NID::kNumUnPackStream; (0x0D)
  UINT64 NumUnPackStreamsInFolders[NumFolders];
  []


  []
  BYTE NID::kSize  (0x09)
  UINT64 UnPackSizes[]
  []


  []
  BYTE NID::kCRC  (0x0A)
  Digests[Number of streams with unknown CRC]
  []

  
  BYTE NID::kEnd


Streams Info
~~~~~~~~~~~~

  []
  PackInfo
  []


  []
  CodersInfo
  []


  []
  SubStreamsInfo
  []

  BYTE NID::kEnd


FilesInfo
~~~~~~~~~
  BYTE NID::kFilesInfo;  (0x05)
  UINT64 NumFiles

  while(true)
  {
    BYTE PropertyType;
    if (aType == 0)
      break;

    UINT64 Size;

    switch(PropertyType)
    {
      kEmptyStream:   (0x0E)
        for(NumFiles)
          BIT IsEmptyStream

      kEmptyFile:     (0x0F)
        for(EmptyStreams)
          BIT IsEmptyFile

      kAnti:          (0x10)
        for(EmptyStreams)
          BIT IsAntiFile
      
      case kCreationTime:   (0x12)
      case kLastAccessTime: (0x13)
      case kLastWriteTime:  (0x14)
        BYTE AllAreDefined
        if (AllAreDefined == 0)
        {
          for(NumFiles)
            BIT TimeDefined
        }
        BYTE External;
        if(External != 0)
          UINT64 DataIndex
        []
        for(Definded Items)
          UINT32 Time
        []
      
      kNames:     (0x11)
        BYTE External;
        if(External != 0)
          UINT64 DataIndex
        []
        for(Files)
        {
          wchar_t Names[NameSize];
          wchar_t 0;
        }
        []

      kAttributes:  (0x15)
        BYTE AllAreDefined
        if (AllAreDefined == 0)
        {
          for(NumFiles)
            BIT AttributesAreDefined
        }
        BYTE External;
        if(External != 0)
          UINT64 DataIndex
        []
        for(Definded Attributes)
          UINT32 Attributes
        []
    }
  }


Header
~~~~~~
  BYTE NID::kHeader (0x01)

  []
  ArchiveProperties
  []

  []
  BYTE NID::kAdditionalStreamsInfo; (0x03)
  StreamsInfo
  []

  []
  BYTE NID::kMainStreamsInfo;    (0x04)
  StreamsInfo
  []

  []
  FilesInfo
  []

  BYTE NID::kEnd


HeaderInfo
~~~~~~~~~~
  []
  BYTE NID::kEncodedHeader; (0x17)
  StreamsInfo for Encoded Header
  []


---
End of document


*)


end.
