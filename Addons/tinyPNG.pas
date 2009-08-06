unit tinyPNG;

//  file: tinyPNG.pas
//  file version: 0.35
//  last modified: 26.08.05
//  package: GRushControls
//  autor: Karpinskyj Alexandr aka homm
//      mailto: homm86@mail.ru
//      My humble Web-Page: http://www.homm86.narod.ru

        //  Switch this define on to get most functionality and error-free
        //  versilon of this decoder. All folowing defination are ingoring.
{$DEFINE MOSTCOMPATIBILITY}

        //  Capability loading gray not palettized images with 1,2,4,8 bit per
        //  pixel and with 16 bit per pixel if HIGHPRECISION also defined
{$DEFINE csG}

        //  Capability loading TrueColor RGB images with 24 bit per pixel and
        //  with 48 bit per pixel if HIGHPRECISION also defined.
{$DEFINE csRGB}

        //  Capability loading palettized images with 1,2,4,8 bit per pixel.
        //  HIGHPRECISION no matter.
{$DEFINE csIndexed}

        // * Capability loading gray not palettized images with alpha chanel
        //  with 16 bit per pixel and with 32 bit per pixel if HIGHPRECISION
        //  also defined.
{$DEFINE csGA}

        // * Capability loading TrueColor RGB images with alpha chanel with 32
        //  bit per pixel and with 64 bit per pixel if HIGHPRECISION also
        //  defined.
{$DEFINE csRGBA}

//  Remarks
//  -------
//  * - Alpha chalnel don't loading anyway. This swithes allow loading RGB and
//      Gray chanels from image with alpha chanel.


        //  Undefine to improve decoding speed, and make decoder else smaller.
        //  Files with correct heared and segment sequence in most cases have
        //  right CRC.
//{$DEFINE CHECKCRCS}

        //  Capability loading images with 16 bit per sample. I actually have
        //  not images with 16 bit per sample and even Photoshop 7 can not
        //  create this images, so this capacity not tested, and in most cases
        //  can be undefined.
{$DEFINE HIGHPRECISION}

        //  Capability loading images with interlaced data
        //  still NOT supported
{$DEFINE INTERLACE}

        //  Avoid all checks, including checks on unsupported formats, not only
        //  on file damage. CRC check also avoiding. This switch must be used
        //  only if application sure what source image is not contein errors.
        //  It is strongly recommended using this switch only for loading images
        //  from resources and private application data storage.
//{$DEFINE USEHACKS}

        //  Filters of internal PNG format. Do not crop filters except cases
        //  described above. Images, which have unrecognized filter interpritate
        //  as error, except case with USEHACKS. Look at folowing rules.
        //  - Defination ALLOWFILTERS not means defination all filters.
        //  - Defination of any filter not means defination ALLOWFILTERS.
        //  - Undefination ALLOWFILTERS means undefination ALL filters, but not
        //      means avoid filter check, if USEHACKS not says otherwise.
        //  (complex preprocessing in code make this rules posible)
{$DEFINE ALLOWFILTERS}
    {$DEFINE FILTER_SUB}
    {$DEFINE FILTER_UP}
    {$DEFINE FILTER_AVERANGE}
    {$DEFINE FILTER_PAETH}


//  function tinyLoadPNG(var TargetBitmap: PBitMap; FileImage: PStream): DWORD;
//******************************************************************************
//  The only public proceedure in this module, exists on this time. FileImage
//  MUST BE VALID strem on PNG-formatted content with headers. If loading is
//  imposible, the nil is returned by TargetBitmap and Error Code returning by
//  functin. If {$DEFINE USEHACKS} is defined, tinyERROR_OK always returning.
//  Return value not allways contain valid decoding error, but contain the most
//  probable error.
//******************************************************************************























interface

uses windows, KOL, MZLib;

{$IFDEF MOSTCOMPATIBILITY}
    {$DEFINE csG}
    {$DEFINE csRGB}
    {$DEFINE csIndexed}
    {$DEFINE csGA}
    {$DEFINE csRGBA}
    {$DEFINE CHECKCRCS}
    {$DEFINE HIGHPRECISION}
    {$DEFINE INTERLACE}
    {$UNDEF USEHACKS}
    {$DEFINE ALLOWFILTERS}
    {$DEFINE FILTER_SUB}
    {$DEFINE FILTER_UP}
    {$DEFINE FILTER_AVERANGE}
    {$DEFINE FILTER_PAETH}
{$ENDIF MOSTCOMPATIBILITY}

{$IFDEF USEHACKS}
    {$IFDEF CHECKCRCS}
        {$UNDEF CHECKCRCS}
    {$ENDIF CHECKCRCS}
{$ENDIF USEHACKS}

{$IFNDEF ALLOWFILTERS}
    {$UNDEF FILTER_SUB}
    {$UNDEF FILTER_UP}
    {$UNDEF FILTER_AVERANGE}
    {$UNDEF FILTER_PAETH}
{$ELSE ALLOWFILTERS}
    {$IFNDEF FILTER_SUB}
        {$IFNDEF FILTER_UP}
            {$IFNDEF FILTER_AVERANGE}
                {$IFNDEF FILTER_PAETH}
                    {$UNDEF ALLOWFILTERS}
                {$ENDIF FILTER_PAETH}
            {$ENDIF FILTER_AVERANGE}
        {$ENDIF FILTER_UP}
    {$ENDIF FILTER_SUB}
{$ENDIF ALLOWFILTERS}

{$IFDEF csG}
    {$DEFINE INCGRAY}
{$ENDIF csG}
{$IFDEF csGA}
    {$DEFINE INCGRAY}
{$ENDIF csGA}

const   tinyERROR_OK =                                  $0000;
        tinyERROR_NotPNGFile =                          $0001;
        tinyERROR_UnexpectedEOF =                       $0002;
        tinyERROR_UnexpectedEOF_or_WrongCRC =           $0003;
        tinyERROR_WrongHeader =                         $0004;
        tinyERROR_UnsupportedImageFormat =              $0005;
        tinyERROR_NotIncludedFormat =                   $0006;
        tinyERROR_WrongSegmentsSequence =               $0007;
        tinyERROR_UnsupportedColorFormat =              $0008;
        tinyERROR_UncompressorInitError =               $0009;
        tinyERROR_UnexpectedEOF_or_DataStreamError =    $000A;
        tinyERROR_UnsupportedDataFilter =               $000B;  


function tinyLoadPNG(var TargetBitmap: PBitMap; FileImage: PStream): DWORD;
function tinyLoadPNGResource(var TargetBitmap: PBitMap; Inst : HInst; ResName : PChar; ResType : PChar): DWORD;

implementation

const
    PNGMagic: array[0..7] of Byte = (137,80,78,71,13,10,26,10);

    IHDR = 'IHDR'; 
    IDAT = 'IDAT'; 
    IEND = 'IEND'; 
    PLTE = 'PLTE';
    gAMA = 'gAMA';

type
    TByteArray = array of byte;
    TDWORDArray = array of DWORD;

    TcuFlags = set of (
        cu_RawBuffer,
        cu_sFile,
        cu_BitMap,
        cu_BITMAPINFO,
        cu_RowBuffer,
        cu_Inflate,
        cu_InterlaceBuffer
    );

    TImageProperties = record          
        Width: cardinal;
        Height: cardinal;
        DIBBits: Pointer;
        Interlaced: boolean;
        HasAlpha: boolean;
        HasGamma: boolean;
        HasPallete: boolean;
    end;

    TChunkType = array[0..3] of Char;

    TPNGChunkHeader = packed record
        Length: cardinal;
        ChunkType: TChunkType;
    end;

    PIHDRChunk = ^TIHDRChunk;
    TIHDRChunk = packed record
        Width: cardinal;
        Height: cardinal;
        BitDepth: byte;
        ColorType: byte;
        Compression: byte;
        Filter: byte;
        Interlaced: byte;
    end;

    TConversionMethod = procedure(Source: pointer; Target: pointer; Count: cardinal);

    TColorManager = packed record
        RowConversion: TConversionMethod;
        SourceBitsPerSample: byte;
        TargetBitsPerSample: byte;
        SourceBytesPerPixel: byte;
        TargetBytesPerPixel: byte;
        SourceBytesPerRow: DWORD;
        TargetBytesPerRow: DWORD;
        FilteredBytes: DWORD;
        {$IFDEF INTERLACE}
        SourceSample: byte;
        Mask: byte;
        {$ENDIF INTERLACE}
    end;

//****************************************************************
//****************************************************************
var     CM: TColorManager;
        IP: TImageProperties;
        Header: TPNGChunkHeader;
        Description: TIHDRChunk;
        InflateStream: TZState;

        RowBuffer: array[Boolean] of PChar;
        RawBuffer: Pointer;
        {$IFDEF INTERLACE}
        InterlaceBuffer: Pointer;
        {$ENDIF INTERLACE}
        BITMAPINFO: PBITMAPINFO;
        sFile: PStream;
        sFile_Size: DWORD;
        sFile_Position: DWORD;
        Bitmap: THandle;
        {$IFDEF CHECKCRCS}
        CurrentCRC: cardinal;
        {$ENDIF CHECKCRCS}
        cuFlags: TcuFlags;
        CurrentSource: pointer;
        IDATSize: Integer;


const   RowStart: array[0..6] of DWORD = (0,0,4,0,2,0,1);
        ColumnStart: array[0..6] of DWORD = (0,4,0,2,0,1,0);
        RowIncrement: array[0..6] of DWORD = (8,8,8,4,4,2,2);
        ColumnIncrement: array[0..6] of DWORD = (8,8,4,4,2,2,1);
        PassMask: array[0..6] of byte = ($80,$08,$88,$22,$AA,$55,$FF);

//****************************************************************
procedure  ConvertS0D16(Source: pointer; Target: pointer; Count: cardinal);
asm
    push ebx
@@01:
    mov bl, byte ptr [eax]
    mov byte ptr [edx], bl
    add eax, $02
    inc edx
    dec ecx
    jnz @@01
    pop ebx
end;
//****************************************************************
procedure  ConvertS2D8(Source: pointer; Target: pointer; Count: cardinal);
asm
    push ebx
    dec ecx
@@01:
    mov ebx, dword ptr [eax]
    bswap ebx
    shr ebx, 8
    mov dword ptr [edx], ebx
    add eax, $03
    add edx, $03
    dec ecx
    jnz @@01
    mov ebx, [eax -1]
    bswap ebx
    mov word ptr [edx], bx
    shr ebx, 16
    mov byte ptr [edx+2], bl
    pop ebx
end;
//****************************************************************
procedure  ConvertS2D16(Source: pointer; Target: pointer; Count: cardinal);
asm
    push ebx
    push edi
    mov edi, ecx
@@01:
    mov bl, [eax]
    mov bh, [eax+2]
    mov cl, [eax+4]
    mov [edx+2], bl
    mov [edx+1], bh
    mov [edx], cl
    add eax, $06
    add edx, $03
    dec edi
    jnz @@01
    pop edi
    pop ebx
end;
//****************************************************************
procedure  ConvertS3D1(Source: pointer; Target: pointer; Count: cardinal);
asm
    push ebx
    push edi
    xor edi, edi
@@01:
    and edi, $07
    jnz @@02
    mov bl, [eax]
    inc eax
@@02:
    xor bh, bh
    shl bx, 1
    mov [edx], bh
    inc edx
    inc edi
    dec ecx
    jnz @@01
    pop edi
    pop ebx
end;
//****************************************************************
procedure  ConvertS3D2(Source: pointer; Target: pointer; Count: cardinal);
asm
    push ebx
    push edi
    xor edi, edi
@@01:
    and edi, $03
    jnz @@02
    mov bl, [eax]
    inc eax
@@02:
    xor bh, bh
    shl bx, 2
    mov [edx], bh
    inc edx
    inc edi
    dec ecx
    jnz @@01
    pop edi
    pop ebx
end;
//****************************************************************
procedure  ConvertS3D4(Source: pointer; Target: pointer; Count: cardinal);
asm
    push ebx
    push edi
    xor edi, edi
@@01:
    and edi, $01
    jnz @@02
    mov bl, [eax]
    inc eax
@@02:
    xor bh, bh
    shl bx, 4
    mov [edx], bh
    inc edx
    inc edi
    dec ecx
    jnz @@01
    pop edi
    pop ebx
end;
//****************************************************************
procedure  ConvertS4D8(Source: pointer; Target: pointer; Count: cardinal);
asm
    push ebx
@@01:
    mov bl, byte ptr [eax]
    mov byte ptr [edx], bl
    add eax, $02
    inc edx
    dec ecx
    jnz @@01
    pop ebx
end;
//****************************************************************
procedure  ConvertS4D16(Source: pointer; Target: pointer; Count: cardinal);
asm
    push ebx
@@01:
    mov bl, byte ptr [eax]
    mov byte ptr [edx], bl
    add eax, $04
    inc edx
    dec ecx
    jnz @@01
    pop ebx
end;
//****************************************************************
procedure  ConvertS6D8(Source: pointer; Target: pointer; Count: cardinal);
asm
    push ebx
    dec ecx
@@01:
    mov ebx, dword ptr [eax]
    bswap ebx
    shr ebx, 8
    mov dword ptr [edx], ebx
    add eax, $04
    add edx, $03
    dec ecx
    jnz @@01
    mov ebx, dword ptr [eax]
    bswap ebx
    shr ebx, 8
    mov word ptr [edx], bx
    shr ebx, 8
    mov byte ptr [edx+2], bh
    pop ebx
end;
//****************************************************************
procedure  ConvertS6D16(Source: pointer; Target: pointer; Count: cardinal);
asm
    push ebx
    push edi
    mov edi, ecx
@@01:
    mov bl, [eax]
    mov bh, [eax+2]
    mov cl, [eax+4]
    mov [edx+2], bl
    mov [edx+1], bh
    mov [edx], cl
    add eax, $08
    add edx, $03
    dec edi
    jnz @@01

    pop edi
    pop ebx
end;
//****************************************************************
{$IFDEF ALLOWFILTERS}
procedure Filter_SUB(Applyto: Pointer);
asm
    push ebx
    xor edx, edx
    mov dl, [CM.SourceBytesPerPixel]
    mov ecx, [CM.FilteredBytes]
    sub ecx, edx
    mov ebx, eax
    add eax, edx
@@01:
    mov dl, [eax]
    add dl, [ebx]
    mov [eax], dl
    inc eax
    inc ebx
    dec ecx
    jnz @@01
    pop ebx
end;
//****************************************************************
procedure Filter_UP(Applyto: Pointer; PrevRow: pointer);
asm
    push ebx
    mov ecx, [CM.FilteredBytes]
@@01:
    mov bl, [eax]
    add bl, [edx]
    mov [eax], bl
    inc eax
    inc edx
    dec ecx
    jnz @@01
    pop ebx
end;
//****************************************************************
procedure Filter_AVERAGE(Applyto: Pointer; PrevRow: pointer);
asm
    push edi
    push esi
    mov edi, edx
    mov esi, eax
    xor edx, edx
    mov dl, [CM.SourceBytesPerPixel]
@@02:
    mov cl, [edi]
    shr cl, 1
    mov ch, [eax]
    add cl, ch
    mov [eax], cl
    inc edi
    inc eax
    dec edx
    jnz @@02
    mov ecx, [CM.FilteredBytes]
    xor edx, edx
    mov dl, [CM.SourceBytesPerPixel]
    sub ecx, edx
@@01:
    mov dl, [edi]
    mov dh, [esi]
    add dl, dh
    rcr dl, 1
    mov dh, [eax]
    add dh,dl
    mov [eax], dh
    inc eax
    inc esi
    inc edi
    dec ecx
    jnz @@01
    pop esi
    pop edi
end;
//****************************************************************
function PaethPredictor(A,B,C: byte): byte;
var     PA,PB,PC: smallint;
begin
    PA:=Abs(B-C);
    PB:=Abs(A-C);
    PC:=Abs(A-C+B-C);
    if (PA<=PB) and (PA<=PC) then Result:=A
    else if (PB<=PC) then Result:=B else Result:=C;
end;
//****************************************************************
procedure Filter_PAETH(Applyto: Pointer; PrevRow: pointer);
asm
    push ebx
    push edi
    push esi
    push ebp

    mov esi, edx
    mov edi, eax
    xor ebx, ebx
    mov bl, [CM.SourceBytesPerPixel]
@@02:
    mov al, [esi + ebx - 1]
    mov ah, [edi + ebx - 1]
    add al, ah
    mov [edi + ebx - 1], al
    dec ebx
    jnz @@02
    mov bl, [CM.SourceBytesPerPixel]
    mov ebp, [CM.FilteredBytes]
    sub ebp, ebx
@@01:
    mov al, [edi]
    mov dl, [esi + ebx]
    mov cl, [esi]
    call PaethPredictor
    mov ah, [edi + ebx]
    add al, ah
    mov [edi + ebx], al
    inc esi
    inc edi
    dec ebp
    jnz @@01

    pop ebp
    pop esi
    pop edi
    pop ebx
end;
{$ENDIF ALLOWFILTERS}
//****************************************************************
{$IFDEF INTERLACE}
procedure Expand(Src: Pointer; Dst: Pointer; DestCount: dword);
asm
    push ebx
    push edi
    push esi
    mov edi, edx
    mov esi, eax
    mov bl, byte ptr [CM.Mask]
    xor edx, edx
    mov dl, byte ptr [CM.TargetBytesPerPixel]
@@01:
    rol bl, 1
    jnb @2
    mov bh, dl
@@02:
    mov al, byte ptr [esi]
    mov byte ptr [edi], al
    inc esi
    inc edi
    dec bh
    jnz @@02
    jmp @1
@2:
    add edi, edx
@1:
    dec ecx
    jnz @@01
    pop esi
    pop edi
    pop ebx
end;
{$ENDIF INTERLACE}
//****************************************************************
procedure CleanUp;
    procedure FreeMem_and_NULL (var ptr: pointer);
    begin
        FreeMem(ptr);
        ptr := nil;
    end;
begin
    if cu_RawBuffer in cuFlags then
        FreeMem_and_NULL(RawBuffer);
    if cu_sFile in cuFlags then
        sFile.Position := sFile_Position;
    if cu_BITMAPINFO in cuFlags then
        FreeMem_and_NULL(Pointer(BITMAPINFO));
    if cu_Inflate in cuFlags then
        InflateEnd(InflateStream);
    if cu_RowBuffer in cuFlags then begin
        FreeMem_and_NULL(pointer(RowBuffer[false]));
        FreeMem_and_NULL(pointer(RowBuffer[true]));
    end;
    if cu_BitMap in cuFlags then begin
        DeleteObject(Bitmap);
        Bitmap := 0;
        IP.DIBBits := nil;
    end;
    {$IFDEF INTERLACE}
    if cu_InterlaceBuffer in cuFlags then
        FreeMem_and_NULL(InterlaceBuffer);
    {$ENDIF INTERLACE}
end;
//****************************************************************
function IsChunk(ChunkType: TChunkType): boolean;
const   Mask = not $20202020;
begin
    Result:=(Cardinal(Header.ChunkType) and Mask)=(Cardinal(ChunkType) and Mask);
end;
//****************************************************************
{$IFNDEF USEHACKS}
function ReadDataAndCheckCRC: Boolean;
{$ELSE USEHACKS}
procedure ReadDataAndCheckCRC;
{$ENDIF USEHACKS}
var     FileCRC: cardinal;
begin
    ReallocMem(RawBuffer, Header.Length);
    {$IFNDEF USEHACKS}
    Result := false;
    if (sFile_Size - sFile.Position) >= (Header.Length + sizeof(FileCRC)) then begin
    {$ENDIF USEHACKS}
        sFile.Read(RawBuffer^, Header.Length);
        sFile.Read(FileCRC, sizeof(FileCRC));
    {$IFNDEF USEHACKS}
    end else exit;
    {$ENDIF USEHACKS}
    asm
        mov eax, [FileCRC]
        bswap eax
        mov [FileCRC], eax
    end;
    {$IFDEF CHECKCRCS}
    CurrentCRC := CRC32(CurrentCRC, RawBuffer, Header.Length);
    if CurrentCRC <> FileCRC then
        exit;
    {$ENDIF CHECKCRCS}
    {$IFNDEF USEHACKS}
    result := true;
    {$ENDIF}
end;
//****************************************************************
{$IFNDEF USEHACKS}
function LoadAndSwapHeader: Boolean;
{$ELSE USEHACKS}
procedure LoadAndSwapHeader;
{$ENDIF USEHACKS}
begin
    {$IFNDEF USEHACKS}
    result := false;
    if (sFile_Size - sFile.Position) >= sizeof(Header) then
    {$ENDIF}
        sFile.Read(Header, sizeof(Header))
    {$IFNDEF USEHACKS}
    else exit
    {$ENDIF};
    {$IFDEF CHECKCRCS}
    CurrentCRC:=CRC32(0, @Header.ChunkType, 4);
    {$ENDIF CHECKCRCS}
    asm
        mov eax, [Header.Length]
        bswap eax
        mov [Header.Length], eax
    end;
    {$IFNDEF USEHACKS}
    result := true;
    {$ENDIF}
end;
//****************************************************************
{$IFNDEF USEHACKS}
function SetupColorDepthAndMakeBitmap: Boolean;
{$ELSE USEHACKS}
procedure SetupColorDepthAndMakeBitmap;
{$ENDIF USEHACKS}
begin
    {$IFNDEF USEHACKS}
    result := false;
    {$ENDIF}
    if BITMAPINFO = nil then begin
        BITMAPINFO := AllocMem(sizeof(TBITMAPINFO));
        Include(cuFlags, cu_BITMAPINFO);
    end;
    with CM do begin
        SourceBitsPerSample := Description.BitDepth;
        TargetBitsPerSample := 8;
        case Description.ColorType of
        {$IFDEF csG}
            0: begin
                case SourceBitsPerSample of
                    1:  RowConversion := ConvertS3D1;
                    2:  RowConversion := ConvertS3D2;
                    4:  RowConversion := ConvertS3D4;
                    8:  RowConversion := @Move;
                    {$IFDEF HIGHPRECISION}
                    16: RowConversion := ConvertS0D16;
                    {$ENDIF HIGHPRECISION}
                {$IFNDEF USEHACKS}
                else exit;
                {$ENDIF USEHACKS}
                end;
                TargetBytesPerPixel := 1;
                {$IFDEF INTERLACE}
                SourceSample := 1;
                {$ENDIF INTERLACE}
                {$IFDEF HIGHPRECISION}
                if SourceBitsPerSample = 16 then
                    SourceBytesPerPixel := 2
                else
                {$ENDIF HIGHPRECISION}
                    SourceBytesPerPixel := 8 div SourceBitsPerSample;
                BITMAPINFO.bmiHeader.biBitCount := 8;
            end;
        {$ENDIF csG}
        {$IFDEF csRGB}
            2: begin
                case SourceBitsPerSample of
                    8:  begin
                            RowConversion := ConvertS2D8;
                            SourceBytesPerPixel := 3;
                        end;
                    {$IFDEF HIGHPRECISION}
                    16: begin
                            RowConversion := ConvertS2D16;
                            SourceBytesPerPixel := 6;
                        end;
                    {$ENDIF HIGHPRECISION}
                {$IFNDEF USEHACKS}
                else exit;
                {$ENDIF USEHACKS}
                end;
                {$IFDEF INTERLACE}
                SourceSample := 3;
                {$ENDIF INTERLACE}
                TargetBytesPerPixel := 3;
                BITMAPINFO.bmiHeader.biBitCount := 24;
            end;
        {$ENDIF csRGB}
        {$IFDEF csIndexed}
            3: begin
                case SourceBitsPerSample of
                    1:  RowConversion := ConvertS3D1;
                    2:  RowConversion := ConvertS3D2;
                    4:  RowConversion := ConvertS3D4;
                    8:  RowConversion := @Move;
                {$IFNDEF USEHACKS}
                else exit;
                {$ENDIF USEHACKS}
                end;
                {$IFDEF INTERLACE}
                SourceSample := 1;
                {$ENDIF INTERLACE}
                TargetBytesPerPixel := 1;
                SourceBytesPerPixel := 8 div SourceBitsPerSample;
                BITMAPINFO.bmiHeader.biBitCount := 8;
            end;
        {$ENDIF csIndexed}
        {$IFDEF csGA}
            4: begin
                case SourceBitsPerSample of
                    8:  begin
                            RowConversion := ConvertS4D8;
                            SourceBytesPerPixel := 2;
                        end;
                    {$IFDEF HIGHPRECISION}
                    16: begin
                            RowConversion := ConvertS4D16;
                            SourceBytesPerPixel := 4;
                        end;
                    {$ENDIF HIGHPRECISION}
                {$IFNDEF USEHACKS}
                else exit;
                {$ENDIF USEHACKS}
                end;
                {$IFDEF INTERLACE}
                SourceSample := 2;
                {$ENDIF INTERLACE}
                TargetBytesPerPixel := 1;
                BITMAPINFO.bmiHeader.biBitCount := 8;
            end;
        {$ENDIF csGA}
        {$IFDEF csRGBA}
            6: begin
                case SourceBitsPerSample of
                    8:  begin
                            RowConversion := ConvertS6D8;
                            SourceBytesPerPixel := 4;
                        end;
                    {$IFDEF HIGHPRECISION}
                    16: begin
                            RowConversion := ConvertS6D16;
                            SourceBytesPerPixel := 8;
                        end;
                    {$ENDIF HIGHPRECISION}
                {$IFNDEF USEHACKS}
                else exit;
                {$ENDIF USEHACKS}
                end;
                {$IFDEF INTERLACE}
                SourceSample := 4;
                {$ENDIF INTERLACE}
                TargetBytesPerPixel := 3;
                BITMAPINFO.bmiHeader.biBitCount := 24;
            end;
        {$ENDIF csRGBA}
        {$IFNDEF USEHACKS}
        else exit;
        {$ENDIF USEHACKS}
        end;

        // IMPORTANT: If SourceBitsPerSample less then 8 SourceBytesPerPixel
        // means Source Pixels Per Byte, otherwise it means what means
        if SourceBitsPerSample < 8 then begin
            SourceBytesPerRow := (IP.Width+SourceBytesPerPixel - 1) div SourceBytesPerPixel;
            // IMPORTANT: Now switch SourceBytesPerPixel to 1 if SourceBitsPerSample
            // less then 8. Old value not used more.
            SourceBytesPerPixel := 1;
        end else
            SourceBytesPerRow := IP.Width * SourceBytesPerPixel;
        TargetBytesPerRow := ((TargetBytesPerPixel * IP.Width +3) shr 2) shl 2;
    end;
    with BITMAPINFO^ do begin
        bmiHeader.biHeight := IP.Height;
        bmiHeader.biWidth := IP.Width;
        bmiHeader.biPlanes := 1;
        bmiHeader.biSize := sizeof(bmiHeader);
    end;
    Bitmap := CreateDIBSection(0, BITMAPINFO^, DIB_RGB_COLORS, IP.DIBBits, 0, 0);
    {$IFNDEF USEHACKS}
    if Bitmap = 0 then exit;
    {$ENDIF USEHACKS}
    include(cuFlags, cu_Bitmap);
    {$IFNDEF USEHACKS}
    if IP.DIBBits = nil then exit;
    result := true;
    {$ENDIF USEHACKS}
end;
//****************************************************************
{$IFNDEF USEHACKS}
function ReadRow(Buffer: Pointer; bytes: integer): Boolean;
{$ELSE USEHACKS}
procedure ReadRow(Buffer: Pointer; bytes: integer);
{$ENDIF USEHACKS}
var     ZLibResult: Integer;
begin
    {$IFNDEF USEHACKS}
    result := false;
    {$ENDIF USEHACKS}
    InflateStream.NextOutput := Buffer;
    InflateStream.AvailableOutput := Bytes;
    repeat
        if InflateStream.AvailableInput = 0 then begin
            IDATSize:=0;
            while IDATSize=0 do begin
                {$IFNDEF USEHACKS}
                if not IsChunk(IDAT) then Exit;
                if not ReadDataAndCheckCRC then exit;
                {$ELSE USEHACKS}
                ReadDataAndCheckCRC;
                {$ENDIF USEHACKS}
                CurrentSource := RawBuffer;
                IDATSize:=Header.Length;
                {$IFNDEF USEHACKS}
                if not LoadAndSwapHeader then exit;
                {$ELSE USEHACKS}
                LoadAndSwapHeader;
                {$ENDIF USEHACKS}
            end;
        end;
        InflateStream.NextInput := CurrentSource;
        InflateStream.AvailableInput := IDATSize-(Integer(CurrentSource)-Integer(RawBuffer));
        ZLibResult := Inflate(InflateStream, Z_PARTIAL_FLUSH);
        CurrentSource := InflateStream.NextInput;
        if ZLibResult = Z_STREAM_END then begin
            if (InflateStream.AvailableOutput <> 0) or (InflateStream.AvailableInput<>0) then exit;
            Break;
        end;
        if ZLibResult <> Z_OK then exit;
    until InflateStream.AvailableOutput = 0;
    {$IFNDEF USEHACKS}
    result := true;
    {$ENDIF USEHACKS}
end;
//****************************************************************
procedure CreateGrayPalette;
var     Entries: integer;
        val: byte;
        i: integer;
        mult: byte;
begin
    if Description.BitDepth = 16 then
        Entries := 255
    else
        Entries := (1 shl Description.BitDepth)-1;
    BITMAPINFO := AllocMem(sizeof(TRGBQuad)*256 + sizeof(TBITMAPINFO));
    Include(cuFlags, cu_BITMAPINFO);
    mult := (255 div Entries);
    for i := 0 to Entries do begin
        val := mult*i;
        BITMAPINFO.bmiColors[i] := TRGBQuad((val shl 16) or (val shl 8) or val);
    end;
end;
//****************************************************************
procedure LoadPalette;
var     Entries: integer;
begin
    Entries := Header.Length div 3;
    BITMAPINFO := AllocMem(sizeof(TRGBQuad)*256 + sizeof(TBITMAPINFO));
    Include(cuFlags, cu_BITMAPINFO);
    asm
        push eax
        push ecx
        push edi
        push esi
        mov esi, [RawBuffer]
        mov edi, [BITMAPINFO]
        lea edi, TBITMAPINFO(edi).bmiColors
        mov ecx, [Entries]
        dec ecx
        jz @@02
    @@01:
        mov eax, [esi]
        bswap eax
        shr eax, 8
        mov [edi], eax
        add edi, 4
        add esi, 3
        dec ecx
        jnz @@01
    @@02:
        dec esi
        mov eax, [esi]
        bswap eax
        and eax, $00FFFFFF
        mov [edi], eax
        pop esi
        pop edi
        pop ecx
        pop eax
    end;
    IP.HasPallete := true;
end;



//****************************************************************
//****************************************************************
//****************************************************************
//****************************************************************
function tinyLoadPNGResource(var TargetBitmap: PBitMap; Inst : HInst; ResName : PChar; ResType : PChar): DWORD;
var     Stream: PStream;
begin
    Stream := NewMemoryStream;
    Resource2Stream(Stream, Inst, ResName, ResType);
    Stream.Position := 0;
    Result := tinyLoadPNG(TargetBitmap, Stream);
    Stream.Free;
end;
//****************************************************************
function tinyLoadPNG(var TargetBitmap: PBitMap; FileImage: PStream): DWORD;
var     Row: DWORD;
        Magic: array[0..7] of byte;
        EvenRow: boolean;
        {$IFDEF INTERLACE}
        Pass: integer;
        InterlaceRowBytes: integer;
        InterlaceWidth: integer;
        {$ENDIF INTERLACE}
        tempRes: Boolean;

begin
    {$IFNDEF USEHACKS}
    TargetBitmap := nil;
    {$ELSE USEHACKS}
    result := tinyERROR_OK;
    {$ENDIF USEHACKS}
    cuFlags := [];
    sFile := FileImage;
    sFile_Position := sFile.Position;
    sFile_Size := sFile.Size;
    include (cuFlags, cu_sFile);
    {$IFNDEF USEHACKS}
    tempRes := sFile_Size - sFile_Position > (Sizeof(Magic) + sizeof(Header) + sizeof(Description));
    if tempRes then begin
        sFile.Read(Magic,sizeof(Magic));
        tempRes := CompareMem(@Magic, @PNGMagic, 8);
    end;
    if not tempRes then begin
        result := tinyERROR_NotPNGFile;
        CleanUp;
        exit;
    end;
    {$ELSE USEHACKS}
    sFile.Seek (8, spCurrent);
    {$ENDIF USEHACKS};

//begin****Initialization**********
    RawBuffer := nil;
    ZeroMemory(@IP, sizeof(IP));
    ZeroMemory(@CM, sizeof(CM));
    ZeroMemory(@InflateStream, sizeof(InflateStream));
//end******Initialization**********

//begin****First Loop**************
    {$IFNDEF USEHACKS}
    if not LoadAndSwapHeader then begin
        result := tinyERROR_UnexpectedEOF;
        CleanUp;
        exit;
    end;
    {$ELSE USEHACKS}
    LoadAndSwapHeader;
    {$ENDIF USEHACKS}

    {$IFNDEF USEHACKS}
    if IsChunk(IHDR) then begin
    {$ENDIF USEHACKS}
        {$IFNDEF USEHACKS}
        tempRes := ReadDataAndCheckCRC;
        include (cuFlags, cu_RawBuffer);
        if not tempRes then begin
            result := tinyERROR_UnexpectedEOF_or_WrongCRC;
            CleanUp;
            exit;
        end;
        {$ELSE USEHACKS}
        ReadDataAndCheckCRC;
        {$ENDIF USEHACKS}
        {$IFNDEF USEHACKS}
        if Header.Length = sizeof(Description) then
        {$ENDIF USEHACKS}
            Move(RawBuffer^, Description, sizeof(Description))
        {$IFNDEF USEHACKS}
        else begin
            result := tinyERROR_WrongHeader;
            CleanUp;
            exit;
        end;
        if (Description.Width = 0) or (Description.Height = 0)
            or (Description.Filter <> 0) or (Description.Compression <> 0)  then begin
            result := tinyERROR_WrongHeader;
            CleanUp;
            Exit;
        end
        {$ENDIF USEHACKS};
        asm
            mov eax, dword ptr [Description]
            bswap eax
            mov dword ptr [Description], eax
            mov eax, dword ptr [Description+4]
            bswap eax
            mov dword ptr [Description+4], eax
        end;
        with IP do begin
            Width := Description.Width;
            Height := Description.Height;
            HasAlpha := Boolean(Description.ColorType and $04);
            Interlaced := Boolean(Description.Interlaced);
            {$IFNDEF USEHACKS}
            {$IFDEF INTERLACE}
            if Description.Interlaced > 1 then begin
                result := tinyERROR_UnsupportedImageFormat;
                CleanUp;
                exit;
            end;
            {$ELSE INTERLACE}
            if Interlaced then begin
                result := tinyERROR_NotIncludedFormat;
                CleanUp;
                exit;
            end;
            {$ENDIF INTERLACE}
            {$ENDIF USEHACKS}

            while sFile.Position < sFile_size do begin
                {$IFNDEF USEHACKS}
                if not LoadAndSwapHeader then begin
                    result := tinyERROR_UnexpectedEOF;
                    CleanUp;
                    exit;
                end;
                {$ELSE USEHACKS}
                LoadAndSwapHeader;
                {$ENDIF USEHACKS}
//**************
                if IsChunk(PLTE) then begin
                    {$IFNDEF USEHACKS}
                    if HasPallete or (Header.Length mod 3<>0) or (Header.Length > 256*3)
                        or (Description.ColorType in [0, 4]) then begin
                        result := tinyERROR_WrongSegmentsSequence;
                        CleanUp;
                        exit;
                    end;
                    if not ReadDataAndCheckCRC then begin
                        result := tinyERROR_UnexpectedEOF_or_WrongCRC;
                        CleanUp;
                        exit;
                    end;
                    {$ELSE USEHACKS}
                    ReadDataAndCheckCRC;
                    {$ENDIF USEHACKS}
                    LoadPalette;
                    Continue;
                end;
//**************
                if IsChunk(IDAT) then begin
                    {$IFNDEF USEHACKS}
                    if (Description.ColorType = 3) and (not HasPallete) then begin
                        result := tinyERROR_WrongSegmentsSequence;
                        CleanUp;
                        exit;
                    end;
                    {$ENDIF USEHACKS}
                    break;
                end;
//**************
                {$IFNDEF USEHACKS}
                if (sFile_Size - sFile.Position) >= Header.Length + 4 then
                {$ENDIF USEHACKS}
                    sFile.Seek(Header.Length + 4, spCurrent)
                {$IFNDEF USEHACKS}
                else begin
                    result := tinyERROR_UnexpectedEOF;
                    CleanUp;
                    exit;
                end;
                if IsChunk(IEND) then begin
                    result := tinyERROR_WrongSegmentsSequence;
                    CleanUp;
                    exit;
                end
                {$ENDIF USEHACKS};
            end; //while sFile.Position < sFile_size do begin
        end; //with IP do begin
    {$IFNDEF USEHACKS}
    end else begin //if IsChunk(IHDR)
        result := tinyERROR_WrongSegmentsSequence;
        CleanUp;
        exit;
    end;
    {$ENDIF USEHACKS}
//end******First Loop**************

//begin***Data Transfer************
    {$IFDEF INCGRAY}
    if Description.ColorType in [0,4] then
        CreateGrayPalette;
    {$ENDIF INCGRAY}
    {$IFNDEF USEHACKS}
    if not SetupColorDepthAndMakeBitmap then begin
        result := tinyERROR_UnsupportedColorFormat;
        CleanUp;
        exit;
    end;
    {$ELSE USEHACKS}
    SetupColorDepthAndMakeBitmap;
    {$ENDIF USEHACKS}
    with CM do begin
        {$IFNDEF USEHACKS}
        if InflateInit(InflateStream) < 0 then begin
            result := tinyERROR_UncompressorInitError;
            CleanUp;
            exit;
        end;
        {$ELSE USEHACKS}
        InflateInit(InflateStream);
        {$ENDIF USEHACKS}
        include(cuFlags, cu_Inflate);
        RowBuffer[True] := AllocMem(SourceBytesPerRow + 1);
        RowBuffer[False] := AllocMem(SourceBytesPerRow + 1);
        include(cuFlags, cu_RowBuffer);
        EvenRow := True;
        {$IFDEF INTERLACE}
        if IP.Interlaced then begin
            InterlaceBuffer := AllocMem(TargetBytesPerPixel * IP.Width);
            include(cuFlags, cu_InterlaceBuffer);
            for Pass := 0 to 6 do begin
                if IP.Width <= ColumnStart[Pass] then Continue;
                InterlaceWidth := (IP.Width + ColumnIncrement[Pass]-1 - ColumnStart[Pass]) div ColumnIncrement[Pass];
                InterlaceRowBytes := SourceSample * ((InterlaceWidth*SourceBitsPerSample+7) div 8);
                Row := RowStart[Pass];
                Mask := PassMask[Pass];
                ZeroMemory(Pointer(RowBuffer[not EvenRow]), SourceBytesPerRow + 1);
                while Row < IP.Height do begin
                    {$IFNDEF USEHACKS}
                    if not ReadRow(RowBuffer[EvenRow], InterlaceRowBytes+1) then begin
                        result := tinyERROR_UnexpectedEOF_or_DataStreamError;
                        CleanUp;
                        exit;
                    end;
                    {$ELSE USEHACKS}
                    ReadRow(RowBuffer[EvenRow], InterlaceRowBytes+1);
                    {$ENDIF USEHACKS}
                    {$IFDEF ALLOWFILTERS}
                    FilteredBytes := InterlaceRowBytes;
                    case Byte(RowBuffer[EvenRow][0]) of
                        0: ;
                        {$IFDEF FILTER_SUB}
                        1:  Filter_SUB(@RowBuffer[EvenRow][1]);
                        {$ENDIF FILTER_SUB}
                        {$IFDEF FILTER_UP}
                        2:  Filter_UP(@RowBuffer[EvenRow][1], @RowBuffer[not EvenRow][1]);
                        {$ENDIF FILTER_UP}
                        {$IFDEF FILTER_AVERANGE}
                        3:  Filter_AVERAGE(@RowBuffer[EvenRow][1], @RowBuffer[not EvenRow][1]);
                        {$ENDIF FILTER_AVERANGE}
                        {$IFDEF FILTER_PAETH}
                        4:  Filter_PAETH(@RowBuffer[EvenRow][1], @RowBuffer[not EvenRow][1]);
                        {$ENDIF FILTER_PAETH}
                    {$IFNDEF USEHACKS}
                    else
                        result := tinyERROR_UnsupportedDataFilter;
                        CleanUp;
                        exit;
                    {$ENDIF USEHACKS}
                    end;
                    {$ELSE ALLOWFILTERS}
                    {$IFNDEF USEHACKS}
                    if Byte(RowBuffer[EvenRow][0]) <> 0 then begin
                        result := tinyERROR_UnsupportedDataFilter;
                        CleanUp;
                        exit;
                    end;
                    {$ENDIF USEHACKS}
                    {$ENDIF ALLOWFILTERS}
                    RowConversion(Pointer(RowBuffer[EvenRow]+1), InterlaceBuffer, InterlaceWidth);
                    Expand(InterlaceBuffer, Pointer(DWORD(IP.DIBBits) + TargetBytesPerRow*(IP.Height -Row-1)), IP.Width);
                    EvenRow := not EvenRow;
                    Inc(Row,RowIncrement[Pass]);
                end;
            end;
        end else begin
        {$ENDIF INTERLACE}
            for Row := 0 to IP.Height-1 do begin
                {$IFNDEF USEHACKS}
                if not ReadRow(RowBuffer[EvenRow], SourceBytesPerRow + 1) then begin
                    result := tinyERROR_UnexpectedEOF_or_DataStreamError;
                    CleanUp;
                    exit;
                end;
                {$ELSE USEHACKS}
                ReadRow(RowBuffer[EvenRow], SourceBytesPerRow + 1);
                {$ENDIF USEHACKS}
                {$IFDEF ALLOWFILTERS}
                FilteredBytes := SourceBytesPerRow;
                case Byte(RowBuffer[EvenRow][0]) of
                    0: ;
                    {$IFDEF FILTER_SUB}
                    1:  Filter_SUB(@RowBuffer[EvenRow][1]);
                    {$ENDIF FILTER_SUB}
                    {$IFDEF FILTER_UP}
                    2:  Filter_UP(@RowBuffer[EvenRow][1], @RowBuffer[not EvenRow][1]);
                    {$ENDIF FILTER_UP}
                    {$IFDEF FILTER_AVERANGE}
                    3:  Filter_AVERAGE(@RowBuffer[EvenRow][1], @RowBuffer[not EvenRow][1]);
                    {$ENDIF FILTER_AVERANGE}
                    {$IFDEF FILTER_PAETH}
                    4:  Filter_PAETH(@RowBuffer[EvenRow][1], @RowBuffer[not EvenRow][1]);
                    {$ENDIF FILTER_PAETH}
                {$IFNDEF USEHACKS}
                else
                    result := tinyERROR_UnsupportedDataFilter;
                    CleanUp;
                    exit;
                {$ENDIF USEHACKS}
                end;
                {$ELSE ALLOWFILTERS}
                {$IFNDEF USEHACKS}
                if Byte(RowBuffer[EvenRow][0]) <> 0 then begin
                    result := tinyERROR_UnsupportedDataFilter;
                    CleanUp;
                    exit;
                end;
                {$ENDIF USEHACKS}
                {$ENDIF ALLOWFILTERS}
                RowConversion(Pointer(RowBuffer[EvenRow]+1), Pointer(DWORD(IP.DIBBits) + TargetBytesPerRow*(IP.Height -Row-1)), IP.Width);
                EvenRow := not EvenRow;
            end;
        {$IFDEF INTERLACE}
        end;
        {$ENDIF INTERLACE}
    end; //with CM do begin
//end*****Data Transfer************

    exclude(cuFlags, cu_Bitmap);
    CleanUp;
    TargetBitmap := NewBitmap(0, 0);
    TargetBitmap.Handle := Bitmap;
    result := tinyERROR_OK;
end;
end.
