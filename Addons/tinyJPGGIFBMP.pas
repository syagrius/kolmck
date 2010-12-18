unit tinyJPGGIFBMP;

//  file: tinyJPGGIFBMP.pas
//  file version: 0.35
//  last modified: 05.01.06
//  package: GRushControls
//  author: Karpinskyj Alexandr aka homm
//      mailto: homm86@mail.ru
//      My humble Web-Page: http://www.homm86.narod.ru


interface

uses
  Windows, KOL, ActiveX;

type
  TBitmapmod = object( TBitMap )
  end;

procedure tinyLoadJPGGIFBMPFile(var TargetBitmap: PBitMap; FileName: String);
procedure tinyLoadJPGGIFBMPMemory(var TargetBitmap: PBitMap; Ptr: HGlobal; Size: DWORD);
procedure tinyLoadJPGGIFBMPResource(var TargetBitmap: PBitMap; Inst : HInst; ResName : PChar; ResType : PChar);

implementation

const
  IID_IPicture:TGUID='{7BF80980-BF32-101A-8BBB-00AA00300CAB}';

function SHCreateStreamOnFileA(FileName: PChar; grfMode: DWORD;var stream: IStream): HResult;
    external 'shlwapi.dll' name 'SHCreateStreamOnFileA';

procedure OleFree( Picta: IPicture );
{begin
    if Picta <> nil then
        Picta._Release;}
asm
    push eax
    mov eax, esp
    call System.@IntFClear
    pop eax  
end;

procedure tinyLoadJPGGIFBMPFile(var TargetBitmap: PBitMap; FileName: String);
var     Stream: IStream;
        Picta: IPicture;
        hh: THandle;
asm
    //[ebx] = PBitmap;
    //edi = FileName;
    push ebx
    push edi
    mov ebx, eax
    mov edi, edx
    //BitMap := nil;
    xor eax, eax
    mov [ebx], eax
    //SHCreateStreamOnFileA(PChar(FileName), 0, Stream);
    lea eax, [Stream]
        push eax
        push $00
        push edi //FileName
        call SHCreateStreamOnFileA
    //if Stream=nil then exit;
    cmp Dword ptr [Stream], $00
    jz @@EXIT
    //OleLoadPicture(Stream, FileSize(FileName), false, IID_IPicture, Picta);
    lea eax, [Picta]
        push eax
        push offset IID_IPicture
        push $00
    mov eax, edi //FileName
    call KOL.FileSize
        push eax
    mov eax, [Stream]
        push eax
        call ActiveX.OleLoadPicture
    //if Picta = nil then exit;
    cmp Dword ptr [Picta], $00
    jz @@EXIT
    //Picta.get_Handle(hh);
    lea eax, [hh]
        push eax
    mov edx, [Picta]
        push edx
    mov eax, [edx]
        call dword ptr [eax+$0c]
    //BitMap := NewBitMap(0, 0);
    xor eax, eax
    xor edx, edx
    call NewBitmap
    mov [ebx], eax
    //BitMap.Handle := hh;
    mov edx, [hh]
    call TBitMapMod.SetHandle
    //BitMap.Add2AutoFreeEx(TObjectMethod(MakeMethod(Pointer(Picta), @OleFree)));
    mov eax, [Picta]
    push eax
    push offset OleFree
    mov eax, [ebx]
    call TObj.Add2AutoFreeEx

@@EXIT:
    lea eax, [Stream]
    call System.@IntFClear

    pop edi
    pop ebx
end;

procedure tinyLoadJPGGIFBMPMemory(var TargetBitmap: PBitMap; Ptr: HGlobal; Size: DWORD);
var     Stream: IStream;
        Picta: IPicture;
        hh: THandle;
begin
    TargetBitmap := nil;
    if CreateStreamOnHGlobal(ptr, TRUE, Stream) <> S_OK then
        exit;
    if OleLoadPicture(Stream, Size, false, IID_IPicture, Picta) <> S_OK then
        exit;
    Picta.get_Handle(hh);
    Picta._AddRef;
    TargetBitmap := NewBitmap(0, 0);
    TargetBitmap.Handle := hh;
    TargetBitmap.Add2AutoFreeEx(TObjectMethod(MakeMethod(Pointer(Picta), @OleFree)));
end;

procedure tinyLoadJPGGIFBMPResource(var TargetBitmap: PBitMap; Inst : HInst; ResName : PChar; ResType : PChar);
var     G: Pointer;
        Sz: DWORD;
        Ptr: Pointer;
        Resource: HRSRC;
begin
    Resource := FindResource(Inst, ResName, ResType);
    Sz := SizeofResource(Inst, Resource);
    DWORD(G) := LoadResource(hinstance, Resource);
    DWORD(Ptr) := LocalAlloc(GMEM_FIXED, Sz);
    move(g^, Ptr^, Sz);
    tinyLoadJPGGIFBMPMemory(TargetBitmap, DWORD(Ptr), Sz);
end;

end.
