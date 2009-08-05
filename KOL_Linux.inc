{$IFDEF global_declare}
type DWORD = LongWord;
     PDWORD = ^DWORD;

  PPoint = ^TPoint;
  TPoint = packed record
    X: Longint;
    Y: Longint;
  end;

  PRect = ^TRect;
  TRect = packed record
    case Integer of
      0: (Left, Top, Right, Bottom: Longint);
      1: (TopLeft, BottomRight: TPoint);
  end;

const
  INVALID_HANDLE_VALUE = Cardinal(-1);
  MAX_PATH = 4095;  // From /usr/include/linux/limits.h PATH_MAX

const
{ File attribute constants }
  FILE_ATTRIBUTE_READONLY  = $00000001;
  FILE_ATTRIBUTE_HIDDEN    = $00000002;
  FILE_ATTRIBUTE_SYSTEM    = $00000004;
  FILE_ATTRIBUTE_VOLUME    = $00000008;
  FILE_ATTRIBUTE_DIRECTORY = $00000010;
  FILE_ATTRIBUTE_ARCHIVE   = $00000020;
  FILE_ATTRIBUTE_SYMLINK   = $00000040;
  FILE_ATTRIBUTE_ANYFILE   = $0000003F;
  FILE_ATTRIBUTE_NORMAL    = FILE_ATTRIBUTE_ARCHIVE;
type
  TFilename = type string;
  PFileTime = ^TFileTime;
  TFileTime = __time_t;
  PFindFileData = ^TFindFileData;
  TFindFileData = packed record
   // from TWin32FindData: -------------
    dwFileAttributes: DWORD;
    ftCreationTime: TFileTime;
    ftLastAccessTime: TFileTime;
    ftLastWriteTime: TFileTime;
    nFileSizeHigh: DWORD;
    nFileSizeLow: DWORD;
    //dwReserved0: DWORD;
    //dwReserved1: DWORD;
    cFileName: array[0..MAX_PATH - 1] of Char;
    //cAlternateFileName: array[0..13] of KOLChar; - no in Linux
    //-------- + handle:
    FindHandle: Pointer;
    ExcludeAttr: Integer;
    Mode: mode_t;
    PathOnly: String;
    Pattern: String;
  end;
const
  ExeBaseAddress = Pointer($8048000); // Kylix only?

function GetModuleFileName(Module: HMODULE; Buffer: PChar; BufLen: Integer): Integer;
function DeleteFile(lpFileName: PChar): Boolean;
{$ENDIF global_declare}

{$IFDEF implementation}
//------------------ Unicode strings

function WAnsiUpperCase(const S: WideString): WideString;
var
  I: Integer;
  P: PWideChar;
begin
  SetLength(Result, Length(S));
  P := @Result[1];
  for I := 1 to Length(S) do
    P[I-1] := WideChar(towupper(UCS4Char(S[I])));
end;

function WAnsiLowerCase(const S: WideString): WideString;
var
  I: Integer;
  P: PWideChar;
begin
  SetLength(Result, Length(S));
  P := @Result[1];
  for I := 1 to Length(S) do
    P[I-1] := WideChar(towlower(UCS4Char(S[I])));
end;

//------------------ Ansi strings

function AnsiUpperCase(const S: string): string;
begin
  Result := WAnsiUpperCase( S );
end;

function AnsiLowerCase(const S: string): string;
begin
  Result := WAnsiLowerCase( S );
end;

function AnsiCompareStr(const S1, S2: string): Integer;
begin
  // glibc 2.1.2 / 2.1.3 implementations of strcoll() and strxfrm()
  // have severe capacity limits.  Comparing two 100k strings may
  // exhaust the stack and kill the process.
  // Fixed in glibc 2.1.91 and later.
  Result := strcoll(PChar(S1), PChar(S2));
end;

function _AnsiCompareStr(S1, S2: PChar): Integer;
begin
  // glibc 2.1.2 / 2.1.3 implementations of strcoll() and strxfrm()
  // have severe capacity limits.  Comparing two 100k strings may
  // exhaust the stack and kill the process.
  // Fixed in glibc 2.1.91 and later.
  Result := strcoll(S1, S2);
end;

function AnsiCompareStrNoCase(const S1, S2: string): Integer;
begin
  // glibc 2.1.2 / 2.1.3 implementations of strcoll() and strxfrm()
  // have severe capacity limits.  Comparing two 100k strings may
  // exhaust the stack and kill the process.
  // Fixed in glibc 2.1.91 and later.
  Result := AnsiCompareStr( AnsiUpperCase( S1 ), AnsiUpperCase( S2 ) );
end;

function _AnsiCompareStrNoCase(S1, S2: PChar): Integer;
begin
  // glibc 2.1.2 / 2.1.3 implementations of strcoll() and strxfrm()
  // have severe capacity limits.  Comparing two 100k strings may
  // exhaust the stack and kill the process.
  // Fixed in glibc 2.1.91 and later.
  Result := AnsiCompareStrNoCase( S1, S2 );
end;

//--------------- File functions

function FileCreate(const FileName: string; OpenFlags: DWord): THandle;
begin
  Result := open64( PChar( FileName ), OpenFlags );
end;

function FileClose(Handle: THandle): boolean;
begin
  Result := FALSE;
  if Handle = INVALID_HANDLE_VALUE then Exit;
  __close( Handle );
  Result := TRUE;
end;

function FileExists( const FileName : String ) : Boolean;
var st: TStatBuf;
begin
  Result := FALSE;
  if stat(PChar(FileName), st) = 0 then
    Result := st.st_mtime <> -1;
end;

function FileSeek( Handle: THandle; MoveTo: integer; MoveMethod: TMoveMethod): DWord;
var Temp: Int64;
begin
  Temp := MoveTo;
  Result := lseek64(Handle, Temp, Integer( MoveMethod ));
end;

function FileFarSeek(Handle: THandle; MoveTo: Int64; MoveMethod: TMoveMethod): DWord;
var Temp: Int64;
begin
  Temp := MoveTo;
  Result := lseek64(Handle, Temp, Integer( MoveMethod ));
end;

function FileRead(Handle: THandle; var Buffer; Count: DWord): DWord;
begin
  Result := __read(Handle, Buffer, Count);
end;

function GetFileSize( Handle: THandle; HiSize: PDWORD ): DWORD;
var Pos: Int64;
    Len: Int64;
begin
  Pos := FileSeek( Handle, 0, spCurrent );
  Len := FileSeek( Handle, 0, spEnd );
  FileSeek( Handle, Pos, spBegin );
  Result := I64( Len ).Lo;
  if HiSize <> nil then HiSize^ := I64( Len ).Hi;
end;

function FileWrite( Handle: THandle; const Buffer; Count: DWord): DWord;
begin
  Result := __write(Handle, Buffer, Count);
end;

// the only way for a file name to be not complete in Unix:
// it is located in current working directory (CWD) ??????
function FileFullPath( const FileName: String ) : String;
var wd: String;
    buffer: array[ 0.._POSIX_PATH_MAX+1 ] of Char;
begin
  Result := FileName;
  wd := '';
  if getwd( buffer ) <> nil then
    wd := buffer;
  if StrIsStartingFrom( PChar( FileName ), PChar( wd ) ) then Exit;
  Result := IncludeTrailingPathDelimiter( wd ) + Filename;
  if not FileExists( Result ) then Result := FileName;
end;

function Find_Next(var F: TFindFileData): Boolean;
var
  PtrDirEnt: PDirEnt;
  Scratch: TDirEnt;
  StatBuf: TStatBuf;
  LinkStatBuf: TStatBuf;
  FName: string;
  Attr: Integer;
  Mode: mode_t;
  Sz: Int64;
begin
  Result := FALSE;
  PtrDirEnt := nil;
  if readdir_r(F.FindHandle, @Scratch, PtrDirEnt) <> 0 then
    Exit;
  while PtrDirEnt <> nil do
  begin
    if fnmatch(PChar(F.Pattern), PtrDirEnt.d_name, 0) = 0 then
    begin   // F.PathOnly must include trailing backslash
      FName := F.PathOnly + string(PtrDirEnt.d_name);

      if lstat(PChar(FName), StatBuf) = 0 then
      begin
        Attr := 0;
        Mode := StatBuf.st_mode;

        if S_ISDIR(Mode) then
          Attr := Attr or FILE_ATTRIBUTE_DIRECTORY
        else
        if not S_ISREG(Mode) then  // directories shouldn't be treated as system files
        begin
          if S_ISLNK(Mode) then
          begin
            Attr := Attr or FILE_ATTRIBUTE_SYMLINK;
            if (stat(PChar(FName), LinkStatBuf) = 0) and
              S_ISDIR(LinkStatBuf.st_mode) then
                Attr := Attr or FILE_ATTRIBUTE_DIRECTORY
          end;
          Attr := Attr or FILE_ATTRIBUTE_SYSTEM;
        end;

        if (PtrDirEnt.d_name[0] = '.') and (PtrDirEnt.d_name[1] <> #0) then
        begin
          if not ((PtrDirEnt.d_name[1] = '.') and (PtrDirEnt.d_name[2] = #0)) then
            Attr := Attr or FILE_ATTRIBUTE_HIDDEN;
        end;

        if euidaccess(PChar(FName), W_OK) <> 0 then
          Attr := Attr or FILE_ATTRIBUTE_READONLY;

        if Attr and F.ExcludeAttr = 0 then
        begin
          Sz := StatBuf.st_size;
          F.nFileSizeLow := I64(Sz).Lo;
          F.nFileSizeHigh := I64(Sz).Hi;
          F.dwFileAttributes := Attr;
          F.Mode := StatBuf.st_mode;
          StrCopy( F.cFileName, PtrDirEnt.d_name );
          F.ftCreationTime := StatBuf.st_mtime;
          F.ftLastWriteTime := StatBuf.st_mtime;
          F.ftLastAccessTime := StatBuf.st_atime;
          Result := TRUE;
          Break;
        end;
      end;
    end;
    Result := FALSE;
    if readdir_r(F.FindHandle, @Scratch, PtrDirEnt) <> 0 then
      Break;
  end // End of While
end;

procedure Find_Close(var F: TFindFileData);
begin
  if F.FindHandle <> nil then
  begin
    closedir(F.FindHandle);
    F.FindHandle := nil;
  end;
  F.PathOnly := ''; // in Kylix this is not done (memory leak bug?)
  F.Pattern := '';
end;

function Find_First( const FilePathName: String; var F: TFindFileData ): Boolean;
begin
  FillChar( F, Sizeof( F ), 0 );
  F.ExcludeAttr := FILE_ATTRIBUTE_HIDDEN or FILE_ATTRIBUTE_SYSTEM or
                   FILE_ATTRIBUTE_VOLUME; // or FILE_ATTRIBUTE_DIRECTORY;
  F.PathOnly := ExtractFilePath(FilePathName);
  F.Pattern := ExtractFileName(FilePathName);
  if F.PathOnly = '' then
    F.PathOnly := GetWorkDir;

  F.FindHandle := opendir(PChar(F.PathOnly));
  if F.FindHandle <> nil then
  begin
    if not Find_Next(F) then
    begin
      Find_Close(F);
      Result := FALSE;
      Exit;
    end;
  end;
  Result:= TRUE;
end;

function FileSize( const Path : String ) : Int64;
var F: TFindFileData;
begin
  Result := 0;
  if Find_First( Path, F ) then
  begin
    Result := PInt64( @ F.nFileSizeLow )^;
    Find_Close( F );
  end;
end;

function FileTimeCompare( const FT1, FT2 : TFileTime ) : Integer;
begin
  Result := Sgn( FT1 - FT2 );
end;

function DirectoryExists(const Name: string): Boolean;
var st: TStatBuf;
begin
  if stat(PChar(Name), st) = 0 then
    Result := S_ISDIR(st.st_mode)
  else
    Result := False;
end;

function GetWorkDir : string;
var
  DirBuf: array[0..MAX_PATH] of Char;
begin
  getcwd(DirBuf, sizeof(DirBuf));
  Result := string(DirBuf);
end;

//[function GetModuleFileName] // grabbed form Kylix/system.pas 
function GetModuleFileName(Module: HMODULE; Buffer: PChar; BufLen: Integer): Integer;
var
  Addr: Pointer;
  Info: TDLInfo;
  FoundInModule: HMODULE;
  Temp: Integer;
begin
  Result := 0;
  if BufLen <= 0 then Exit;
  if (Module = MainInstance) or (Module = 0) then
  begin
    // First, try the dlsym approach.
    // dladdr fails to return the name of the main executable
    // in glibc prior to 2.1.91

{   Look for a dynamic symbol exported from this program.
    _DYNAMIC is not required in a main program file.
    If the main program is compiled with Delphi, it will always
    have a resource section, named @Sysinit@ResSym.
    If the main program is not compiled with Delphi, dlsym
    will search the global name space, potentially returning
    the address of a symbol in some other shared object library
    loaded by the program.  To guard against that, we check
    that the address of the symbol found is within the
    main program address range.  }

    dlerror;   // clear error state;  dlsym doesn't
    Addr := dlsym(Pointer( Module ), '@Sysinit@ResSym');
    if (Addr <> nil) and (dlerror = nil)
      and (dladdr(Addr, Info) <> 0)
      and (Info.{FileName}dli_fname <> nil)
      and (Info.{BaseAddress}dli_fbase = ExeBaseAddress) then
    begin
      Result := StrLen(Info.{FileName}dli_fname);
      if Result >= BufLen then Result := BufLen-1;

      // dlinfo may not give a full path.  Compare to /proc/self/exe,
      // take longest result.
      Temp := readlink('/proc/self/exe', Buffer, BufLen);
      if Temp >= BufLen then Temp := BufLen-1;
      if Temp > Result then
        Result := Temp
      else
        Move(Info.{FileName}dli_fname^, Buffer^, Result);
      Buffer[Result] := #0;
      Exit;
    end;

    // Try inspecting the /proc/ virtual file system
    // to find the program filename in the process info
    Result := readlink('/proc/self/exe', Buffer, BufLen);
    if Result <> -1 then
    begin
      if Result >= BufLen then Result := BufLen-1;
      Buffer[Result] := #0;
    end;
{$IFDEF AllowParamStrModuleName}
{   Using ParamStr(0) to obtain a module name presents a potential
    security hole.  Resource modules are loaded based upon the filename
    of a given module.  We use dlopen() to load resource modules, which
    means the .init code of the resource module will be executed.
    Normally, resource modules contain no code at all - they're just
    carriers of resource data.
    An unpriviledged user program could launch our trusted,
    priviledged program with a bogus parameter list, tricking us
    into loading a module that contains malicious code in its
    .init section.
    Without this ParamStr(0) section, GetModuleFilename cannot be
    misdirected by unpriviledged code (unless the system program loader
    or the /proc file system or system root directory has been compromised).
    Resource modules are always loaded from the same directory as the
    given module.  Trusted code (programs, packages, and libraries)
    should reside in directories that unpriviledged code cannot alter.

    If you need GetModuleFilename to have a chance of working on systems
    where glibc < 2.1.91 and /proc is not available, and your
    program will not run as a priviledged user (or you don't care),
    you can define AllowParamStrModuleNames and rebuild the System unit
    and baseCLX package.  Note that even with ParamStr(0) support
    enabled, GetModuleFilename can still fail to find the name of
    a module.  C'est la Unix.  }

    if Result = -1 then // couldn't access the /proc filesystem
    begin               // return less accurate ParamStr(0)

{     ParamStr(0) returns the name of the link used
      to launch the app, not the name of the app itself.
      Also, if this app was launched by some other program,
      there is no guarantee that the launching program has set
      up our environment at all.  (example: Apache CGI) }

      if (ArgValues = nil) or (ArgValues^ = nil) or
        (PCharArray(ArgValues^)[0] = nil) then
      begin
        Result := 0;
        Exit;
      end;
      Result := _strlen(PCharArray(ArgValues^)[0]);
      if Result >= BufLen then Result := BufLen-1;
      Move(PCharArray(ArgValues^)[0]^, Buffer^, Result);
      Buffer[Result] := #0;
    end;
{$ENDIF}
  end
  else
  begin
{   For shared object libraries, we can rely on the dlsym technique.
    Look for a dynamic symbol in the requested module.
    Don't assume the module was compiled with Delphi.
    We look for a dynamic symbol with the name _DYNAMIC.  This
    exists in all ELF shared object libraries that export
    or import symbols;  If someone has a shared object library that
    contains no imports or exports of any kind, this will probably fail.
    If dlsym can't find the requested symbol in the given module, it
    will search the global namespace and could return the address
    of a symbol from some other module that happens to be loaded
    into this process.  That would be bad, so we double check
    that the module handle of the symbol found matches the
    module handle we asked about.}

    dlerror;   // clear error state;  dlsym doesn't
    Addr := dlsym(Pointer( Module ), '_DYNAMIC');
    if (Addr <> nil) and (dlerror = nil)
      and (dladdr(Addr, Info) <> 0) then
    begin
      if Info.{BaseAddress}dli_fbase = ExeBaseAddress then
        Info.{FileName}dli_fname := nil;
      FoundInModule := HMODULE(dlopen(Info.{FileName}dli_fname, RTLD_LAZY));
      if FoundInModule <> 0 then
        dlclose(Pointer( FoundInModule ));
      if Module = FoundInModule then
      begin
        if Assigned(Info.{FileName}dli_fname) then
        begin
          Result := StrLen(Info.{FileName}dli_fname);
          if Result >= BufLen then Result := BufLen-1;
          Move(Info.{FileName}dli_fname^, Buffer^, Result);
        end
        else
          Result := 0;
        Buffer[Result] := #0;
      end;
    end;
  end;
  if Result < 0 then Result := 0;
end;
//[END GetModuleFileName]

function CreateTempFile( const DirPath, Prefix: String ): String;
var i: Integer;
begin
  i := 0;
  REPEAT
    Result := DirPath + Prefix + Int2Str( i );
    inc( i );
  UNTIL not FileExists( Result );
end;

function DeleteFile(lpFileName: PChar): Boolean;
begin
  Result := remove( lpFileName ) = 0;
end;

{--- TTimer ---}
(*)
procedure TTimer.SetEnabled(const Value: Boolean);
begin
  if FEnabled = Value then Exit;
  fEnabled := Value;
  if Value then
  begin
    fTV.it_interval.tv_sec := fInterval div 1000;
    fTV.it_interval.tv_usec := (fInterval mod 1000) * 1000;
    setitimer( fTimerKind,   )
    fHandle := SetTimer( {$IFDEF TIMER_APPLETWND} Applet.GetWindowHandle
                         {$ELSE} TimerOwnerWnd.GetWindowHandle
                         {$ENDIF}, Integer( @Self ),
                         fInterval, @TimerProc );
  end
     else
  begin
    if fHandle <> 0 then
    begin
      KillTimer( TimerOwnerWnd.fHandle, fHandle );
      fHandle := 0;
    end;
  end;
end;
(*)
{$ENDIF implementation}




