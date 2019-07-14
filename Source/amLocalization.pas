unit amLocalization;

interface

function EnableResourceLoadingFallback: Boolean;

implementation

uses
  Windows;

function HookAPI(const Name, Module: string; Hook: pointer): pointer;
var
  ImageBase, Old: Cardinal;
  PEHeader: PImageNtHeaders;
  PImport: PImageImportDescriptor;
  PRVA_Import: LPDWORD;
  ProcAddress: Pointer;
begin
  Result := nil;

  ImageBase := GetModuleHandle(NIL);
  PEHeader := Pointer(Int64(ImageBase) + PImageDosHeader(ImageBase)._lfanew);
  // pointer to the imports table of the main process:
  PImport := Pointer(PEHeader.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT].VirtualAddress + ImageBase);

  // pointer to the WinAPI function we want to hook:
  ProcAddress := GetProcAddress(GetModuleHandle(PChar(Module)), PChar(Name));
  if ProcAddress = NIL then
    Exit;

  while PImport.Name <> 0 do
  begin
    PRVA_Import := LPDWORD(pImport.FirstThunk + ImageBase);
    while PRVA_Import^ <> 0 do
    begin
      if PPointer(PRVA_Import)^ = ProcAddress then
      begin
        // initially imports table is in read-only segment:
        if not VirtualProtect(PPointer(PRVA_Import), 4, PAGE_READWRITE, Old) then
          Exit;

        Result := PPointer(PRVA_Import)^;

        // replacing import address with our own:
        PPointer(PRVA_Import)^ := Hook;

        // restoring old memory protection mode:
        if not VirtualProtect(PPointer(PRVA_Import), 4, Old, Old) then
        begin
          Result := nil;
          Exit;
        end;

      end;

      Inc(PRVA_Import);
    end;

    Inc(PImport);
  end;
end;


type
  TFindResourceW = function(hModule: HMODULE; lpName, lpType: PWideChar): HRSRC; stdcall;
var
  FindResourceW: TFindResourceW = nil;

function HookedFindResourceW(hModule: HMODULE; lpName, lpType: PWideChar): HRSRC; stdcall;
begin
  Result := FindResourceW(hModule, lpName, lpType);

  if (Result = 0) and (hModule <> hInstance) then
    Result := FindResourceW(hInstance, lpName, lpType);
end;

type
  TLoadResource = function(hModule: HINST; hResInfo: HRSRC): HGLOBAL; stdcall;
var
  LoadResource: TLoadResource = nil;

function HookedLoadResource(hModule: HINST; hResInfo: HRSRC): HGLOBAL; stdcall;
begin
  Result := LoadResource(hModule, hResInfo);

  if (Result = 0) and (hModule <> hInstance) then
    Result := LoadResource(hInstance, hResInfo);
end;


type
  TSizeofResource = function(hModule: HINST; hResInfo: HRSRC): DWORD; stdcall;

var
  SizeofResource: TSizeofResource = nil;

function HookedSizeofResource(hModule: HINST; hResInfo: HRSRC): DWORD; stdcall;
begin
  Result := SizeofResource(hModule, hResInfo);

  if (Result = 0) and (hModule <> hInstance) then
    Result := SizeofResource(hInstance, hResInfo);
end;

function EnableResourceLoadingFallback: Boolean;
begin
  FindResourceW := HookAPI('FindResourceW', kernel32, @HookedFindResourceW);
  Result := Assigned(FindResourceW);

  if (Result) then
  begin
    LoadResource := HookAPI('LoadResource', kernel32, @HookedLoadResource);
    Result := Assigned(LoadResource);
  end;

  if (Result) then
  begin
    SizeofResource := HookAPI('SizeofResource', kernel32, @HookedSizeofResource);
    Result := Assigned(SizeofResource);
  end;
end;

initialization
  EnableResourceLoadingFallback;
finalization
end.
