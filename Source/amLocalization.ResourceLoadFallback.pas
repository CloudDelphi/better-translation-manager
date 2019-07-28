unit amLocalization.ResourceLoadFallback;

interface

// -----------------------------------------------------------------------------
// Replaces the FindResourceW, LoadResource and SizeofResource WinAPI functions
// with similar functions that first tries to operate of the specified module
// and if that fails tries to operate on the main module.
// -----------------------------------------------------------------------------
// Returns True on success, False otherwise.
// You should call DisableResourceLoadingFallback to restore the original
// functions regardless of the result.
// -----------------------------------------------------------------------------
function EnableResourceLoadingFallback: Boolean;


// -----------------------------------------------------------------------------
// Unhook FindResourceW, LoadResource and SizeofResource WinAPI functions.
// -----------------------------------------------------------------------------
// Returns True on success, False otherwise.
// -----------------------------------------------------------------------------
function DisableResourceLoadingFallback: Boolean;


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

implementation

uses
  Windows;


// -----------------------------------------------------------------------------
// Redirect a WinAPI function
// -----------------------------------------------------------------------------
function HookAPI(const Name, Module: string; Hook: pointer): pointer;
var
  ImageBase, Old: Cardinal;
  PEHeader: PImageNtHeaders;
  PImport: PImageImportDescriptor;
  PRVA_Import: LPDWORD;
  ProcAddress: Pointer;
begin
  Result := nil;

  ImageBase := GetModuleHandle(nil);
  PEHeader := Pointer(Int64(ImageBase) + PImageDosHeader(ImageBase)._lfanew);

  // Get pointer to the imports table of the main process
  PImport := Pointer(PEHeader.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT].VirtualAddress + ImageBase);

  // Get address of the WinAPI function we want to hook
  ProcAddress := GetProcAddress(GetModuleHandle(PChar(Module)), PChar(Name));
  if ProcAddress = NIL then
    Exit;

  while (PImport.Name <> 0) do
  begin
    PRVA_Import := LPDWORD(pImport.FirstThunk + ImageBase);

    // Scan import table for the function to be redirected
    while (PRVA_Import^ <> 0) do
    begin
      if (PPointer(PRVA_Import)^ = ProcAddress) then
      begin
        // Import table is in read-only segment. Change it to read-write so we can modify it.
        if (not VirtualProtect(PPointer(PRVA_Import), SizeOf(Pointer), PAGE_READWRITE, Old)) then
          Exit;

        // Return address of existing function
        Result := PPointer(PRVA_Import)^;

        // Replacing import address with our hook
        PPointer(PRVA_Import)^ := Hook;

        // Restore old memory protection mode
        if (not VirtualProtect(PPointer(PRVA_Import), SizeOf(Pointer), Old, Old)) then
          Result := nil;

        Exit;
      end;

      Inc(PRVA_Import);
    end;

    Inc(PImport);
  end;
end;


// -----------------------------------------------------------------------------
// Replacement FindResourceW, LoadResource and SizeofResource implementations.
// -----------------------------------------------------------------------------
// First tries operation on specified module (supposedly a resource module) and
// if that fails, tries operation on main module (the application).
// -----------------------------------------------------------------------------
type
  TFindResourceW = function(hModule: HMODULE; lpName, lpType: PWideChar): HRSRC; stdcall;

var
  // Pointer to original WinAPI FindResourceW function
  FindResourceW: TFindResourceW = nil;

function HookedFindResourceW(hModule: HMODULE; lpName, lpType: PWideChar): HRSRC; stdcall;
begin
  Result := FindResourceW(hModule, lpName, lpType);

  if (Result = 0) and (hModule <> hInstance) then
    Result := FindResourceW(hInstance, lpName, lpType);
end;

// -----------------------------------------------------------------------------

type
  TLoadResource = function(hModule: HINST; hResInfo: HRSRC): HGLOBAL; stdcall;

var
  // Pointer to original WinAPI LoadResource function
  LoadResource: TLoadResource = nil;

function HookedLoadResource(hModule: HINST; hResInfo: HRSRC): HGLOBAL; stdcall;
begin
  Result := LoadResource(hModule, hResInfo);

  if (Result = 0) and (hModule <> hInstance) then
    Result := LoadResource(hInstance, hResInfo);
end;


// -----------------------------------------------------------------------------

type
  TSizeofResource = function(hModule: HINST; hResInfo: HRSRC): DWORD; stdcall;

var
  // Pointer to original WinAPI SizeofResource function
  SizeofResource: TSizeofResource = nil;

function HookedSizeofResource(hModule: HINST; hResInfo: HRSRC): DWORD; stdcall;
begin
  Result := SizeofResource(hModule, hResInfo);

  if (Result = 0) and (hModule <> hInstance) then
    Result := SizeofResource(hInstance, hResInfo);
end;


// -----------------------------------------------------------------------------
// Redirect the FindResourceW, LoadResource and SizeofResource WinAPI functions
// to our own implementations.
// -----------------------------------------------------------------------------
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


// -----------------------------------------------------------------------------
// Unhook FindResourceW, LoadResource and SizeofResource WinAPI functions.
// -----------------------------------------------------------------------------
function DisableResourceLoadingFallback: Boolean;
begin
  Result := True;

  if (Assigned(FindResourceW)) then
    Result := (HookAPI('FindResourceW', kernel32, @FindResourceW) <> nil);

  if (Assigned(LoadResource)) then
    Result := (HookAPI('LoadResource', kernel32, @LoadResource) <> nil) and Result;

  if (Assigned(SizeofResource)) then
    Result := (HookAPI('SizeofResource', kernel32, @SizeofResource) <> nil) and Result;
end;

// -----------------------------------------------------------------------------

initialization
  EnableResourceLoadingFallback;

finalization
  DisableResourceLoadingFallback;

end.
