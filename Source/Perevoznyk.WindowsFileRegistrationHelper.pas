// ----------------------------------------------------------------------------------
// Windows 7 Delphi interface
//
// Serhiy Perevoznyk 
//
// THIS CODE AND INFORMATION ARE PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND,
// EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE IMPLIED WARRANTIES
// OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
// ----------------------------------------------------------------------------------

unit Perevoznyk.WindowsFileRegistrationHelper;

interface

uses
  Windows,
  SysUtils,
  ShlObj,
  ShlWApi;

type
  // Provides a straightforward and useful way for users and developers to
  // customize the Shell's treatment of defined file types.
  // Use file associations any time you need to control, extend, or modify the
  // Shell's treatment of certain file types.
  TFileRegistrationHelper = class
  {$REGION 'Private'}
  private
    FProgID : string;
    FAppPath : string;
    FFriendlyName : string;
    FAppUserModelID : string;
    FExtToRegister : string;
    function RegisterProgid(DoRegister : boolean; LocalToUser : boolean) : HRESULT;
    function RegisterToHandleExt(pszExt : string; fRegister : boolean; LocalToUser : boolean) : HRESULT;
    function RegSetString(Key : HKEY; pszSubKey : string; pszValue : string; pszData : string) : HRESULT;
 {$ENDREGION}
 public
    constructor Create(const ProgramId, Path, FriendlyName,
          AppUserModelID, Extension : string);
    function RegisterToHandleFileType(LocalToUser : boolean) : HRESULT;
    function IsFileTypeRegistered : boolean;
    function UnRegisterFileTypeHandler(LocalToUser : boolean) : HRESULT;
 end;

implementation

{ TFileRegistrationHelper }

function TFileRegistrationHelper.IsFileTypeRegistered: boolean;
var
 hkeyProgid : HKEY;
begin
  Result := false;
  if (SUCCEEDED(HResultFromWin32(RegOpenKey(HKEY_CLASSES_ROOT, PWideChar(FProgID), hkeyProgid)))) then
  begin
    Result := true;
    RegCloseKey(hkeyProgid);
  end;
  if not Result then
   begin
      if (SUCCEEDED(HResultFromWin32(RegOpenKey(HKEY_CURRENT_USER, PWideChar('Software\Classes\' + FProgID), hkeyProgid)))) then
      begin
        Result := true;
        RegCloseKey(hkeyProgid);
      end;
   end;
end;

constructor TFileRegistrationHelper.Create(const ProgramId, Path, FriendlyName,
  AppUserModelID, Extension: string);
begin
  FProgID := ProgramId;
  FAppPath := Path;
  FFriendlyName := FriendlyName;
  FAppUserModelID := AppUserModelID;
  FExtToRegister  := Extension;
end;


function TFileRegistrationHelper.RegisterProgid(DoRegister: boolean; LocalToUser : boolean): HRESULT;
var
 hkeyProgid : HKEY;
 lRes : integer;
 hkeyShell : HKEY;
 szIcon : string;
 szCmdLine : string;

 RootKey : HKEY;
 ProgIdKeyName : PWideChar;

begin
  if LocalToUser then
     RootKey := HKEY_CURRENT_USER
       else
         RootKey := HKEY_CLASSES_ROOT;

  if LocalToUser then
      ProgIdKeyName := PWideChar('Software\Classes\' + FProgId)
        else
          ProgIdKeyName := PWideChar(FProgId);

  if (DoRegister) then
  begin

      Result := HResultFromWin32(RegCreateKeyEx(RootKey, ProgIdKeyName, 0, nil, REG_OPTION_NON_VOLATILE,
          KEY_SET_VALUE or KEY_CREATE_SUB_KEY , nil, hkeyProgid, nil));
      if (SUCCEEDED(Result)) then
      begin
          RegSetString(hkeyProgid, '', 'FriendlyTypeName', FFriendlyName);
          Result := RegSetString(hkeyProgid, '', 'AppUserModelID', FAppUserModelID);
          if (SUCCEEDED(Result)) then
          begin
              szIcon := FAppPath + ',0';
              Result := RegSetString(hkeyProgid, 'DefaultIcon', '', szIcon);
              if (SUCCEEDED(Result)) then
              begin
                  Result := RegSetString(hkeyProgid, 'CurVer', '', FProgID);
                  if (SUCCEEDED(Result)) then
                  begin
                      Result := HResultFromWin32(RegCreateKeyEx(hkeyProgid, 'shell', 0, nil, REG_OPTION_NON_VOLATILE,
                          KEY_SET_VALUE or KEY_CREATE_SUB_KEY, nil, hkeyShell, nil));
                      if (SUCCEEDED(Result)) then
                      begin
                          // The list of verbs provided by the ProgID is located uner the "shell" key.  Here, only
                          // the single "Open" verb is registered.
                          szCmdLine := '"' + FAppPath + '" "%1"';
                          Result := RegSetString(hkeyShell, 'Open\Command', '', szCmdLine);
                          if (SUCCEEDED(Result)) then
                          begin
                              // Set "Open" as the default verb for this ProgID.
                              Result := RegSetString(hkeyShell, '', '', 'Open');
                          end;
                          RegCloseKey(hkeyShell);
                      end
                  end
              end
          end;
          RegCloseKey(hkeyProgid);
      end
  end
  else
  begin
      lRes := SHDeleteKey(RootKey, ProgIdKeyName);
      if ( (ERROR_SUCCESS = lRes) or (ERROR_FILE_NOT_FOUND = lRes)) then
        Result := S_OK
         else
           Result := HResultFromWin32(lRes);
  end;
end;

function TFileRegistrationHelper.RegisterToHandleExt(pszExt: string;
  fRegister: boolean; LocalToUser : boolean): HRESULT;
var
  szKey : string;
  hkeyProgidList : HKEY;
  hkeyExtension  : HKEY;
  RootKey : HKEY;
begin

  if LocalToUser then
     RootKey := HKEY_CURRENT_USER
       else
         RootKey := HKEY_CLASSES_ROOT;

  if LocalToUser then
    szKey := 'Software\Classes\' + pszExt
     else
       szKey := pszExt;

  Result := HResultFromWin32(RegCreateKeyEx(RootKey, PWideChar(szKey), 0, nil, REG_OPTION_NON_VOLATILE,
                KEY_SET_VALUE, nil, hkeyExtension, nil));

  if Succeeded(Result) then
   begin
     RegSetString(hkeyExtension, '', '', PWideChar(FProgId));
     RegCloseKey(hkeyExtension);
   end;
  // All ProgIDs that can handle a given file type should be listed under OpenWithProgids, even if listed
  // as the default, so they can be enumerated in the Open With dialog, and so the Jump Lists can find
  // the correct ProgID to use when relaunching a document with the specific application the Jump List is
  // associated with.
  szKey := szKey + '\OpenWithProgids';
  Result := HResultFromWin32(RegCreateKeyEx(RootKey, PWideChar(szKey), 0, nil, REG_OPTION_NON_VOLATILE,
                KEY_SET_VALUE, nil, hkeyProgidList, nil));
  if (SUCCEEDED(Result)) then
    begin
       if (fRegister) then
         begin
           Result := HResultFromWin32(RegSetValueEx(hkeyProgidList, PWideChar(FProgID), 0, REG_NONE, nil, 0));
         end
            else
               begin
                    Result := HResultFromWin32(RegDeleteValue(hkeyProgidList, PWideChar(FProgID)));
               end;
         RegCloseKey(hkeyProgidList);
    end;
end;

function TFileRegistrationHelper.RegisterToHandleFileType(LocalToUser : boolean): HRESULT;
begin
  Result := RegisterProgid(TRUE, LocalToUser);
  if (SUCCEEDED(Result)) then
  begin
    Result := RegisterToHandleExt(FExtToRegister, TRUE, LocalToUser);
    if (SUCCEEDED(Result)) then
      // Notify that file associations have changed
      SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
   end;
end;

function TFileRegistrationHelper.RegSetString(Key: HKEY; pszSubKey, pszValue,
  pszData: string): HRESULT;
begin
  Result := HResultFromWin32(SHSetValue(Key, PWideChar(pszSubKey), PWideChar(pszValue), REG_SZ, PWideChar(pszData), (Length(pszData) + 1) * sizeof(CHAR)));
end;

function TFileRegistrationHelper.UnRegisterFileTypeHandler(LocalToUser : boolean): HRESULT;
begin
  Result := RegisterProgid(FALSE, LocalToUser);
  if (SUCCEEDED(Result)) then
    begin
       Result := RegisterToHandleExt(FExtToRegister, FALSE, LocalToUser);
       if (SUCCEEDED(Result)) then
         // Notify that file associations have changed
         SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
    end;
end;

end.
