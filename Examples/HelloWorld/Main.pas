unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Menus;

type
  TFormMain = class(TForm)
    ListBoxValues: TListBox;
    PanelTop: TPanel;
    LabelValue: TLabel;
    EditValue: TEdit;
    ButtonAdd: TButton;
    MainMenu: TMainMenu;
    MenuItemFile: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemLanguage: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ButtonAddClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
  private
    procedure SelectLanguage(Sender: TObject);
    procedure PopulateLanguagesMenu;
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  UITypes,
  IOUtils,
  Registry,
  amLocale,
  amLocalization.Utils;

procedure TFormMain.ButtonAddClick(Sender: TObject);
begin
  ListBoxValues.Items.Insert(0, EditValue.Text);
  EditValue.Text := '';
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  PopulateLanguagesMenu;
end;

procedure TFormMain.MenuItemExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
resourcestring
  sCloseQuery = 'Are you sure you want to exit the application?';
begin
  CanClose := (MessageDlg(sCloseQuery, mtConfirmation, [mbYes, mbNo], 0, mbNo) = mrYes);
end;

procedure TFormMain.PopulateLanguagesMenu;

  function IsLanguageModule(const Filename: string; LocaleID: LCID; var LocaleName: string): boolean;
  var
    ModuleNameScheme: TModuleNameScheme;
    BaseFilename: string;
    ModuleFilename: string;
  begin
    Result := False;
    BaseFilename := TPath.GetFileName(Filename);
    for ModuleNameScheme := Low(TModuleNameScheme) to High(TModuleNameScheme) do
    begin
      ModuleFilename := LocalizationTools.BuildModuleFilename(BaseFilename, LocaleID, ModuleNameScheme);

      if (SameText(BaseFilename, ModuleFilename)) then
      begin
        LocaleName := TPath.GetExtension(BaseFilename);
        Delete(LocaleName, 1, 1); // Remove the '.'
        Result := True;
        break;
      end;
    end;
  end;

  procedure AddLanguage(const Caption, LocaleName: string; Default: boolean);
  var
    MenuItem: TMenuItem;
  begin
    MenuItem := TMenuItem.Create(Self);
    MenuItem.Caption := Caption;
    MenuItem.Hint := LocaleName;
    MenuItem.Default := Default;
    MenuItem.RadioItem := True;
    MenuItem.Checked := SameText(LocaleName, GetLocaleOverride(Application.ExeName));
    MenuItem.OnClick := SelectLanguage;

    MenuItemLanguage.Add(MenuItem);
  end;

var
  FileMask: string;
  Filename: string;
  i: integer;
  LocaleName: string;
  DefaultLocaleItem: TLocaleItem;
  LocaleItem: TLocaleItem;
resourcestring
  sDefaultLanguage = 'Automatic: %s';
begin
  // Populate sub menu with available languages
  MenuItemLanguage.Clear;

  DefaultLocaleItem := TLocaleItems.FindLCID(GetThreadUILanguage);

  // Native application language is en-US
  LocaleItem := TLocaleItems.FindLCID($00000409);
  if (LocaleItem <> nil) then
    AddLanguage(LocaleItem.LanguageName, 'ENU', (LocaleItem = DefaultLocaleItem));

  // Look for files with the same name as the application
  FileMask := TPath.ChangeExtension(TPath.GetFileName(Application.ExeName), '.*');
  for Filename in TDirectory.GetFiles(TPath.GetDirectoryName(Application.ExeName), FileMask) do
    // For each filename see if it matches a language module filename for any of the known locales
    for i := 0 to TLocaleItems.Count-1 do
    begin
      LocaleItem := TLocaleItems.Items[i];
      if (IsLanguageModule(Filename, LocaleItem.Locale, LocaleName)) then
      begin
        // Found a match. Add a menu item.
        AddLanguage(LocaleItem.LanguageName, LocaleName, (LocaleItem = DefaultLocaleItem));
        break;
      end;
    end;

  MenuItemLanguage.NewBottomLine;
  // User default language
  if (DefaultLocaleItem <> nil) then
    AddLanguage(Format(sDefaultLanguage, [DefaultLocaleItem.LanguageName]), '', False);
end;

procedure TFormMain.SelectLanguage(Sender: TObject);
var
  Reg: TRegIniFile;
  LocaleName: string;
resourcestring
  sRestart = 'Please restart the application to use the selected language module';
begin
  LocaleName := TMenuItem(Sender).Hint;

  Reg := TRegIniFile.Create('\Software\Embarcadero', KEY_ALL_ACCESS);
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    if (LocaleName <> '') then
      // Use selected locale
      Reg.WriteString('Locales', Application.ExeName, LocaleName)
    else
      // Use system/user default locale
      Reg.DeleteKey('Locales', Application.ExeName);
  finally
    Reg.Free;
  end;

  TMenuItem(Sender).Checked := True;

  ShowMessage(sRestart);
end;

end.
