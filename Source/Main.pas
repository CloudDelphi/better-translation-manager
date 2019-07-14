unit Main;


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls;

type
  TForm4 = class(TForm)
    ListView1: TListView;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  Form4: TForm4;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

implementation

{$R *.dfm}

uses
  amLocalizer,
  IOUtils,
  msxmldom;

procedure TForm4.Button1Click(Sender: TObject);
var
  ProjectProcessor: TProjectProcessorDFMResource;
  LocalizerProject: TLocalizerProject;
begin
  ProjectProcessor := TProjectProcessorDFMResource.Create;
  try
    LocalizerProject := TLocalizerProject.Create(TPath.GetFileNameWithoutExtension(Application.ExeName), GetThreadLocale);
    try
      ProjectProcessor.Execute(LocalizerProject, Application.ExeName,
        function(LocalizerProperty: TLocalizerProperty; var NewValue: string): boolean
        var
          Item: TListItem;
        begin
          NewValue := 'Whoop'+LocalizerProperty.Value;
          Result := True;

              Item := ListView1.Items.Add;

              Item.Caption := LocalizerProperty.Item.Module.Name;
              Item.SubItems.Add(LocalizerProperty.Item.Name);
              Item.SubItems.Add(LocalizerProperty.Item.TypeName);
              Item.SubItems.Add(LocalizerProperty.Name);
              Item.SubItems.Add(LocalizerProperty.Value);
        end);
    finally
      LocalizerProject.Free;
    end;
  finally
    ProjectProcessor.Free;
  end;
end;

procedure TForm4.Button2Click(Sender: TObject);
var
  Importer: TModuleImporterXLIFF;
  LocalizerProject: TLocalizerProject;
  LocalizerModule: TLocalizerModule;
  LocalizerItem: TLocalizerItem;
  LocalizerProperty: TLocalizerProperty;
  Item: TListItem;
begin
  if (not OpenDialog1.Execute(Handle)) then
    exit;

  LocalizerProject := TLocalizerProject.Create('test', GetThreadLocale);
  try
    Importer := TModuleImporterXLIFF.Create;
    try
      Importer.LoadFromFile(LocalizerProject, OpenDialog1.FileName);

      for LocalizerModule in LocalizerProject.Modules.Values.ToArray do
        for LocalizerItem in LocalizerModule.Items.Values.ToArray do
          for LocalizerProperty in LocalizerItem.Properties.Values.ToArray do
          begin
            Item := ListView1.Items.Add;

            Item.Caption := LocalizerProperty.Item.Module.Name;
            Item.SubItems.Add(LocalizerProperty.Item.Name);
            Item.SubItems.Add(LocalizerProperty.Item.TypeName);
            Item.SubItems.Add(LocalizerProperty.Name);
            Item.SubItems.Add(LocalizerProperty.Value);
//            Item.SubItems.Add(LocalizerProperty.NewValue);
          end;
    finally
      Importer.Free;
    end;
  finally
    LocalizerProject.Free;
  end;
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  msxmldom.MSXMLDOMDocumentFactory.AddDOMProperty('ProhibitDTD', False);
  OpenDialog1.InitialDir := TPath.GetDirectoryName(Application.ExeName);
end;

end.
