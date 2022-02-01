unit amLocalization.Dialog.Tracker;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TranslationManagerTracker = class(TForm)
  private
    procedure MsgCopyData(var Msg: TWMCopyData); message WM_COPYDATA;
  public
  end;

implementation

{$R *.dfm}

{ TranslationManagerTracker }

procedure TranslationManagerTracker.MsgCopyData(var Msg: TWMCopyData);
begin
  TForm(Owner).Dispatch(Msg);
end;

end.
