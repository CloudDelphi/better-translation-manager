unit amLocalization.Integration.Tracker.API;

interface

{$define DEVEXPRESS} // Define this if you're using the DevExpress layout control

uses
  Classes,
  SysUtils,
  Forms;

type
  TTranslationManagerTrackKind = (tkForm, tkControl);

type
  TranslationManagerIntegration = record
    class function Track(AKind: TTranslationManagerTrackKind; const AName: string): boolean; static;
    class function TrackForm(AForm: TScrollingWinControl): boolean; static;
    class function TrackControl(AControl: TComponent): boolean; static;
  end;

const
  sTranslationManagerTrackerWindow = 'TranslationManagerTracker';

implementation

uses
{$ifdef DEVEXPRESS}
  dxLayoutControl,
  dxLayoutContainer,
{$endif DEVEXPRESS}
  IOUtils,
  Windows,
  Messages,
  Controls;

{ TranslationManagerIntegration }

class function TranslationManagerIntegration.Track(AKind: TTranslationManagerTrackKind; const AName: string): boolean;
begin
  var hTranslationManagerTracker := FindWindow(PChar(sTranslationManagerTrackerWindow), PChar(TPath.GetFileNameWithoutExtension(ParamStr(0))));

  if (hTranslationManagerTracker <> 0) then
  begin
    var CopyDataStruct: TCopyDataStruct;
    CopyDataStruct.dwData := Ord(AKind);
    CopyDataStruct.cbData := ByteLength(AName)+1; // +1 = Include zero termination
    CopyDataStruct.lpData := PChar(AName);

    Result := (SendMessage(hTranslationManagerTracker, WM_COPYDATA, WPARAM(Application.Handle), LPARAM(@CopyDataStruct)) = 1);
  end else
    Result := False;
end;

class function TranslationManagerIntegration.TrackControl(AControl: TComponent): boolean;

  function ItemName: string;
  begin
    Result := '';

    if (AControl is TControl) then
    begin
      if (TControl(AControl).Action <> nil) then
        AControl := TControl(AControl).Action;
    end;

    while (AControl <> nil) do
    begin
      if (AControl.Name <> '') then
      begin
        if (Result <> '') then
          Result := '.' + Result;
        Result := AControl.Name + Result;
      end;

      if (AControl is TCustomForm) or (AControl is TCustomFrame) then
        break;

      var ParentControl: TComponent := nil;
{$ifdef DEVEXPRESS}
      if (AControl is TdxCustomLayoutLabeledItem) then
        ParentControl := TdxCustomLayoutLabeledItem(AControl).Container.ItemsParent
      else
{$endif DEVEXPRESS}
      if (AControl.HasParent) then
        ParentControl := AControl.GetParentComponent;

      if (ParentControl = nil) then
      begin
        if (AControl is TControl) then
          ParentControl := TControl(AControl).Parent
        else
          ParentControl := AControl.Owner;
      end else
      begin
{$ifdef DEVEXPRESS}
      if (ParentControl is TdxLayoutControl) then
      begin
        var LayoutItem := TdxLayoutControl(ParentControl).FindItem(TControl(AControl));
        if (LayoutItem <> nil) and (LayoutItem.CaptionOptions.Visible) then
        begin
          // Restart with layout item
          ParentControl := LayoutItem;
          Result := '';
        end;
      end;
{$endif DEVEXPRESS}
      end;

      AControl := ParentControl;

      if (AControl is TApplication) then
        break;
    end;
  end;

begin
  var Name := ItemName;
  Result := (AControl <> nil) and (Name <> '') and Track(tkControl, Name);
end;

class function TranslationManagerIntegration.TrackForm(AForm: TScrollingWinControl): boolean;
begin
  Result := (AForm <> nil) and Track(tkForm, AForm.ClassName);
end;

end.

