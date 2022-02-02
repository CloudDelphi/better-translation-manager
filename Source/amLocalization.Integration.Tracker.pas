unit amLocalization.Integration.Tracker;

interface

uses
  Vcl.Controls;

type
  ITranslationManagerTracker = interface
    ['{7C103173-F098-4493-8E56-6827FA095726}']
  end;

function CreateTranslationManagerTracker(AMessageSink: TWinControl; const AName: string): ITranslationManagerTracker;

implementation

uses
  System.Classes, Winapi.Windows, Winapi.Messages,
  amLocalization.Integration.Tracker.API;

type
  TTranslationManagerTracker = class(TInterfacedObject, ITranslationManagerTracker)
  strict private
    class var FTrackerWindowClass: TWndClass;
  strict private
    FTrackerWindow: HWND;
    FMessageSink: TWinControl;
  private
    class constructor Create;
  public
    constructor Create(AMessageSink: TWinControl; const AName: string);
    destructor Destroy; override;

    property MessageSink: TWinControl read FMessageSink;
  end;


function CreateTranslationManagerTracker(AMessageSink: TWinControl; const AName: string): ITranslationManagerTracker;
begin
  Result := TTranslationManagerTracker.Create(AMessageSink, AName);
end;

function TranslationManagerTrackerWndProc(hWnd, Msg: Longint; wParam: WPARAM; lParam: LPARAM): Longint; stdcall;
begin
  if (Msg = WM_COPYDATA) then
  begin
    var Tracker := TTranslationManagerTracker(GetWindowLongPtr(hWnd, 0));
    var &Message: TMessage;
    &Message.Msg := Msg;
    &Message.WParam := wParam;
    &Message.LParam := lParam;
    &Message.Result := 0;
    Tracker.MessageSink.Dispatch(&Message);
    Result := &Message.Result;
  end else
    Result := DefWindowProc(hWnd, Msg, wParam, lParam);
end;

{ TTranslationManagerTracker }

constructor TTranslationManagerTracker.Create(AMessageSink: TWinControl; const AName: string);
begin
  inherited Create;
  FMessageSink := AMessageSink;
  FTrackerWindow := CreateWindow(FTrackerWindowClass.lpszClassName, PChar(AName), 0, 0, 0, 0, 0, 0, 0, FTrackerWindowClass.hInstance, nil);
  SetWindowLongPtr(FTrackerWindow, 0, NativeInt(Self));
end;

class constructor TTranslationManagerTracker.Create;
begin
  FTrackerWindowClass.hInstance := hInstance;
  FTrackerWindowClass.lpszClassName := sTranslationManagerTrackerWindow;
  FTrackerWindowClass.lpfnWndProc := @TranslationManagerTrackerWndProc;
  FTrackerWindowClass.cbWndExtra := SizeOf(pointer);
  RegisterClass(FTrackerWindowClass);
end;

destructor TTranslationManagerTracker.Destroy;
begin
  DestroyWindow(FTrackerWindow);

  inherited;
end;

end.
