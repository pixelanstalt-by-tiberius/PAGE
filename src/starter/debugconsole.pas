unit DebugConsole;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  DebugDataHandler, DateUtils, ConsoleComponent, LCLType, Character, LazUTF8;

type

  { TfrmDebugConsole }

  TfrmDebugConsole = class(TForm)
    Console: TPaintBox;
    pnlConsoleView: TPanel;
    UpdateDispatchedEvents: TTimer;
    procedure ConsolePaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure UpdateDispatchedEventsTimer(Sender: TObject);
  private
  protected
    FConsoleOutput: TConsoleOutput;
    procedure DebugDataHandlerOnNewData(Sender: TObject);

  end;

var
  frmDebugConsole: TfrmDebugConsole;

implementation

{$R *.lfm}

{ TfrmDebugConsole }

procedure TfrmDebugConsole.FormCreate(Sender: TObject);
begin
  if Console.Width mod Console.Canvas.TextWidth('X') <> 0 then
    Width := Width - (Console.Width mod Console.Canvas.TextWidth('X'));
  FConsoleOutput := TConsoleOutput.Create(Console.Canvas, Console.Width,
    Console.Height);
  gDebugDataHandler.OnNewData := @DebugDataHandlerOnNewData;
end;

procedure TfrmDebugConsole.FormDestroy(Sender: TObject);
begin
  FConsoleOutput.Free;
end;

procedure TfrmDebugConsole.FormUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
begin
  case GetUnicodeCategory(UTF8ToUTF16(UTF8Key), 1) of
    TUnicodeCategory.ucControl:
      begin
        case UTF8Key of
          #8: FConsoleOutput.InputBuffer := UTF8Copy(FConsoleOutput.InputBuffer,
                1, UTF8Length(FConsoleOutput.InputBuffer)-1);
          #13:
            begin
              //FConsoleOutput.Buffer.Add(FConsoleOutput.InputBuffer);
              //FConsoleOutput.InputBuffer := '';
            end;
        end;
      end;
  else
    FConsoleOutput.InputBuffer := FConsoleOutput.InputBuffer + UTF8Key;
  end;

  Console.Invalidate;
end;

procedure TfrmDebugConsole.UpdateDispatchedEventsTimer(Sender: TObject);
begin
  gDebugDataHandler.UpdateDispatchedEventQueue;
end;

procedure TfrmDebugConsole.ConsolePaint(Sender: TObject);
begin
  FConsoleOutput.DoDraw;
end;

procedure TfrmDebugConsole.DebugDataHandlerOnNewData(Sender: TObject);
var
  LastInfo: TInfo;
  strSeverity: String;
begin
  LastInfo := (Sender as TDebugDataHandler).Infos[(Sender as
    TDebugDataHandler).InfoCount-1];
  case LastInfo.Severity of
    isDebug: strSeverity := 'Debug';
    isWarning: strSeverity := '\Warn Warning \Norm';
    isError: strSeverity := '\Error Error \Norm';
    isException: strSeverity := '\Error \High Exception \Norm';
  end;

  FConsoleOutput.Buffer.Add(Format('[%s] %s | %s: %s', [TimeToStr(UnixToDateTime(
    LastInfo.Timestamp)), strSeverity, LastInfo.SenderName, LastInfo.Text]));

  Console.Invalidate;

  //memoDebug.Append(Format('[%s] %s | %s: %s', [TimeToStr(UnixToDateTime(
  //  LastInfo.Timestamp)), strSeverity, LastInfo.SenderName, LastInfo.Text]));
end;

end.

