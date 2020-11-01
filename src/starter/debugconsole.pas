unit DebugConsole;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  DebugDataHandler, DateUtils;

type

  { TfrmDebugConsole }

  TfrmDebugConsole = class(TForm)
    memoDebug: TMemo;
    pnlConsoleView: TPanel;
    procedure FormCreate(Sender: TObject);
  private
  protected
    procedure DebugDataHandlerOnNewData(Sender: TObject);

  end;

var
  frmDebugConsole: TfrmDebugConsole;

implementation

{$R *.lfm}

{ TfrmDebugConsole }

procedure TfrmDebugConsole.FormCreate(Sender: TObject);
begin
  gDebugDataHandler.OnNewData := @DebugDataHandlerOnNewData;
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
    isWarning: strSeverity := 'Warning';
    isError: strSeverity := 'Error';
    isException: strSeverity := 'Exception';
  end;

  memoDebug.Append(Format('[%s] %s | %s: %s', [TimeToStr(UnixToDateTime(
    LastInfo.Timestamp)), strSeverity, LastInfo.SenderName, LastInfo.Text]));
end;

end.

