unit ConsoleComponent;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazUTF8, Graphics, Math;

type

  { TConsoleOutput }

  TConsoleOutput = class
  private

  protected
    FBackgroundColor: TColor;
    FBackgroundStyle: TBrushStyle;

    FTextBuffer: TStringList;
    FTextColor, FTextErrorColor, FTextWarningColor: TColor;
    FHighlight: Integer;

    FCanvas: TCanvas;
    FHeight, FWidth: Integer;

    FInputBuffer: String;

    procedure DrawBackground;
    procedure DrawTextBuffer;
    procedure DrawInputLine;

    procedure EncodeAndDrawLine(X, Y, Line: Integer);
    procedure ProcessCommand(Command: String);
  public
    constructor Create(ACanvas: TCanvas; AWidth, AHeight: Integer);
    destructor Free;
    procedure DoDraw;

    property Canvas: TCanvas read FCanvas write FCanvas;
    property Buffer: TStringList read FTextBuffer;
    property Height: Integer read FHeight write FHeight;
    property Width: Integer read FWidth write FWidth;
    property InputBuffer: String read FInputBuffer write FInputBuffer;
  end;

implementation

{ TConsoleOutput }

procedure TConsoleOutput.DrawBackground;
begin
  FCanvas.Brush.Color := FBackgroundColor;
  FCanvas.Brush.Style := FBackgroundStyle;
  FCanvas.FillRect(1, 1, FWidth, FHeight);
end;

procedure TConsoleOutput.DrawTextBuffer;
var
  intLoop, intLineOffset: Integer;
  intLinesPerScreen: Integer;
begin
  FCanvas.Brush.Style := bsClear;
  FCanvas.Font.Color := FTextColor;

  intLinesPerScreen := Floor(FHeight/(FCanvas.Font.Size+3))-1;

  if FTextBuffer.Count > intLinesPerScreen then
    intLineOffset := FTextBuffer.Count-intLinesPerScreen
  else
    intLineOffset := 0;

  for intLoop := intLineOffset to FTextBuffer.Count-1 do
    EncodeAndDrawLine(1, ((intLoop-intLineOffset)*(FCanvas.Font.Size+3)), intLoop);
end;

procedure TConsoleOutput.DrawInputLine;
begin
  FCanvas.Pen.Color := clSilver;
  FCanvas.Line(1, FHeight-(FCanvas.Font.Size+8), FWidth, FHeight-(FCanvas.Font.Size+8));
  FCanvas.TextOut(1, FHeight-(FCanvas.Font.Size+7), '$>' + FInputBuffer + '▁');
end;

procedure TConsoleOutput.EncodeAndDrawLine(X, Y, Line: Integer);
var
  strLeft: String;
  intEscapePos: Integer;
  intCurrentXPos: Integer;
  intCMDEndPos: Integer;
begin
  { TODO: Add word wrap }
  strLeft := FTextBuffer[Line];
  intCurrentXPos := X;
  repeat
    intEscapePos := UTF8Pos('\', strLeft);
    if intEscapePos = 1 then
    begin
      intCMDEndPos := UTF8Pos(' ', strLeft, intEscapePos);
      if intCMDEndPos = 0 then
        intCMDEndPos := UTF8Length(strLeft)+1;
      ProcessCommand(UTF8Copy(strLeft, intEscapePos+1,
          intCMDEndPos-intEscapePos-1));
      if intCMDEndPos <> UTF8Length(strLeft) then
        strLeft := UTF8Copy(strLeft, intCMDEndPos+1,
          UTF8Length(strLeft)-intCMDEndPos+1)
      else
        strLeft := '';
    end
    else
    if intEscapePos = 0 then
    begin
      FCanvas.TextOut(intCurrentXPos, Y, strLeft);
      strLeft := '';
    end
    else
    if intEscapePos > 1 then
    begin
      FCanvas.TextOut(intCurrentXPos, Y, UTF8Copy(strLeft, 1, intEscapePos-1));
      Inc(intCurrentXPos, FCanvas.TextWidth(UTF8Copy(strLeft, 1, intEscapePos-1)));
      strLeft := UTF8Copy(strLeft, intEscapePos,
        UTF8Length(strLeft)-intEscapePos+1);

    end;
  until Trim(strLeft) = '';
end;

procedure TConsoleOutput.ProcessCommand(Command: String);
begin
  case Command of
    'High':
      begin
        FCanvas.Font.Style := FCanvas.Font.Style + [fsBold];
        FCanvas.Font.Color := RGBToColor(Min(Red(FCanvas.Font.Color)+
          FHighlight, 255), Min(Green(FCanvas.Font.Color)+FHighlight, 255),
          Min(Blue(FCanvas.Font.Color)+FHighlight, 255));
      end;
    'Norm', 'Error', 'Warn':
      begin
        FCanvas.Font.Style := [];
        case Command of
          'Norm': FCanvas.Font.Color := FTextColor;
          'Warn': FCanvas.Font.Color := FTextWarningColor;
          'Error': FCanvas.Font.Color := FTextErrorColor;
        end;
      end;
  end;
end;

constructor TConsoleOutput.Create(ACanvas: TCanvas; AWidth, AHeight: Integer);
begin
  FCanvas := ACanvas;
  FWidth := AWidth;
  FHeight := AHeight;
  FBackgroundColor := clBlack;
  FBackgroundStyle := bsSolid;
  FTextColor := RGBToColor(0, 200, 0);
  FTextErrorColor := RGBToColor(200, 0, 0);
  FTextWarningColor := RGBToColor(200, 200, 0);
  FHighlight := 55;
  FTextBuffer := TStringList.Create;
end;

destructor TConsoleOutput.Free;
begin
  FTextBuffer.Free;
end;

procedure TConsoleOutput.DoDraw;
begin
  DrawBackground;
  DrawTextBuffer;
  DrawInputLine;
end;

end.

