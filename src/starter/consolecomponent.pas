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
    FTextColor, FTextErrorColor, FTextWarningColor, FTextDebugColor: TColor;
    FHighlight: Integer;

    FCanvas: TCanvas;
    FHeight, FWidth: Integer;

    FInputBuffer: String;

    procedure DrawBackground;
    procedure DrawTextBuffer;
    procedure DrawInputLine;

    procedure EncodeAndDrawLine(X, Line: Integer; Y: Integer);
    procedure ProcessCommand(Command: String);
    function StripCommands(Line: String): String;
    function NumberOfLines(Index: Integer): Integer;
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
  intLoop, intY, intSkip: Integer;
  intLinesPerScreen: Integer;
begin
  FCanvas.Brush.Style := bsClear;
  FCanvas.Font.Color := FTextColor;

  intLinesPerScreen := Floor(FHeight/(FCanvas.Font.Size+3))-1;

  intY := (intLinesPerScreen-1)*(FCanvas.Font.Size+3);
  intLoop := FTextBuffer.Count-1;

  if intLoop >= 0 then
    intSkip := NumberOfLines(intLoop)-1;

  while (intY >= 0) and (intLoop >= 0) do
  begin
    if intSkip = 0 then
    begin
      EncodeAndDrawLine(1, intLoop, intY);

      Dec(intLoop);
      Dec(intY, FCanvas.Font.Size+3);

      if intLoop >= 0 then
        intSkip := NumberOfLines(intLoop)-1;
    end
    else
    begin
      Dec(intSkip);
      Dec(intY, FCanvas.Font.Size+3);
    end;
  end;
end;

procedure TConsoleOutput.DrawInputLine;
begin
  FCanvas.Pen.Color := clSilver;
  FCanvas.Line(1, FHeight-(FCanvas.Font.Size+8), FWidth, FHeight-(FCanvas.Font.Size+8));
  FCanvas.TextOut(1, FHeight-(FCanvas.Font.Size+7), '$>' + FInputBuffer + '▁');
end;

procedure TConsoleOutput.EncodeAndDrawLine(X, Line: Integer; Y: Integer);
var
  strLeft: String;
  intEscapePos, intY: Integer;
  intCurrentXPos: Integer;
  intCMDEndPos: Integer;
begin
  strLeft := FTextBuffer[Line];
  intY := Y;
  intCurrentXPos := X;
  repeat
    intEscapePos := UTF8Pos('\', strLeft);
    if intEscapePos = 1 then
    begin
      // Process Command
      // The current position is a command. Extract the command, process it
      // and delete it from the string
      intCMDEndPos := UTF8Pos(' ', strLeft, intEscapePos);
      // Is the command the last bit of the string? If so, the command ends at
      // the end of the string
      if intCMDEndPos = 0 then
        intCMDEndPos := UTF8Length(strLeft)+1;

      ProcessCommand(UTF8Copy(strLeft, intEscapePos+1,
        intCMDEndPos-intEscapePos-1));

      if intCMDEndPos <> UTF8Length(strLeft)+1 then
        strLeft := UTF8Copy(strLeft, intCMDEndPos+1,
          UTF8Length(strLeft)-intCMDEndPos+1)
      else
        strLeft := '';
    end
    else
    if intEscapePos = 0 then
    begin
      // There is no more command left to process
      if FWidth-intCurrentXPos < UTF8Length(strLeft)*FCanvas.TextWidth('X') then
      begin
        FCanvas.TextOut(intCurrentXPos, intY, UTF8Copy(strLeft, 1,
          Round((FWidth-intCurrentXPos)/FCanvas.TextWidth('X'))));
        strLeft := UTF8Copy(strLeft, Round((FWidth-intCurrentXPos)/
          FCanvas.TextWidth('X')), -1);
        Inc(intY, FCanvas.Font.Size+3);
        intCurrentXPos := 1;
      end
      else
      begin
        FCanvas.TextOut(intCurrentXPos, intY, strLeft);
        strLeft := '';
      end;
    end
    else
    if intEscapePos > 1 then
    begin
      // There are more commands to process. Write the string until next command
      FCanvas.TextOut(intCurrentXPos, intY, UTF8Copy(strLeft, 1, intEscapePos-1));
      Inc(intCurrentXPos, FCanvas.TextWidth(UTF8Copy(strLeft, 1,
        intEscapePos-1)));
      strLeft := UTF8Copy(strLeft, intEscapePos,
        UTF8Length(strLeft)-intEscapePos+1);

    end;
  until Trim(strLeft) = '';
end;

procedure TConsoleOutput.ProcessCommand(Command: String);
begin
  case LowerCase(Command) of
    'high':
      begin
        FCanvas.Font.Style := FCanvas.Font.Style + [fsBold];
        FCanvas.Font.Color := RGBToColor(Min(Red(FCanvas.Font.Color)+
          FHighlight, 255), Min(Green(FCanvas.Font.Color)+FHighlight, 255),
          Min(Blue(FCanvas.Font.Color)+FHighlight, 255));
      end;
    'norm', 'error', 'warn', 'debug':
      begin
        FCanvas.Font.Style := [];
        case LowerCase(Command) of
          'norm': FCanvas.Font.Color := FTextColor;
          'warn': FCanvas.Font.Color := FTextWarningColor;
          'error': FCanvas.Font.Color := FTextErrorColor;
          'debug': FCanvas.Font.Color := FTextDebugColor;
        end;
      end;
  end;
end;

function TConsoleOutput.StripCommands(Line: String): String;
var
  strLeft: String;
  intEscapePos: Integer;
  intCMDEndPos: Integer;
begin
  Result := '';
  strLeft := Line;
  repeat
    intEscapePos := UTF8Pos('\', strLeft);
    if intEscapePos = 1 then
    begin
      // Strip command
      // The current position is a command. Extract the command, process it
      // and delete it from the string
      intCMDEndPos := UTF8Pos(' ', strLeft, intEscapePos);
      // Is the command the last bit of the string? If so, the command ends at
      // the end of the string
      if intCMDEndPos = 0 then
        intCMDEndPos := UTF8Length(strLeft)+1;


      if intCMDEndPos <> UTF8Length(strLeft)+1 then
        strLeft := UTF8Copy(strLeft, intCMDEndPos+1,
          UTF8Length(strLeft)-intCMDEndPos+1)
      else
        strLeft := '';
    end
    else
    if intEscapePos = 0 then
    begin
      // There is no more command left to process
      Result := Result + strLeft;
      strLeft := '';
    end
    else
    if intEscapePos > 1 then
    begin
      // There are more commands to process. Write the string until next command
      Result := Result + UTF8Copy(strLeft, 1, intEscapePos-1);

      strLeft := UTF8Copy(strLeft, intEscapePos,
        UTF8Length(strLeft)-intEscapePos+1);

    end;
  until Trim(strLeft) = '';
end;

function TConsoleOutput.NumberOfLines(Index: Integer): Integer;
begin
  Result := Ceil((UTF8Length(StripCommands(FTextBuffer[Index]))*
    FCanvas.TextWidth('X'))/FWidth);
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
  FTextDebugColor := RGBToColor(0, 200, 200);
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

