program irc;

uses
  Classes, SysUtils, StrUtils, test;

var
  FilesToProcess: array of String;
  Identifiers: TStringList;
  OutputFilename: String;
  OutputFileContents: TStringList;

function CheckAndAddIdentifier(Filename: String): Boolean;
var
  strIdentifier: String;
begin
  Result := False;
  strIdentifier := StringReplace(ExtractFileName(Filename), '.', '',
    [rfReplaceAll]);

  if Identifiers.IndexOf(LowerCase(strIdentifier)) <> -1 then
    Result := False
  else
  begin
    Identifiers.Add(LowerCase(strIdentifier));
    Result := True;
  end;
end;

function ProcessParameters: Boolean;
var
  intParamLoop: Integer;
begin
  Result := False;
  for intParamLoop := 1 to ParamCount do
  begin
    case LowerCase(Copy(ParamStr(intParamLoop), 1, 2)) of
      '-f': begin
              SetLength(FilesToProcess, Length(FilesToProcess)+1);
              FilesToProcess[High(FilesToProcess)] :=
                Copy(ParamStr(intParamLoop), 3, Length(
                ParamStr(intParamLoop))-2);
              if FilesToProcess[High(FilesToProcess)][1] = '"' then
                FilesToProcess[High(FilesToProcess)] := Copy(
                  FilesToProcess[High(FilesToProcess)], 3,
                  Length(FilesToProcess[High(FilesToProcess)])-2);
              if not CheckAndAddIdentifier(FilesToProcess[High(
                FilesToProcess)]) then
              begin
                WriteLn(Format('Failed: Duplicate identifier for file "%s"',
                  [FilesToProcess[High(FilesToProcess)]]));
                Halt(3);
              end;
            end;
      '-o': OutputFilename := Copy(ParamStr(intParamLoop), 3, Length(
              ParamStr(intParamLoop))-2);
    end;
  end;

  if (OutputFilename <> '') and (OutputFilename[1] = '"') then
    OutputFilename := Copy(OutputFilename, 2, Length(OutputFilename)-2);

  if (Length(FilesToProcess) > 0) and (OutputFilename <> '') then
    Result := True;
end;

procedure PrintWelcomeMessage(PrintHelp: Boolean = False);
begin
  WriteLn('Pixelanstalt Game Engine Inline Resource Creator');
  WriteLn;
  if PrintHelp then
  begin
    WriteLn(' irc -fFILENAME [-fFILENAME2] ... -oOUTPUT');
    WriteLn;
    WriteLn('The Inline Resource Creator creates a .pas-file as OUTPUT which ');
    WriteLn(' will contain the input files referenced by FILENAME as raw data');
    WriteLn(' in arrays of byte');
  end;
end;

procedure InitOutputFile;
begin
  OutputFileContents := TStringList.Create;
  OutputFileContents.Add('unit ' + ChangeFileExt(ExtractFileName(
    OutputFilename), '') + ';');
  OutputFileContents.Add('');
  OutputFileContents.Add('interface');
  OutputFileContents.Add('');
  OutputFileContents.Add('const');
end;

function ComposeIdentifiers: String;
var
  intIdentLoop: Integer;
  strIdentContents: String;
begin
  strIdentContents := '';
  for intIdentLoop := 0 to Identifiers.Count-1 do
  begin
    if strIdentContents <> '' then
      strIdentContents := strIdentContents + ', ''' +
        Identifiers[intIdentLoop] + ''''
    else
      strIdentContents := '''' + Identifiers[intIdentLoop] + '''';
  end;
  Result := strIdentContents;
end;

procedure DoneOutputFile;
begin
  OutputFileContents.Add('');

  // Add identifiers if possible
  if Identifiers.Count > 1 then
  begin
    OutputFileContents.Add(Format('Identifiers: array[0..%d] of String = (%s);',
      [Identifiers.Count-1, ComposeIdentifiers]));
    OutputFileContents.Add('');
  end;

  OutputFileContents.Add('implementation');
  OutputFileContents.Add('');
  OutputFileContents.Add('end.');
end;

function ComposeByteArray(var Stream: TStream): String;
var
  Buffer: array[0..65535] of Byte;
  intRead, intBufferLoop: Integer;
begin
  Result := '';
  repeat
    intRead := Stream.Read(Buffer, Length(Buffer));
    for intBufferLoop := 0 to intRead-1 do
    begin
      if Result <> '' then
        Result := Result + ', ' + IntToStr(Buffer[intBufferLoop])
      else
        Result := IntToStr(Buffer[intBufferLoop]);
    end;
  until intRead <> Length(Buffer);
end;

procedure ProcessInputFile(Filename: String; Identifier: String);
var
  InputFileStream: TFileStream;
begin
  try
    try
      InputFileStream := TFileStream.Create(Filename, fmOpenRead or
        fmShareDenyWrite);

      OutputFileContents.Add(Format('%s: array[0..%d] of byte = (%s);',
        [Identifier, InputFileStream.Size-1, ComposeByteArray(TStream(
        InputFileStream))]));

      OutputFileContents.Add('');
    finally
      InputFileStream.Free;
    end;
  except
    WriteLn(Format('Failed to process "%s"', [Filename]));
    Halt(2);
  end;
end;

var
  intInputLoop: Integer;
begin
  SetLength(FilesToProcess, 0);
  OutputFilename := '';
  Identifiers := TStringList.Create;

  PrintWelcomeMessage(ParamCount <= 1);

  if not ProcessParameters then
  begin
    WriteLn('Error: No input file(s) or no output file declared');
    Halt(1);
  end;

  WriteLn(Format('%d input file(s) to process', [Length(FilesToProcess)]));

  InitOutputFile;
  for intInputLoop := 0 to High(FilesToProcess) do
  begin
    WriteLn(Format('Processing resource #%d (%s)', [intInputLoop+1,
      FilesToProcess[intInputLoop]]));
    ProcessInputFile(FilesToProcess[intInputLoop], Identifiers[intInputLoop]);
  end;
  DoneOutputFile;
  OutputFileContents.SaveToFile(OutputFilename);
  OutputFileContents.Free;
  Identifiers.Free;
end.

