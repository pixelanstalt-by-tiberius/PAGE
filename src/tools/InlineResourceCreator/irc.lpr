{ Pixelanstalt Game Engine Inline Resource Creator.

  This command line application takes all files passed by the -f parameter and
  stores their contents into byte arrays. These byte arrays will be identified
  by the input filenames and stored into the output file determined by the -o
  parameter. }
program irc;

uses
  Classes, SysUtils, StrUtils, main;


var
  intInputLoop: Integer;
begin
  SetLength(FilesToProcess, 0);
  OutputFilename := '';
  Identifiers := TStringList.Create;
  FNTIdentifiers := TStringList.Create;
  boolFontDef := False;

  PrintWelcomeMessage(ParamCount <= 1);

  if not ProcessParameters then
  begin
    WriteLn('Error: No input file(s) or no output file declared');
    Halt(1);
  end;

  WriteLn(Format('%d input file(s) to process', [Length(FilesToProcess)]));
  WriteLn(Format('%d FNT-file(s) to process', [Length(FNTToProcess)]));

  InitOutputFile;
  for intInputLoop := 0 to High(FilesToProcess) do
  begin
    if FileFlags[intInputLoop] <> [] then
    begin
      if ffCompress in FileFlags[intInputLoop] then
        WriteLn(Format('Processing resource #%d [compress] (%s)', [intInputLoop+1,
          FilesToProcess[intInputLoop]]));
    end
    else
      WriteLn(Format('Processing resource #%d (%s)', [intInputLoop+1,
        FilesToProcess[intInputLoop]]));

    ProcessInputFile(FilesToProcess[intInputLoop], Identifiers[intInputLoop],
      ffCompress in FileFlags[intInputLoop]);
  end;
  for intInputLoop := 0 to High(FNTToProcess) do
  begin
    WriteLn(Format('Processing FNT #%d (%s)', [intInputLoop+1,
      FNTToProcess[intInputLoop]]));
    ProcessFNT(FNTToProcess[intInputLoop], FNTIdentifiers[intInputLoop]);
  end;
  DoneOutputFile;
  OutputFileContents.SaveToFile(OutputFilename);
  OutputFileContents.Free;
  Identifiers.Free;
  FNTIdentifiers.Free;
end.

