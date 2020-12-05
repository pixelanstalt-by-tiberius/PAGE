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

