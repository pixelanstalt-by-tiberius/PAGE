{ Main unit of the Pixelanstalt Game Engine Inline Resource Creator.

  Contains all methods to create an inline resource file. }
unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, ZStream;

type
  TFileFlags = set of (ffDummy, ffCompress);

var
  // Filenames (full path) of resource files are stored here
  FilesToProcess: array of String;
  // Flags for inputfiles
  FileFlags: array of TFileFlags;
  // Filenames (full path) of BMFont-files are stored here
  fntToProcess: array of String;

  // Identifiers (names) for the constants are stored in this string list by the
  // CheckAndAddIdentifier @link(CheckAndAddIdentifier) method
  Identifiers: TStringList;

  FNTIdentifiers: TStringList;
  // Filename of the output file
  OutputFilename: String;
  // Will be filled with the pas' contents during program execution
  OutputFileContents: TStringList;


{ Checks whether the constant identifier derived from the filename  already
  exists.

  @param(Filename is the full path qualifier of the input file.)
  @returns(@true if the identifier did not exist and was added to the
           Identifiers @link(Identifiers) list. @false if the derived identifier already exists.)

  The identifier is the stripped from the path and any periods.

  If the identifier does not exist, it will be added to the Identifiers
  stringlist and will return TRUE.
  If the identifier already exists, the function will NOT add the identifier and
  will return FALSE. }
function CheckAndAddIdentifier(Filename: String): Boolean;
function CheckAndAddFNTIdentifier(Filename: String): Boolean;
function ProcessParameters: Boolean;
procedure PrintWelcomeMessage(PrintHelp: Boolean = False);
procedure InitOutputFile;
function ComposeIdentifiers: String;
procedure DoneOutputFile;
function ComposeByteArray(var Stream: TStream): String;
procedure ProcessInputFile(Filename: String; Identifier: String;
  Compress: Boolean = False);
procedure ProcessFNT(FNTFilename: String; Identifier: String);
function ToRect(strRect: String): TRect;
function GetIntValue(Content, ParamName: String): Integer;
function GetStrValue(Content, ParamName: String): String;

const
  CharInfoDefinition =
    '  TFNTCharInfo = record' + #13#10 +
    '    CharID: char;' + #13#10 +
    '    X, Y, W, H: Word;' + #13#10 +
    '    xOffset, yOffset: SmallInt;' + #13#10 +
    '  end;';

  CharInfoTemplate = ('( CharID: #%d; X: %d; Y: %d; W: %d; H: %d; xOffset: %d; yOffset: %d)');

var
  boolFontDef: Boolean;

implementation

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

function CheckAndAddFNTIdentifier(Filename: String): Boolean;
var
  strIdentifier: String;
begin
  Result := False;
  strIdentifier := 'bmf_' + ExtractFileName(ChangeFileExt(Filename, ''));

  if FNTIdentifiers.IndexOf(LowerCase(strIdentifier)) <> -1 then
    Result := False
  else
  begin
    FNTIdentifiers.Add(LowerCase(strIdentifier));
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
              SetLength(FileFlags, Length(FileFlags)+1);
              FileFlags[High(FileFlags)] := [];
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
      '-c': begin
              SetLength(FilesToProcess, Length(FilesToProcess)+1);
              SetLength(FileFlags, Length(FileFlags)+1);
              FilesToProcess[High(FilesToProcess)] :=
                Copy(ParamStr(intParamLoop), 3, Length(
                ParamStr(intParamLoop))-2);
              FileFlags[High(FileFlags)] := [ffCompress];
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
      '-b': begin
              boolFontDef := True;
              SetLength(fntToProcess, Length(fntToProcess)+1);
              fntToProcess[High(fntToProcess)] :=
                Copy(ParamStr(intParamLoop), 3, Length(
                ParamStr(intParamLoop))-2);
              if fntToProcess[High(fntToProcess)][1] = '"' then
                fntToProcess[High(fntToProcess)] := Copy(
                  fntToProcess[High(fntToProcess)], 3,
                  Length(fntToProcess[High(fntToProcess)])-2);
              if not CheckAndAddFNTIdentifier(fntToProcess[High(
                fntToProcess)]) then
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
    WriteLn(' irc -fFILENAME [-fFILENAME2] [-cFILETOCOMPRESS3] [-bFNTFILE] ... -oOUTPUT');
    WriteLn;
    WriteLn('The Inline Resource Creator creates a .pas-file as OUTPUT which ');
    WriteLn(' will contain the input files referenced by FILENAME as raw data');
    WriteLn(' in arrays of byte.');
    WriteLn('If a .fnt-file referenced by FNTFILE is given, the file will be');
    WriteLn(' parsed and it''s contents will be stored in an array with font');
    WriteLn(' information ');
  end;
end;

procedure InitOutputFile;
begin
  OutputFileContents := TStringList.Create;
  OutputFileContents.Add('unit ' + ChangeFileExt(ExtractFileName(
    OutputFilename), '') + ';');
  OutputFileContents.Add('');
  OutputFileContents.Add('interface');
  if boolFontDef then
  begin
    OutputFileContents.Add('');
    OutputFileContents.Add('type');
    OutputFileContents.Add(CharInfoDefinition);
  end;
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
  Buffer: array[0..655359] of Byte;
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

procedure ProcessInputFile(Filename: String; Identifier: String;
  Compress: Boolean);
var
  InputFileStream: TFileStream;
  CompressionStream: TCompressionStream;
  MemoryStream: TMemoryStream;
  SrcStream: Pointer;
begin
  try
    try
      InputFileStream := TFileStream.Create(Filename, fmOpenRead or
        fmShareDenyWrite);
      if Compress then
      begin
        MemoryStream := TMemoryStream.Create;
        CompressionStream := TCompressionStream.create(clMax, MemoryStream,
          True);
        CompressionStream.CopyFrom(InputFileStream, InputFileStream.Size);
        CompressionStream.flush;
        SrcStream := @MemoryStream;
      end
      else
        SrcStream := @InputFileStream;

      OutputFileContents.Add(Format('%s: array[0..%d] of byte = (%s);',
        [Identifier, InputFileStream.Size-1, ComposeByteArray(TStream(
        TStream(SrcStream^)))]));

      OutputFileContents.Add('');
    finally
      if Compress then
      begin
        CompressionStream.Free;
        MemoryStream.Free;
      end;
      InputFileStream.Free;
    end;
  except
    WriteLn(Format('Failed to process "%s"', [Filename]));
    Halt(2);
  end;
end;

procedure ProcessFNT(FNTFilename: String; Identifier: String);
var
  InputFile, Chars: TStringList;
  intLines: Integer;
  pding: TRect;
begin
  InputFile := TStringList.Create;
  InputFile.LoadFromFile(FNTFilename);
  Chars := TStringList.Create;
  Chars.StrictDelimiter := True;
  Chars.Delimiter := ',';

  for intLines := 0 to InputFile.Count-1 do
  begin
    case LowerCase(ExtractDelimited(1, InputFile.Strings[intLines], [' '])) of
      'info':
        begin
          pding := ToRect(GetStrValue(InputFile.Strings[intLines], 'padding'));
        end;
      'char':
        begin
          Chars.Add(Format(CharInfoTemplate, [
            GetIntValue(InputFile.Strings[intLines], 'id'),
            GetIntValue(InputFile.Strings[intLines], 'x'),
            GetIntValue(InputFile.Strings[intLines], 'y'),
            GetIntValue(InputFile.Strings[intLines], 'width'),
            GetIntValue(InputFile.Strings[intLines], 'height'),
            GetIntValue(InputFile.Strings[intLines], 'xoffset'),
            GetIntValue(InputFile.Strings[intLines], 'yoffset')]));
        end;
    end;
  end;

  OutputFileContents.Add(Format('%s: array[0..%d] of TFNTCharInfo = (%s);',
    [Identifier, Chars.Count-1, Chars.DelimitedText]));
  OutputFileContents.Add('');

  Chars.Free;
  InputFile.Free;
end;

function ToRect(strRect: String): TRect;
begin
  Result.Top := StrToInt(ExtractDelimited(1, strRect, [',']));
  Result. Right := StrToInt(ExtractDelimited(2, strRect, [',']));
  Result.Bottom := StrToInt(ExtractDelimited(3, strRect, [',']));
  Result.Left := StrToInt(ExtractDelimited(4, strRect, [',']));
end;

function GetIntValue(Content, ParamName: String): Integer;
begin
  Result := StrToInt(GetStrValue(Content, ParamName));
end;

function GetStrValue(Content, ParamName: String): String;
var
  paramStart, paramEnd, valueStart: Integer;
begin
  paramStart := Pos(ParamName, Content);
  paramEnd := PosEx(' ', Content, paramStart);
  valueStart := PosEx('=', Content, paramStart)+1;
  Result := Copy(Content, valueStart, paramEnd-valueStart);
end;


end.

