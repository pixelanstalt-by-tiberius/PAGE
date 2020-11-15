unit APIHelper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PAGEAPI, DynLibs;

type

  { TPAGE_APIHelper }

  TPAGE_APIHelper = class
  private
    function GetMethodPointer(Index: Integer): Pointer;
    function GetMethodPointerArrayLength: Integer;
    function GetPAGEAddEventQueueListener: TPAGE_AddEventQueueListener;
    function GetPAGEBindToApp: TPAGE_BindToApp;
    function GetPAGEEnterGameLoop: TPAGE_EnterGameLoop;
    function GetPAGEFinalize: TPAGE_Finalize;
    function GetPAGEGetRendererInfos: TPAGE_GetRendererInfos;
    function GetPAGEInitialize: TPAGE_Initialize;
    procedure SetPAGEAddEventQueueListener(AValue: TPAGE_AddEventQueueListener);
    procedure SetPAGEBindToApp(AValue: TPAGE_BindToApp);
    procedure SetPAGEEnterGameLoop(AValue: TPAGE_EnterGameLoop);
    procedure SetPAGEFinalize(AValue: TPAGE_Finalize);
    procedure SetPAGEGetRendererInfos(AValue: TPAGE_GetRendererInfos);
    procedure SetPAGEInitialize(AValue: TPAGE_Initialize);
  protected
    FMethodPointers: array[0..PAGE_METHOD_NUM-1] of Pointer;
  public
    property PAGEInitialize: TPAGE_Initialize read GetPAGEInitialize write
      SetPAGEInitialize;
    property PAGEFinalize: TPAGE_Finalize read GetPAGEFinalize write
      SetPAGEFinalize;
    property PAGEBindToApp: TPAGE_BindToApp read GetPAGEBindToApp write
      SetPAGEBindToApp;
    property PAGEGetRendererInfos: TPAGE_GetRendererInfos read
      GetPAGEGetRendererInfos write SetPAGEGetRendererInfos;
    property PAGEEnterGameLoop: TPAGE_EnterGameLoop read GetPAGEEnterGameLoop
      write SetPAGEEnterGameLoop;
    property PAGEAddEventQueueListener: TPAGE_AddEventQueueListener read
      GetPAGEAddEventQueueListener write SetPAGEAddEventQueueListener;

    property MethodPointers[Index: Integer]: Pointer read GetMethodPointer;
    property MethodPointerNum: Integer read GetMethodPointerArrayLength;

    procedure GetMethodPointersFromLibrary(hLibrary: TLibHandle);
    function isMethodPointerArrayValid: Boolean;
    procedure DoNilPointerArray;
    function ReturnAddressesAsString: String;
  end;
  PPAGE_APIHelper = ^TPAGE_APIHelper;

implementation

{ TPAGE_APIHelper }

function TPAGE_APIHelper.GetMethodPointer(Index: Integer): Pointer;
begin
  if (Index < Low(FMethodPointers)) or (Index > High(FMethodPointers)) then
    Exception.Create('Method pointer index out of range');

  Result := FMethodPointers[Index];
end;

function TPAGE_APIHelper.GetMethodPointerArrayLength: Integer;
begin
  Result := Length(FMethodPointers);
end;

function TPAGE_APIHelper.GetPAGEAddEventQueueListener: TPAGE_AddEventQueueListener;
begin
  Result := TPAGE_AddEventQueueListener(FMethodPointers[6]);
end;

function TPAGE_APIHelper.GetPAGEBindToApp: TPAGE_BindToApp;
begin
  Result := TPAGE_BindToApp(FMethodPointers[2]);
end;

function TPAGE_APIHelper.GetPAGEEnterGameLoop: TPAGE_EnterGameLoop;
begin
  Result := TPAGE_EnterGameLoop(FMethodPointers[4]);
end;

function TPAGE_APIHelper.GetPAGEFinalize: TPAGE_Finalize;
begin
  Result := TPAGE_Finalize(FMethodPointers[1]);
end;

function TPAGE_APIHelper.GetPAGEGetRendererInfos: TPAGE_GetRendererInfos;
begin
  Result := TPAGE_GetRendererInfos(FMethodPointers[3]);
end;

function TPAGE_APIHelper.GetPAGEInitialize: TPAGE_Initialize;
begin
  Result := TPAGE_Initialize(FMethodPointers[0]);
end;

procedure TPAGE_APIHelper.SetPAGEAddEventQueueListener(
  AValue: TPAGE_AddEventQueueListener);
begin
  FMethodPointers[6] := AValue;
end;

procedure TPAGE_APIHelper.SetPAGEBindToApp(AValue: TPAGE_BindToApp);
begin
  FMethodPointers[2] := AValue;
end;

procedure TPAGE_APIHelper.SetPAGEEnterGameLoop(AValue: TPAGE_EnterGameLoop);
begin
  FMethodPointers[4] := AValue;
end;

procedure TPAGE_APIHelper.SetPAGEFinalize(AValue: TPAGE_Finalize);
begin
  FMethodPointers[1] := AValue;
end;

procedure TPAGE_APIHelper.SetPAGEGetRendererInfos(AValue: TPAGE_GetRendererInfos
  );
begin
  FMethodPointers[3] := AValue;
end;

procedure TPAGE_APIHelper.SetPAGEInitialize(AValue: TPAGE_Initialize);
begin
  FMethodPointers[0] := AValue;
end;

procedure TPAGE_APIHelper.GetMethodPointersFromLibrary(hLibrary: TLibHandle);
var
  intLoop: Integer;
begin
  for intLoop := 0 to High(FMethodPointers) do
    FMethodPointers[intLoop] := GetProcAddress(hLibrary,
      PAGE_METHOD_NAMES[intLoop]);
end;

function TPAGE_APIHelper.isMethodPointerArrayValid: Boolean;
var
  intLoop: Integer;
begin
  Result := True;
  intLoop := 0;
  while (Result) and (intLoop < PAGE_METHOD_NUM) do
  begin
    Result := (FMethodPointers[intLoop] <> nil);
    Inc(intLoop);
  end;
end;

procedure TPAGE_APIHelper.DoNilPointerArray;
begin
  FillByte(FMethodPointers, Length(FMethodPointers)*SizeOf(Pointer), 0);
end;

function TPAGE_APIHelper.ReturnAddressesAsString: String;
var
  intLoop: Integer;
begin
  Result := '';
  for intLoop := 0 to PAGE_METHOD_NUM-1 do
    Result := Result + Format('%x ', [PtrUInt(FMethodPointers[intLoop])]);
end;

end.

