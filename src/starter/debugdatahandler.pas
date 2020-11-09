unit DebugDataHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, PAGEAPI;

type
  TInfoSeverity = (isDebug, isWarning, isError, isException);

  TInfo = record
    Timestamp: Int64;
    Severity: TInfoSeverity;
    SenderName: String;
    Text: String;
  end;

  { TDebugDataHandler }

  TDebugDataHandler = class
  private
    function GetInfoCount: Integer;
    function GetInfo(Index: Integer): TInfo;
    function GetOnNewData: TNotifyEvent;
    procedure SetOnNewData(AValue: TNotifyEvent);
  protected
    FOnNewData: TNotifyEvent;
    { TODO: Modify to constant }
    FDispatchedEvents: array[0..65535] of TPAGE_Event;
    FDispatchedEventStrings: array[0..65535] of String;
    FNumDispatchedEvents: Integer;

    FInfos: array of TInfo;

    procedure AddInfo(ASenderName, AText: String; ASeverity: TInfoSeverity);
  public
    constructor Create;
    destructor Destroy; override;

    procedure DebugInfoText(ASender: TObject; ADebugInfo: String);
    procedure WarningInfoText(ASender: TObject; AWarningInfo: String);
    procedure ErrorInfoText(ASender: TObject; AErrorInfo: String);
    procedure ExceptionInfoText(ASender: TObject; AExceptionInfo: String);

    procedure UpdateDispatchedEventQueue;

    property InfoCount: Integer read GetInfoCount;
    property Infos[Index: Integer]: TInfo read GetInfo;
    property OnNewData: TNotifyEvent read GetOnNewData write SetOnNewData;
  end;


procedure EventQueueDispatch(aDispatchedEvent: TPAGE_Event; aMessage: PChar);

var
  gDebugDataHandler: TDebugDataHandler;
  gEventDispatchCriticalSection: TRTLCriticalSection;

implementation

{ TDebugDataHandler }

function TDebugDataHandler.GetInfoCount: Integer;
begin
  Result := Length(FInfos);
end;

function TDebugDataHandler.GetInfo(Index: Integer): TInfo;
begin
  if (Index > High(FInfos)) or (Index < Low(FInfos)) then
    Exception.CreateFmt('Info Index (%d) out of bounds', [Index]);

  Result := FInfos[Index];
end;

function TDebugDataHandler.GetOnNewData: TNotifyEvent;
begin
  Result := FOnNewData;
end;

procedure TDebugDataHandler.SetOnNewData(AValue: TNotifyEvent);
begin
  FOnNewData := AValue;
end;

procedure TDebugDataHandler.AddInfo(ASenderName, AText: String;
  ASeverity: TInfoSeverity);
begin
  // TODO: Reserve enough space and save current pointer
  SetLength(FInfos, Length(FInfos)+1);
  with FInfos[High(FInfos)] do
  begin
    Timestamp := DateTimeToUnix(Now);
    Severity := ASeverity;
    SenderName := ASenderName;
    Text := AText;
  end;

  if Assigned(FOnNewData) then
    FOnNewData(Self);
end;

constructor TDebugDataHandler.Create;
begin
  if Assigned(gDebugDataHandler) then
    Exception.Create('Tried to create singleton ''Debug Data Handler'' twice');

  FOnNewData := nil;
  FNumDispatchedEvents := 0;
  InitCriticalSection(gEventDispatchCriticalSection);
end;

destructor TDebugDataHandler.Destroy;
begin
  DoneCriticalSection(gEventDispatchCriticalSection);
  SetLength(FInfos, 0);
end;

procedure TDebugDataHandler.DebugInfoText(ASender: TObject; ADebugInfo: String);
begin
  AddInfo(ASender.ClassName, ADebugInfo, isDebug);
end;

procedure TDebugDataHandler.WarningInfoText(ASender: TObject;
  AWarningInfo: String);
begin
  AddInfo(ASender.ClassName, AWarningInfo, isWarning);
end;

procedure TDebugDataHandler.ErrorInfoText(ASender: TObject; AErrorInfo: String);
begin
  AddInfo(ASender.ClassName, AErrorInfo, isError);
end;

procedure TDebugDataHandler.ExceptionInfoText(ASender: TObject;
  AExceptionInfo: String);
begin
  AddInfo(ASender.ClassName, AExceptionInfo, isException);
end;

procedure TDebugDataHandler.UpdateDispatchedEventQueue;
var
  intLoop, NumEvents: Integer;
begin
  EnterCriticalSection(gEventDispatchCriticalSection);
  NumEvents := FNumDispatchedEvents;
  LeaveCriticalSection(gEventDispatchCriticalSection);

  // Loop dispatched events
  for intLoop := 0 to NumEvents-1 do
  begin
    AddInfo('EventQueue', Format('%s (@Tick %d)', [
      FDispatchedEventStrings[intLoop], FDispatchedEvents[intLoop].EventTick]),
      isDebug);
  end;

  { TODO: Implement rolling buffer }
  EnterCriticalSection(gEventDispatchCriticalSection);
  if NumEvents = FNumDispatchedEvents then
    FNumDispatchedEvents := 0
  else
    Exception.Create('This should not happen! Implement the f*cking' +
      'rolling buffer!');
  LeaveCriticalSection(gEventDispatchCriticalSection);
end;

procedure EventQueueDispatch(aDispatchedEvent: TPAGE_Event;
  aMessage: PChar);
begin
  EnterCriticalSection(gEventDispatchCriticalSection);
  Inc(gDebugDataHandler.FNumDispatchedEvents);
  gDebugDataHandler.FDispatchedEvents[gDebugDataHandler.
    FNumDispatchedEvents-1] := aDispatchedEvent;
  if (aDispatchedEvent.EventMessage = emString) then
  begin
    { TODO: Make sure that string is actually copied }
    gDebugDataHandler.FDispatchedEventStrings[gDebugDataHandler.
      FNumDispatchedEvents-1] := aMessage;
  end;

  LeaveCriticalSection(gEventDispatchCriticalSection);
end;

Initialization
  gDebugDataHandler := TDebugDataHandler.Create;

Finalization
  gDebugDataHandler.Free;

end.

