unit DebugDataHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, PAGEAPI, typinfo;

const
  EVENT_QUEUE_SIZE = High(Word);
  INFO_INCREASE_AMOUNT = 1024;

type
  TInfoSeverity = (isDebug, isNormal, isWarning, isError, isException);

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
    FDispatchedEvents: array[0..EVENT_QUEUE_SIZE-1] of TPAGE_Event;
    FDispatchedEventsHead, FDispatchedEventsTail: Integer;

    FInfos: array of TInfo;
    FInfoHead: Integer;



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


procedure EventQueueDispatch(aDispatchedEvent: TPAGE_Event);

var
  gDebugDataHandler: TDebugDataHandler;
  gEventDispatchCriticalSection: TRTLCriticalSection;

implementation

{ TDebugDataHandler }


function TDebugDataHandler.GetInfoCount: Integer;
begin
  Result := FInfoHead+1;
end;

function TDebugDataHandler.GetInfo(Index: Integer): TInfo;
begin
  if (Index > FInfoHead) or (Index < Low(FInfos)) then
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
  if (FInfoHead = -1) or (FInfoHead+1 > Length(FInfos)) then
    SetLength(FInfos, Length(FInfos)+INFO_INCREASE_AMOUNT);

  Inc(FInfoHead);

  with FInfos[FInfoHead] do
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
  FDispatchedEventsHead := 0;
  FDispatchedEventsTail := -1;
  FInfoHead := -1;
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
  intLoop, EventHead, EventTail: Integer;
  strEventType, strVariableType: String;
begin
  // Exit update if no event was added
  if (FDispatchedEventsTail = -1) or
    (FDispatchedEventsTail = FDispatchedEventsHead) then
    Exit;

  EnterCriticalSection(gEventDispatchCriticalSection);
  EventHead := FDispatchedEventsHead;
  EventTail := FDispatchedEventsTail;
  LeaveCriticalSection(gEventDispatchCriticalSection);

  // Loop dispatched events
  if EventHead > EventTail then
    for intLoop := EventTail to EventHead-1 do
    begin
      case FDispatchedEvents[intLoop].EventType of
        etRequest: strEventType := 'Request';
        etNotification: strEventType := 'Notification';
      end;
      case FDispatchedEvents[intLoop].EventMessage of
        emDebugInfo:
          case FDispatchedEvents[intLoop].DebugInfoType of
            diString: AddInfo('EventQueue Debug Info',
                        Format('%s (@Tick %d)', [
                        FDispatchedEvents[intLoop].DebugString,
                        FDispatchedEvents[intLoop].EventTick]), isDebug);
            diVariable: AddInfo('EventQueue Debug Variable',
                          Format('%s: [%s] $%x @%d', [
                          FDispatchedEvents[intLoop].DebugVariable.Name,
                          GetEnumName(TypeInfo(TPageDebugVariableType),
                            Ord(FDispatchedEvents[intLoop].DebugVariable.
                            VariableType)), PtrUInt(FDispatchedEvents[intLoop].
                            DebugVariable.Address), FDispatchedEvents[intLoop].
                            DebugVariable.Size]), isDebug);
          end;

        emString: AddInfo(Format('EventQueue Raw String (%s)', [strEventType]),
          Format('%s (@Tick %d)', [
          FDispatchedEvents[intLoop].EventMessageString,
          FDispatchedEvents[intLoop].EventTick]), isNormal);
      end;
    end
  else
  begin
    // EventHead is before EventTail (ringbuffer looped)
    for intLoop := EventTail to High(FDispatchedEvents) do
    begin
      case FDispatchedEvents[intLoop].EventType of
        etRequest: strEventType := 'Request';
        etNotification: strEventType := 'Notification';
      end;
      if FDispatchedEvents[intLoop].EventMessage = emString then
        AddInfo(Format('EventQueue (%s)', [strEventType]),
          Format('%s (@Tick %d)', [
          FDispatchedEvents[intLoop].EventMessageString,
          FDispatchedEvents[intLoop].EventTick]), isDebug);
    end;

    for intLoop := 0 to EventHead-1 do
    begin
      case FDispatchedEvents[intLoop].EventType of
        etRequest: strEventType := 'Request';
        etNotification: strEventType := 'Notification';
      end;
      if FDispatchedEvents[intLoop].EventMessage = emString then
        AddInfo(Format('EventQueue (%s)', [strEventType]),
          Format('%s (@Tick %d)', [FDispatchedEvents[intLoop].
          EventMessageString, FDispatchedEvents[intLoop].EventTick]), isDebug);
    end;
  end;

  { TODO: Dispose strings }
  EnterCriticalSection(gEventDispatchCriticalSection);
  FDispatchedEventsTail := EventHead;
  LeaveCriticalSection(gEventDispatchCriticalSection);
end;

procedure EventQueueDispatch(aDispatchedEvent: TPAGE_Event);
begin
  if (gDebugDataHandler.FDispatchedEventsHead+1) mod EVENT_QUEUE_SIZE <>
    gDebugDataHandler.FDispatchedEventsTail then
    Exception.Create('Event queue overflow');

  EnterCriticalSection(gEventDispatchCriticalSection);
  if gDebugDataHandler.FDispatchedEventsTail < 0 then
    gDebugDataHandler.FDispatchedEventsTail := 0;

  gDebugDataHandler.FDispatchedEvents[gDebugDataHandler.
    FDispatchedEventsHead] := aDispatchedEvent;

  if (aDispatchedEvent.EventMessage = emString) then
  begin
    gDebugDataHandler.FDispatchedEvents[gDebugDataHandler.
      FDispatchedEventsHead].EventMessageString := StrNew(aDispatchedEvent.
      EventMessageString);
  end;

  if (aDispatchedEvent.EventMessage = emDebugInfo) then
  begin
    case aDispatchedEvent.DebugInfoType of
      diString: gDebugDataHandler.FDispatchedEvents[gDebugDataHandler.
        FDispatchedEventsHead].DebugString := StrNew(aDispatchedEvent.
        DebugString);
      diVariable: gDebugDataHandler.FDispatchedEvents[gDebugDataHandler.
        FDispatchedEventsHead].DebugVariable.Name := StrNew(aDispatchedEvent.
        DebugVariable.Name);
    end;
  end;

  gDebugDataHandler.FDispatchedEventsHead :=
    (gDebugDataHandler.FDispatchedEventsHead + 1) mod EVENT_QUEUE_SIZE;
  LeaveCriticalSection(gEventDispatchCriticalSection);
end;

Initialization
  gDebugDataHandler := TDebugDataHandler.Create;

Finalization
  gDebugDataHandler.Free;

end.

