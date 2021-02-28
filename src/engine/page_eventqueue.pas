unit PAGE_EventQueue;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef unix} cthreads, {$endif} Classes, SysUtils, PageAPI, SDL2;

const
  MAX_EVENTS = High(Word);

type
  TPAGE_EventQueueListenerDefinition = record
    ListenerDirection: TPAGE_SubSystems;
    case MethodType: Integer of
      0: (ListenerProcedure: TPAGE_EventQueueListenerProcedure);
      1: (ListenerMethod: TPAGE_EventQueueListenerMethod);
  end;

  { TODO: Queue events in WRAM if possible }

  { TPAGE_EventQueue }

  TPAGE_EventQueue = class
  protected
    FEvents: array[0..MAX_EVENTS-1] of TPAGE_Event;
    FNumEvents: Integer;
    FListeners: array of TPAGE_EventQueueListenerDefinition;

    FEventCriticalSection: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure CastEvent(aEvent: TPAGE_Event; aString: PChar = ''); overload;
    procedure CastEventString(aEventType: TPAGE_EventType;
      FromSubSystem, ToSubSystem: TPAGE_SubSystem; theSeverity:
      TPageEventSeverity; theString: PChar); overload;
    procedure CastDebugVariable(aSenderSubSystem: TPAGE_SubSystem;
      aVariableName: PChar; aVariableType: TPageDebugVariableType; aAddress:
      Pointer; aSize: Integer);

    procedure AddEventListener(aEventListener: TPAGE_EventQueueListenerProcedure;
      ListenToSubSystems: TPAGE_SubSystems); overload;
    procedure AddEventListener(aEventListener: TPAGE_EventQueueListenerMethod;
      ListenToSubSystems: TPAGE_SubSystems); overload;



    procedure DoDispatchEvents;
  end;

  { TPageEventQueueListenerClass }

  TPageEventQueueListenerClass = class
  private
    //function GetEvent(Index: Integer): TPAGE_Event;
    //function GetEventCount: Integer;
  protected
    FEventDispatchCriticalSection: TRTLCriticalSection;
    FDispatchedEvents: array[0..MAX_EVENTS-1] of TPAGE_Event;
    FDispatchedEventsTail, FDispatchedEventsHead: Integer;
    FEventCount: Integer;
  public
    //property Events[Index: Integer]: TPAGE_Event read GetEvent;
    //property EventCount: Integer read GetEventCount;

    constructor Create;
    destructor Destroy; override;

    function EventPending: Boolean;
    function PickEvent: TPAGE_Event;

    procedure EventQueueDispatch(aDispatchedEvent: TPAGE_Event);
    procedure RegisterDispatchMethod(SubSystems: TPAGE_SubSystems);
    //procedure Update;
  end;

var
  gEventQueue: TPAGE_EventQueue;



implementation

{ TPageEventQueueListenerClass }

{ function TPageEventQueueListenerClass.GetEvent(Index: Integer): TPAGE_Event;
begin
  if (Index > EventCount-1) or (Index < 0) then
    raise Exception.CreateFmt('Event index out of bounds (%d)', [Index]);

  Result := FDispatchedEvents[(FEventHead+Index)
    mod (High(FDispatchedEvents)+1];
end; }

{ function TPageEventQueueListenerClass.GetEventCount: Integer;
begin
  Result := FEventCount;
end; }

constructor TPageEventQueueListenerClass.Create;
begin
  FDispatchedEventsTail := -1;
  FDispatchedEventsHead := 0;
  InitCriticalSection(FEventDispatchCriticalSection);
end;

destructor TPageEventQueueListenerClass.Destroy;
begin
  DoneCriticalSection(FEventDispatchCriticalSection);
  inherited Destroy;
end;

function TPageEventQueueListenerClass.EventPending: Boolean;
begin
  Result := not ((FDispatchedEventsTail = -1) or
    (FDispatchedEventsTail = FDispatchedEventsHead));
end;

function TPageEventQueueListenerClass.PickEvent: TPAGE_Event;
begin
  Result := EMPTY_EVENT;
  if EventPending then
  begin
    Move(FDispatchedEvents[FDispatchedEventsTail], Result, SizeOf(TPAGE_Event));
    EnterCriticalSection(FEventDispatchCriticalSection);
    FDispatchedEventsTail := (FDispatchedEventsTail+1)
      mod (High(FDispatchedEvents)+1);
    LeaveCriticalSection(FEventDispatchCriticalSection);
  end;
end;

procedure TPageEventQueueListenerClass.EventQueueDispatch(
  aDispatchedEvent: TPAGE_Event);
begin
  if (FDispatchedEventsHead+1) mod (High(FDispatchedEventsTail)+1) =
    FDispatchedEventsTail then
    raise Exception.Create('Event queue overflow');

  EnterCriticalSection(FEventDispatchCriticalSection);
  if FDispatchedEventsTail < 0 then
    FDispatchedEventsTail := 0;

  FDispatchedEvents[FDispatchedEventsHead] := aDispatchedEvent;

  if (aDispatchedEvent.EventMessage = emString) then
  begin
    FDispatchedEvents[FDispatchedEventsHead].EventMessageString :=
      StrNew(aDispatchedEvent.EventMessageString);
  end;

  if (aDispatchedEvent.EventMessage = emDebugInfo) then
  begin
    case aDispatchedEvent.DebugInfoType of
      diString: FDispatchedEvents[FDispatchedEventsHead].DebugString :=
        StrNew(aDispatchedEvent.DebugString);
      diVariable: FDispatchedEvents[FDispatchedEventsHead].DebugVariable.Name :=
        StrNew(aDispatchedEvent.DebugVariable.Name);
    end;
  end;

  FDispatchedEventsHead :=
    (FDispatchedEventsHead + 1) mod (High(FDispatchedEvents)+1);
  LeaveCriticalSection(FEventDispatchCriticalSection);
end;

procedure TPageEventQueueListenerClass.RegisterDispatchMethod(
  SubSystems: TPAGE_SubSystems);
begin
  gEventQueue.AddEventListener(@EventQueueDispatch, SubSystems);
end;

{ procedure TPageEventQueueListenerClass.Update;
var
  intLoop, EventHead, EventTail: Integer;
begin
  // Exit update if no event was added
  if (FDispatchedEventsTail = -1) or
    (FDispatchedEventsTail = FDispatchedEventsHead) then
    Exit;

  EnterCriticalSection(FEventDispatchCriticalSection);
  FCurrentEventHead := FDispatchedEventsHead;
  FCurrentEventTail := FDispatchedEventsTail;
  LeaveCriticalSection(FEventDispatchCriticalSection);

  // Loop dispatched events
  if EventHead > EventTail then
    EventCount := EventHead-EventTail
  else
    EventCount := High(FDispatchedEvents)-EventHead+EventTail;


  { TODO: Dispose strings }
  EnterCriticalSection(FEventDispatchCriticalSection);
  FDispatchedEventsTail := EventHead;
  EventCount := 0;
  LeaveCriticalSection(FEventDispatchCriticalSection);
end; }


{ TPAGE_EventQueue }

constructor TPAGE_EventQueue.Create;
begin
  if Assigned(gEventQueue) then
    Exception.Create('Tried to initialize singleton EventQueue more than once');
  InitCriticalSection(FEventCriticalSection);
  FNumEvents := 0;
  FillByte(FEvents[0], SizeOf(TPAGE_Event)*Length(FEvents), 0);
end;

destructor TPAGE_EventQueue.Destroy;
begin
  SetLength(FListeners, 0);
  DoneCriticalSection(FEventCriticalSection);
end;

procedure TPAGE_EventQueue.CastEvent(aEvent: TPAGE_Event; aString: PChar = '');
begin
  EnterCriticalSection(FEventCriticalSection);
  if FNumEvents = MAX_EVENTS then
    Exception.Create('Event queue overflow');

  Inc(FNumEvents);

  if FEvents[FNumEvents-1].EventMessage = emString then
  begin
    StrDispose(FEvents[FNumEvents-1].EventMessageString);
  end;

  if (FEvents[FNumEvents-1].EventMessage = emDebugInfo) and
    (FEvents[FNumEvents-1].DebugInfoType = diVariable) then
      StrDispose(FEvents[FNumEvents-1].DebugVariable.Name);

  FEvents[FNumEvents-1] := aEvent;
  FEvents[FNumEvents-1].EventTick := SDL_GetTicks;
  if StrLen(aString) > 0 then
  begin
    FEvents[FNumEvents-1].EventMessageString := StrNew(aString);
  end;
  LeaveCriticalSection(FEventCriticalSection);
end;

procedure TPAGE_EventQueue.CastEventString(aEventType: TPAGE_EventType;
  FromSubSystem, ToSubSystem: TPAGE_SubSystem; theSeverity: TPageEventSeverity;
  theString: PChar);
var
  theEvent: TPAGE_Event;
begin
  with theEvent do
  begin
    EventType := aEventType;
    EventSenderSubSystem := FromSubSystem;
    EventReceiverSubSystem := ToSubSystem;
    EventMessage := emString;
    EventSeverity := theSeverity;
  end;
  CastEvent(theEvent, theString);
end;

procedure TPAGE_EventQueue.CastDebugVariable(aSenderSubSystem: TPAGE_SubSystem;
  aVariableName: PChar; aVariableType: TPageDebugVariableType;
  aAddress: Pointer; aSize: Integer);
var
  newEvent: TPAGE_Event;
begin
  newEvent.EventType := etNotification;
  newEvent.EventSenderSubsystem := aSenderSubSystem;
  newEvent.EventReceiverSubsystem := psDebug;
  newEvent.EventMessage := emDebugInfo;
  newEvent.DebugInfoType := diVariable;

  newEvent.DebugVariable.VariableType := aVariableType;
  newEvent.DebugVariable.Name := StrNew(aVariableName);
  newEvent.DebugVariable.Address := aAddress;
  newEvent.DebugVariable.Size := aSize;

  CastEvent(newEvent);
end;

{ THIS METHOD IS NOT THREAD-SAFE! }
procedure TPAGE_EventQueue.AddEventListener(
  aEventListener: TPAGE_EventQueueListenerProcedure;
  ListenToSubSystems: TPAGE_SubSystems);
begin
  { TODO: Maybe make thread safe ?? }
  SetLength(FListeners, Length(FListeners)+1);

  with FListeners[High(FListeners)] do
  begin
    MethodType := 0;
    ListenerProcedure := aEventListener;
    ListenerDirection := ListenToSubSystems;
  end;
end;

procedure TPAGE_EventQueue.AddEventListener(
  aEventListener: TPAGE_EventQueueListenerMethod;
  ListenToSubSystems: TPAGE_SubSystems);
begin
  { TODO: Maybe make thread safe ?? }
  SetLength(FListeners, Length(FListeners)+1);

  with FListeners[High(FListeners)] do
  begin
    MethodType := 1;
    ListenerMethod := aEventListener;
    ListenerDirection := ListenToSubSystems;
  end;
end;

procedure TPAGE_EventQueue.DoDispatchEvents;
var
  intEventLoop, intListenersLoop: Integer;
begin
  if TryEnterCriticalSection(FEventCriticalSection)<>0 then
  begin
    for intEventLoop := 0 to FNumEvents-1 do
    begin
      for intListenersLoop := 0 to High(FListeners) do
      begin
        if (psDebug in FListeners[intListenersLoop].ListenerDirection) or
          (FEvents[intEventLoop].EventReceiverSubsystem in
           FListeners[intListenersLoop].ListenerDirection) then
        begin
          case FListeners[intListenersLoop].MethodType of
            0: FListeners[intListenersLoop].ListenerProcedure(FEvents[intEventLoop]);
            1: FListeners[intListenersLoop].ListenerMethod(FEvents[intEventLoop]);
          end;
        end;
      end;
    end;
    FNumEvents := 0;
    { Strings will be disposed in CastEvent() once the casted event is
      overwritten by a new event }
    LeaveCriticalSection(FEventCriticalSection);
  end;
end;

initialization
  gEventQueue := TPAGE_EventQueue.Create;

finalization
  gEventQueue.Free;

end.

