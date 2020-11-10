unit PAGE_EventQueue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PageAPI, SDL2;//, Contnrs;

const
  MAX_EVENTS = High(Word);

type
  //TPAGE_EventQueue = class;

  { TPAGE_EventQueueThread = class(TThread)
  protected
    FEventQueue: TPAGE_EventQueue;
  public
    procedure Execute; override;
  end; }

  TPAGE_EventQueueListenerDefinition = record
    ListenerMethod: TPAGE_EventQueueListener;
    ListenerDirection: TPAGE_SubSystems;
  end;

  { TPAGE_EventQueue }

  TPAGE_EventQueue = class
  protected
    FEvents: array[0..MAX_EVENTS] of TPAGE_Event;
    FNumEvents: Integer;
    FListeners: array of TPAGE_EventQueueListenerDefinition;

    FEventCriticalSection: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure CastEvent(aEvent: TPAGE_Event; aString: PChar = ''); overload;
    procedure CastEventString(aEventType: TPAGE_EventType;
      FromSubSystem, ToSubSystem: TPAGE_SubSystem; theString: PChar); overload;

    procedure AddEventListener(aEventListener: TPAGE_EventQueueListener;
      ListenToSubSystems: TPAGE_SubSystems);

    procedure DoDispatchEvents;
  end;

var
  gEventQueue: TPAGE_EventQueue;



implementation


{ TPAGE_EventQueue }

constructor TPAGE_EventQueue.Create;
begin
  if Assigned(gEventQueue) then
    Exception.Create('Tried to initialize singleton EventQueue more than once');
  InitCriticalSection(FEventCriticalSection);
  FNumEvents := 0;
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

  FEvents[FNumEvents-1] := aEvent;
  FEvents[FNumEvents-1].EventTick := SDL_GetTicks;
  if Length(aString) <> 0 then
  begin
    StrDispose(FEvents[FNumEvents-1].EventMessageString);
    FEvents[FNumEvents-1].EventMessageString := StrAlloc(StrLen(aString)+1);
    StrCopy(FEvents[FNumEvents-1].EventMessageString, aString);
  end;
  LeaveCriticalSection(FEventCriticalSection);
end;

procedure TPAGE_EventQueue.CastEventString(aEventType: TPAGE_EventType;
  FromSubSystem, ToSubSystem: TPAGE_SubSystem; theString: PChar);
var
  theEvent: TPAGE_Event;
begin
  with theEvent do
  begin
    EventType := aEventType;
    EventSenderSubSystem := FromSubSystem;
    EventReceiverSubSystem := ToSubSystem;
    EventMessage := emString;
  end;
  CastEvent(theEvent, theString);
end;

{ THIS METHOD IS NOT THREAD-SAFE! }
procedure TPAGE_EventQueue.AddEventListener(
  aEventListener: TPAGE_EventQueueListener; ListenToSubSystems: TPAGE_SubSystems
  );
begin
  { TODO: Maybe make thread safe ?? }
  SetLength(FListeners, Length(FListeners)+1);

  { DONE: Maybe join ListenersEvents and Directions in one record }
  with FListeners[High(FListeners)] do
  begin
    ListenerMethod := aEventListener;
    ListenerDirection := ListenToSubSystems;
  end;
end;

procedure TPAGE_EventQueue.DoDispatchEvents;
var
  intEventLoop, intListenersLoop: Integer;
begin
  EnterCriticalSection(FEventCriticalSection);
  for intEventLoop := 0 to FNumEvents-1 do
    for intListenersLoop := 0 to High(FListeners) do
      if (psDebug in FListeners[intListenersLoop].ListenerDirection) or
        (FEvents[intEventLoop].EventReceiverSubsystem in
         FListeners[intListenersLoop].ListenerDirection) then
      begin
        FListeners[intListenersLoop].ListenerMethod(FEvents[intEventLoop]);
      end;
  FNumEvents := 0;
  LeaveCriticalSection(FEventCriticalSection);
end;

initialization
  //gEventQueue := TPAGE_EventQueueThread.Create(True);
  gEventQueue := TPAGE_EventQueue.Create;

finalization
  gEventQueue.Free;

end.

