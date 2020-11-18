unit PAGE_EventQueue;

{$mode objfpc}{$H+}

interface

uses
  cthreads, Classes, SysUtils, PageAPI, SDL2;

const
  MAX_EVENTS = High(Word);

type
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

  FEvents[FNumEvents-1] := aEvent;
  FEvents[FNumEvents-1].EventTick := SDL_GetTicks;
  if StrLen(aString) > 0 then
  begin
    FEvents[FNumEvents-1].EventMessageString := StrNew(aString);
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
          FListeners[intListenersLoop].ListenerMethod(FEvents[intEventLoop]);
        end;
      end;
    end;
    FNumEvents := 0;
    LeaveCriticalSection(FEventCriticalSection);
  end;
end;

initialization
  gEventQueue := TPAGE_EventQueue.Create;

finalization
  gEventQueue.Free;

end.

