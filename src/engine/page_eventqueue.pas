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



  { TPAGE_EventQueue }

  TPAGE_EventQueue = class
  protected
    FEvents: array[0..MAX_EVENTS] of TPAGE_Event;
    FEventStrings: array[0..MAX_EVENTS] of PChar;
    FNumEvents: Integer;
    // Maybe a better name would be FListenerCallbacks
    FListenersEvents: array of TPAGE_EventQueueListener;
    FListenersDirection: array of TPAGE_SubSystems;

    FEventCriticalSection: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure CastEvent(aEvent: TPAGE_Event; aString: PChar = ''); overload;
    procedure CastEventString(aEventType: TPAGE_EventType;
      FromSubSystem, ToSubSystem: TPAGE_SubSystem; theString: PChar); overload;

    function GetEventString(Number: Word): PChar;

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
  SetLength(FListenersEvents, 0);
  SetLength(FListenersDirection, 0);
  DoneCriticalSection(FEventCriticalSection);
end;

procedure TPAGE_EventQueue.CastEvent(aEvent: TPAGE_Event; aString: PChar = '');
begin
  EnterCriticalSection(FEventCriticalSection);
  if FNumEvents < MAX_EVENTS then
    Inc(FNumEvents);
  { TODO: Exception }
  FEvents[FNumEvents-1] := aEvent;
  FEvents[FNumEvents-1].EventTick := SDL_GetTicks;
  if Length(aString) <> 0 then
  begin
    StrDispose(FEventStrings[FNumEvents-1]);
    FEventStrings[FNumEvents-1] := StrAlloc(StrLen(aString)+1);
    StrCopy(FEventStrings[FNumEvents-1], aString);
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

function TPAGE_EventQueue.GetEventString(Number: Word): PChar;
begin
  Result := FEventStrings[Number];
end;


procedure TPAGE_EventQueue.AddEventListener(
  aEventListener: TPAGE_EventQueueListener; ListenToSubSystems: TPAGE_SubSystems
  );
begin
  { TODO: Maybe make thread safe ?? }

  SetLength(FListenersEvents, Length(FListenersEvents)+1);
  SetLength(FListenersDirection, Length(FListenersEvents));

  { TODO: Maybe put a FNumListeners here to be more performant }
  { TODO: Maybe join ListenersEvents and Directions in one record }
  FListenersEvents[High(FListenersEvents)] := aEventListener;
  FListenersDirection[High(FListenersEvents)] := ListenToSubSystems;
end;

procedure TPAGE_EventQueue.DoDispatchEvents;
var
  intEventLoop, intListenerEventsLoop: Integer;
begin
  EnterCriticalSection(FEventCriticalSection);
  for intEventLoop := 0 to FNumEvents-1 do
    for intListenerEventsLoop := 0 to High(FListenersEvents) do
      if (psDebug in FListenersDirection[intListenerEventsLoop]) or
        (FEvents[intEventLoop].EventReceiverSubsystem in
         FListenersDirection[intListenerEventsLoop]) then
      begin
        FListenersEvents[intListenerEventsLoop](FEvents[intEventLoop],
          FEventStrings[intEventLoop]);
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

