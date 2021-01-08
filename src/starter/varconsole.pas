unit varconsole;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  PageAPI, typinfo;

const
  EVENT_QUEUE_SIZE = High(Word);
  INFO_INCREASE_AMOUNT = 1024;

type

  { TfrmVarConsole }

  TfrmVarConsole = class(TForm)
    lvVariableConsole: TListView;
    RefreshEvents: TTimer;
    RefreshVariableValues: TTimer;
    procedure RefreshEventsTimer(Sender: TObject);
    procedure RefreshVariableValuesTimer(Sender: TObject);
  private
    function GetVariable(Index: Integer): TPageDebugVariable;
    function GetVariableCount: Integer;
    procedure SetVariable(Index: Integer; AValue: TPageDebugVariable);
  protected
    FDispatchedEvents: array[0..EVENT_QUEUE_SIZE-1] of TPAGE_Event;
    FDispatchedEventsHead, FDispatchedEventsTail: Integer;
    FVariables: array of TPageDebugVariable;

    procedure CreateVariableList;
  public
    class procedure EventQueueDispatch(aDispatchedEvent: TPAGE_Event); static;

    property VariableCount: Integer read GetVariableCount;
    property Variables[Index: Integer]: TPageDebugVariable read GetVariable
      write SetVariable;
  end;

var
  frmVarConsole: TfrmVarConsole;
  gEventDispatchCriticalSection: TRTLCriticalSection;


implementation

{$R *.lfm}

{ TfrmVarConsole }

procedure TfrmVarConsole.RefreshVariableValuesTimer(Sender: TObject);
var
  intLoop: Integer;
begin
  for intLoop := 0 to lvVariableConsole.Items.Count-1 do
  begin
    with TPageDebugVariable(lvVariableConsole.Items[intLoop].Data^) do
    begin
      case VariableType of
        dvPointer: lvVariableConsole.Items[intLoop].SubItems[3] :=
                     Format('%x', [ptrUInt(Address)]);
        dvInteger: lvVariableConsole.Items[intLoop].SubItems[3] :=
                     Format('%d', [Integer(Address^)]);
        dvByte: lvVariableConsole.Items[intLoop].SubItems[3] :=
                  Format('%d', [Integer(Byte(Address^))]);
        dvWord: lvVariableConsole.Items[intLoop].SubItems[3] :=
                  Format('%d', [Integer(Word(Address^))]);
        dvDWord: lvVariableConsole.Items[intLoop].SubItems[3] :=
                   Format('%d', [Integer(DWord(Address^))]);
        dvInt64: lvVariableConsole.Items[intLoop].SubItems[3] :=
                  Format('%d', [Int64(Address^)]);
        dvUInt64: lvVariableConsole.Items[intLoop].SubItems[3] :=
                  Format('%d', [UInt64(Address^)]);
        dvPChar: lvVariableConsole.Items[intLoop].SubItems[3] :=
                   Format('%s', [PChar(Address^)]);
      end;
    end;
  end;
end;

procedure TfrmVarConsole.RefreshEventsTimer(Sender: TObject);
var
  intLoop, EventHead, EventTail: Integer;
  boolNewVars: Boolean;
begin
  boolNewVars := False;
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
      if (FDispatchedEvents[intLoop].EventMessage = emDebugInfo) and
        (FDispatchedEvents[intLoop].DebugInfoType = diVariable) then
      begin
        boolNewVars := True;
        SetLength(FVariables, Length(FVariables)+1);
        FVariables[High(FVariables)] := FDispatchedEvents[intLoop].
          DebugVariable;
        FVariables[High(FVariables)].Name := StrNew(FDispatchedEvents[intLoop].
          DebugVariable.Name);
      end
    end
  else
  begin
    // EventHead is before EventTail (ringbuffer looped)
    for intLoop := EventTail to High(FDispatchedEvents) do
    begin
      if (FDispatchedEvents[intLoop].EventMessage = emDebugInfo) and
        (FDispatchedEvents[intLoop].DebugInfoType = diVariable) then
      begin
        boolNewVars := True;
        SetLength(FVariables, Length(FVariables)+1);
        FVariables[High(FVariables)] := FDispatchedEvents[intLoop].
          DebugVariable;
        FVariables[High(FVariables)].Name := StrNew(FDispatchedEvents[intLoop].
          DebugVariable.Name);
      end;
    end;

    for intLoop := 0 to EventHead-1 do
    begin
      if (FDispatchedEvents[intLoop].EventMessage = emDebugInfo) and
        (FDispatchedEvents[intLoop].DebugInfoType = diVariable) then
      begin
        boolNewVars := True;
        SetLength(FVariables, Length(FVariables)+1);
        FVariables[High(FVariables)] := FDispatchedEvents[intLoop].
          DebugVariable;
        FVariables[High(FVariables)].Name := StrNew(FDispatchedEvents[intLoop].
          DebugVariable.Name);
      end;
    end;
  end;

  { TODO: Dispose strings }
  EnterCriticalSection(gEventDispatchCriticalSection);
  FDispatchedEventsTail := EventHead;
  LeaveCriticalSection(gEventDispatchCriticalSection);

  if boolNewVars then
    CreateVariableList;
end;

function TfrmVarConsole.GetVariable(Index: Integer): TPageDebugVariable;
begin
  Result := FVariables[Index];
end;

function TfrmVarConsole.GetVariableCount: Integer;
begin
  Result := Length(FVariables);
end;

procedure TfrmVarConsole.SetVariable(Index: Integer; AValue: TPageDebugVariable
  );
begin
  FVariables[Index] := AValue;
end;

procedure TfrmVarConsole.CreateVariableList;
var
  intLoop: Integer;
  LI: TListItem;
begin
  lvVariableConsole.Items.Clear;
  lvVariableConsole.BeginUpdate;
  for intLoop := 0 to High(FVariables) do
  begin
    LI := lvVariableConsole.Items.Add;
    LI.Caption := FVariables[intLoop].Name;
    LI.SubItems.Add(Format('%x', [PtrUInt(FVariables[intLoop].Address)]));
    LI.SubItems.Add(GetEnumName(TypeInfo(TPageDebugVariableType),
      Ord(FVariables[intLoop].VariableType)));
    LI.SubItems.Add(Format('%d', [FVariables[intLoop].Size]));
    LI.SubItems.Add('n/a');
    LI.Data := @FVariables[intLoop];
  end;
  lvVariableConsole.EndUpdate;
end;

class procedure TfrmVarConsole.EventQueueDispatch(aDispatchedEvent: TPAGE_Event
  );
begin
  if (frmVarConsole.FDispatchedEventsHead+1) mod EVENT_QUEUE_SIZE <>
    frmVarConsole.FDispatchedEventsTail then
    Exception.Create('Event queue overflow');

  EnterCriticalSection(gEventDispatchCriticalSection);
  if frmVarConsole.FDispatchedEventsTail < 0 then
    frmVarConsole.FDispatchedEventsTail := 0;

  frmVarConsole.FDispatchedEvents[frmVarConsole.FDispatchedEventsHead] := aDispatchedEvent;

  if (aDispatchedEvent.EventMessage = emString) then
  begin
    frmVarConsole.FDispatchedEvents[frmVarConsole.FDispatchedEventsHead].EventMessageString :=
      StrNew(aDispatchedEvent.EventMessageString);
  end;

  if (aDispatchedEvent.EventMessage = emDebugInfo) then
  begin
    case aDispatchedEvent.DebugInfoType of
      diString: frmVarConsole.FDispatchedEvents[frmVarConsole.FDispatchedEventsHead].DebugString :=
        StrNew(aDispatchedEvent.DebugString);
      diVariable: frmVarConsole.FDispatchedEvents[frmVarConsole.FDispatchedEventsHead].
        DebugVariable.Name := StrNew(aDispatchedEvent.DebugVariable.Name);
    end;
  end;

  frmVarConsole.FDispatchedEventsHead := (frmVarConsole.FDispatchedEventsHead + 1) mod EVENT_QUEUE_SIZE;
  LeaveCriticalSection(gEventDispatchCriticalSection);
end;

end.

