unit page_soundmanager;

{$mode objfpc}{$H+}

interface

uses
  SDL2, SDL2_Mixer, page_eventqueue, PageAPI;

type

  { TPageSoundManager }

  TPageSoundManager = class
  protected
    FEventQueueListener: TPageEventQueueListenerClass;
    FMixChunk: PMix_Chunk;

    procedure PlaySoundFromInlineResource(aMemory: Pointer; aSize: ptrUint);
    procedure PlaySoundFromFile(aFile: PChar);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Initialize;
    procedure PlaySound(aResource: TPageSoundResource);

    procedure Update;
  end;

implementation

{ TPageSoundManager }

procedure TPageSoundManager.PlaySound(aResource: TPageSoundResource);
begin
  case aResource.ResourceType of
    rtInline: PlaySoundFromInlineResource(aResource.Memory, aResource.Size);
    rtFile: PlaySoundFromFile(aResource.Filename);
  end;
end;

procedure TPageSoundManager.PlaySoundFromInlineResource(aMemory: Pointer;
  aSize: ptrUint);
var
  rwop: PSDL_RWops;
begin
  rwop := SDL_RWFromConstMem(aMemory, aSize);
  FMixChunk := Mix_LoadWAV_RW(rwop, -1);
  Mix_PlayChannel(-1, FMixChunk, 0);
end;

procedure TPageSoundManager.PlaySoundFromFile(aFile: PChar);
begin
  FMixChunk := Mix_LoadWAV(aFile);
  Mix_PlayChannel(-1, FMixChunk, 0);
end;

constructor TPageSoundManager.Create;
begin
  FEventQueueListener := TPageEventQueueListenerClass.Create;
  //gEventQueue.AddEventListener(TPAGE_EventQueueListener(TMethod(
  //  @(FEventQueueListener.EventQueueDispatch)).Data), [psAudio]);
  FEventQueueListener.RegisterDispatchMethod([psAudio]);
  {if Mix_Init(MIX_INIT_FLAC or MIX_INIT_MOD or MIX_INIT_MODPLUG or MIX_INIT_MP3
    or MIX_INIT_OGG or MIX_INIT_FLUIDSYNTH) = 0 then
    FMixChunk := nil;}
end;

destructor TPageSoundManager.Destroy;
begin
  FEventQueueListener.Destroy;
  inherited Destroy;
end;

procedure TPageSoundManager.Initialize;
begin
  { TODO: Initialization parameters needed }
  Mix_OpenAudio(44100, MIX_DEFAULT_FORMAT, 2, 4096);
end;

procedure TPageSoundManager.Update;
var
  Event: TPAGE_Event;
begin
  while FEventQueueListener.EventPending do
  begin
    Event := FEventQueueListener.PickEvent;
    if Event.EventType = etRequest then
    begin
      case Event.EventMessage of
        emPlaySound: PlaySound(Event.SoundResource);
      end;
    end;
  end;
end;

end.

