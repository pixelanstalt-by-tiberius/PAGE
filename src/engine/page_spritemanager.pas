unit page_spritemanager;

{$mode objfpc}{$H+}

interface

uses
  PAGEAPI, SysUtils, page_texturemanager;

type
  TSpriteLookup = record
    Tilemap: Word;
    Offset: Word;
  end;

  { TPageSpriteManager }

  TPageSpriteManager = class
  private
    function GetSprite(SpriteID: Integer): TPageSprite; inline;
    procedure SetSprite(SpriteID: Integer; AValue: TPageSprite); inline;
  protected
    FSpriteStreams: array[0..PAGE_MAX_TILEMAPS-1] of Pointer;
    { TODO: Move LUT to persistence layer (Memory Wrapper - VRAM) }
    FLookupTable: array[0..PAGE_MAX_SPRITES-1] of TSpriteLookup;
    FTextureManager: PPageTextureManager;

    function GetLookupTableIndexBySpriteOffset(aLayer: Integer;
      aOffset: Word): Integer;
  public
    constructor Create;
    procedure AssignStream(aLayer: Integer; aStream: Pointer);
    procedure AssignTextureManager(aTextureManager: PPageTextureManager);
    property Sprites[SpriteID: Integer]: TPageSprite read
      GetSprite write SetSprite;
    procedure DeleteSprite(SpriteID: Integer);
    function AddSprite(aLayer: Integer; aSprite: TPageSprite): Integer;


    function AddSpriteFromInlineResource(aLayer: Integer; aResource: Pointer;
      aResourceSize: Integer; aResourceType: TPageInlineResourceType): Integer;
  end;

const
  EMPTY_LUT: TSpriteLookup = ( Tilemap: 0; Offset: 0 );

operator=(luta: TSpriteLookup; lutb: TSpriteLookup)b: Boolean;

implementation

operator=(luta: TSpriteLookup; lutb: TSpriteLookup)b: Boolean;
begin
  Result := (luta.Tilemap = lutb.Tilemap) and (luta.Offset = lutb.Offset);
end;

{ TPageSpriteManager }

function TPageSpriteManager.GetSprite(SpriteID: Integer): TPageSprite;
begin
  { TODO: Range check }
  Result := TPageSprite((FSpriteStreams[FLookupTable[SpriteID].Tilemap]+
    FLookupTable[SpriteID].Offset)^);
end;

procedure TPageSpriteManager.SetSprite(SpriteID: Integer; AValue: TPageSprite);
begin
  { TODO: Range check }
  Move(AValue, TPageSprite((FSpriteStreams[FLookupTable[SpriteID].Tilemap]+
    FLookupTable[SpriteID].Offset)^), SizeOf(TPageSprite));
end;

function TPageSpriteManager.GetLookupTableIndexBySpriteOffset(aLayer: Integer;
  aOffset: Word): Integer;
var
  intLoop: Integer;
begin
  Result := -1;
  for intLoop := 0 to High(FLookupTable) do
    if (FLookupTable[intLoop].Tilemap = aLayer) and
      (FLookupTable[intLoop].Offset = aOffset) then
    begin
      Result := intLoop;
      Break;
    end;
end;

constructor TPageSpriteManager.Create;
var
  intLoop: Integer;
begin
  for intLoop := 0 to High(FSpriteStreams) do
    FSpriteStreams[intLoop] := nil;
  for intLoop := 0 to High(FLookupTable) do
    FLookupTable[intLoop] := EMPTY_LUT;
  FTextureManager := nil;
end;

procedure TPageSpriteManager.AssignStream(aLayer: Integer; aStream: Pointer);
begin
  { TODO: Check boundaries }
  FSpriteStreams[aLayer] := aStream;
end;

procedure TPageSpriteManager.AssignTextureManager(
  aTextureManager: PPageTextureManager);
begin
  FTextureManager := aTextureManager;
end;

procedure TPageSpriteManager.DeleteSprite(SpriteID: Integer);
var
  ptrSprite: Pointer;
  ptrLastSprite: Pointer;
  LUTEntry: Integer;
begin
  ptrSprite := (FSpriteStreams[FLookupTable[SpriteID].Tilemap]+
    FLookupTable[SpriteID].Offset);
  ptrLastSprite := ptrSprite;
  { TODO: End if exceeded maximum sprites per layer }
  repeat
    Inc(ptrLastSprite, SizeOf(TPageSprite));
  until TPageSprite(ptrLastSprite^) = EMPTY_SPRITE;
  Dec(ptrLastSprite, SizeOf(TPageSprite));
  LUTEntry := GetLookupTableIndexBySpriteOffset(FLookupTable[SpriteID].Tilemap,
    ptrLastSprite-FSpriteStreams[FLookupTable[SpriteID].Tilemap]);
  { TODO: Better error message handling - MessageQueue!}
  if LUTEntry = -1 then
    Exception.Create('Entry not found');
  Move(ptrLastSprite^, ptrSprite^, SizeOf(TPageSprite));
  Move(EMPTY_SPRITE, ptrLastSprite^, SizeOf(TPageSprite));
  FLookupTable[LUTEntry].Offset := ptrSprite-
    FSpriteStreams[FLookupTable[SpriteID].Tilemap];
  FLookupTable[SpriteID].Tilemap := 0;
  FLookupTable[SpriteID].Offset := 0;
end;

function TPageSpriteManager.AddSprite(aLayer: Integer; aSprite: TPageSprite
  ): Integer;
var
  ptrEmpty: Pointer;
  intEmptyLUT: Integer;
begin
  { TODO: Range check }
  ptrEmpty := FSpriteStreams[aLayer];
  { TODO: End if exceeded maximum sprites per layer }
  while TPageSprite(ptrEmpty^) <> EMPTY_SPRITE do
    Inc(ptrEmpty, SizeOf(TPageSprite));
  Move(aSprite, ptrEmpty^, SizeOf(TPageSprite));

  intEmptyLUT := 0;
  { TODO: End if exceeded maximum table entries }
  while FLookupTable[intEmptyLUT] <> EMPTY_LUT do
    Inc(intEmptyLUT);

  if intEmptyLUT > -1 then
  begin
    FLookupTable[intEmptyLUT].Tilemap := aLayer;
    FLookupTable[intEmptyLUT].Offset := ptrEmpty-FSpriteStreams[aLayer];
    Result := intEmptyLUT;
  end;
end;

function TPageSpriteManager.AddSpriteFromInlineResource(aLayer: Integer;
  aResource: Pointer; aResourceSize: Integer;
  aResourceType: TPageInlineResourceType): Integer;
var
  Sprite: TPageSprite;
begin
  Result := -1;
  Sprite := EMPTY_SPRITE;
  Sprite.TextureID := FTextureManager^.AddTextureFromInlineResource(aResource,
    aResourceSize, aResourceType);
  if Sprite.TextureID = -1 then
  begin
    Exit;
  end;
  { TODO: Check boundary of Layer }
  Result := AddSprite(aLayer, Sprite);
end;

end.

