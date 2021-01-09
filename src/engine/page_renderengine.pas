unit page_renderengine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, page_texturemanager, page_tilemanager, PageAPI, SDL2;

type

  { TPageRenderEngine }

  { TODO: Add transparency/fading effects to whole layers }

  TPageRenderEngine = class
  private
    function GetSpriteHeight: Word;
    function GetSpriteStream(Index: Integer): Pointer;
    function GetSpriteWidth: Word;
    function GetTileHeight: Word;
    function GetTileMap(Index: Integer): TPageTileMap;
    function GetTilemapCount: Integer;
    function GetTilemapOffset(Index: Integer): TPageCoordinate2D;
    function GetTileWidth: Word;
    procedure SetSpriteHeight(AValue: Word);
    procedure SetSpriteStream(Index: Integer; AValue: Pointer);
    procedure SetSpriteWidth(AValue: Word);
    procedure SetTileHeight(AValue: Word);
    procedure SetTilemapCount(AValue: Integer);
    procedure SetTilemapOffset(Index: Integer; AValue: TPageCoordinate2D);
    procedure SetTileWidth(AValue: Word);
  protected
    FTilemaps: array[0..PAGE_MAX_TILEMAPS-1] of PPageTileMap;
    FTilemapOffsets: array[0..PAGE_MAX_TILEMAPS-1] of TPageCoordinate2D;
    FSpriteStreams: array[0..PAGE_MAX_TILEMAPS-1] of Pointer;
    FRenderedBackgrounds: array[0..PAGE_MAX_TILEMAPS-1] of Pointer;
    FTextureManager: PPageTextureManager;
    FMemoryWrapper: IPageMemoryWrapper;

    // procedure DoRenderPerTile;
    procedure DoRenderPerLayer;

    procedure InitializeRenderedBackgroundsTextures;

    procedure DoDrawTileTexture(TextureID: TPageTextureID; X, Y: Word; FlipH,
      FlipV: Boolean);
    procedure DoRenderTilemapToTexture(Tilemap: PPageTilemap;
      Texture: PSDL_Texture);
    procedure DoRenderSprite(Sprite: TPageSprite);
  public
    constructor Create(aMemoryWrapper: IPageMemoryWrapper; aTextureManager:
      PPageTextureManager);

    procedure DoRender;
    procedure SetTilemap(Number: Word; TileMap: PPageTileMap);
    procedure SetSpriteStreamPointer(Number: Word;
      SpriteStreamPointer: Pointer);

    property SpriteStreams[Index: Integer]: Pointer read GetSpriteStream
      write SetSpriteStream;
    property Tilemaps[Index: Integer]: TPageTileMap read GetTileMap;
    property TilemapCount: Integer read GetTilemapCount write SetTilemapCount;
    property TilemapOffsets[Index: Integer]: TPageCoordinate2D
      read GetTilemapOffset write SetTilemapOffset;
    property SpriteHeight: Word read GetSpriteHeight write SetSpriteHeight;
    property SpriteWidth: Word read GetSpriteWidth write SetSpriteWidth;
    property TileHeight: Word read GetTileHeight write SetTileHeight;
    property TileWidth: Word read GetTileWidth write SetTileWidth;
  end;

implementation

{ TPageRenderEngine }

function TPageRenderEngine.GetSpriteHeight: Word;
begin
  Result := FMemoryWrapper.RenderEngineInfo.SpriteDimension.Y;
end;

function TPageRenderEngine.GetSpriteStream(Index: Integer): Pointer;
begin
  { TODO: Range check }
  Result := FSpriteStreams[Index];
end;

function TPageRenderEngine.GetSpriteWidth: Word;
begin
  Result := FMemoryWrapper.RenderEngineInfo.SpriteDimension.X;
end;

function TPageRenderEngine.GetTileHeight: Word;
begin
  Result := FMemoryWrapper.RenderEngineInfo.TileDimension.Y;
end;

function TPageRenderEngine.GetTileMap(Index: Integer): TPageTileMap;
begin
  { TODO: Range check }
  Result := FTilemaps[Index]^;
end;

function TPageRenderEngine.GetTilemapCount: Integer;
begin
  Result := FMemoryWrapper.RenderEngineInfo.TilemapCount;
end;

function TPageRenderEngine.GetTilemapOffset(Index: Integer): TPageCoordinate2D;
begin
  { TODO: Range check }
  Result := FTilemapOffsets[Index];
end;

function TPageRenderEngine.GetTileWidth: Word;
begin
  Result := FMemoryWrapper.RenderEngineInfo.TileDimension.X;
end;

procedure TPageRenderEngine.SetSpriteHeight(AValue: Word);
begin
  TPageRenderEngineInfo(FMemoryWrapper.RenderEngineInfoPointer^).
    SpriteDimension.Y := AValue;
end;

procedure TPageRenderEngine.SetSpriteStream(Index: Integer; AValue: Pointer);
begin
  { TODO: Range check }
  { TODO: Free memory if slot was used before }
  FSpriteStreams[Index] := AValue;
end;

procedure TPageRenderEngine.SetSpriteWidth(AValue: Word);
begin
  TPageRenderEngineInfo(FMemoryWrapper.RenderEngineInfoPointer^).
    SpriteDimension.X := AValue;
end;


procedure TPageRenderEngine.SetTileHeight(AValue: Word);
begin
  TPageRenderEngineInfo(FMemoryWrapper.RenderEngineInfoPointer^).
    TileDimension.Y := AValue;
end;

procedure TPageRenderEngine.SetTilemapCount(AValue: Integer);
begin
  if (AValue < 0) then
    TPageRenderEngineInfo(FMemoryWrapper.RenderEngineInfoPointer^).
      TilemapCount := 0
  else
    if (AValue <= PAGE_MAX_TILEMAPS) then
      TPageRenderEngineInfo(FMemoryWrapper.RenderEngineInfoPointer^).
        TilemapCount := AValue
    else
      TPageRenderEngineInfo(FMemoryWrapper.RenderEngineInfoPointer^).
        TilemapCount := PAGE_MAX_TILEMAPS;
end;

procedure TPageRenderEngine.SetTilemapOffset(Index: Integer;
  AValue: TPageCoordinate2D);
begin
  { TODO: Range check }
  FTilemapOffsets[Index] := AValue;
end;

procedure TPageRenderEngine.SetTileWidth(AValue: Word);
begin
  TPageRenderEngineInfo(FMemoryWrapper.RenderEngineInfoPointer^).
    TileDimension.X := AValue;
end;

{
procedure TPageRenderEngine.DoRenderPerTile;
var
  intTilemaps, intSprites, intX, intY: Integer;
begin
  for intTilemaps := 0 to TilemapCount-1 do
  begin
    for intX := 0 to FTilemaps[intTilemaps]^.Width-1 do
    begin
      for intY := 0 to FTilemaps[intTilemaps]^.Height-1 do
      begin
        DoDrawTexture(FTilemaps[intTilemaps]^.Map[intX, intY].TextureID,
          TileWidth*intX+FTilemapOffsets[intTilemaps].X,
          TileHeight*intY+FTilemapOffsets[intTilemaps].Y,
          tfFlipH in FTilemaps[intTilemaps]^.Map[intX, intY].
          Flags, tfFlipV in FTilemaps[intTilemaps]^.Map[intX, intY].
          Flags);
      end;
    end;

    // Draw high priority tiles of the prior layer on top
    if intTilemaps >= 1 then
      for intX := 0 to FTilemaps[intTilemaps-1]^.Width-1 do
        for intY := 0 to FTilemaps[intTilemaps-1]^.Height-1 do
        begin
          if tfPriorityHigh in FTilemaps[intTilemaps-1]^.Map[intX, intY].
            Flags then
            DoDrawTexture(FTilemaps[intTilemaps-1]^.Map[intX, intY].TextureID,
              TileWidth*intX+FTilemapOffsets[intTilemaps-1].X,
              TileHeight*intY+FTilemapOffsets[intTilemaps-1].Y,
              tfFlipH in FTilemaps[intTilemaps-1]^.Map[intX, intY].
              Flags, tfFlipV in FTilemaps[intTilemaps-1]^.Map[intX, intY].
              Flags);
        end;

    for intSprites := 0 to SPRITE_COUNT-1 do
      if TPageSprite((FSpriteStreams[intTilemaps]+
        intSprites*SizeOf(TPageSprite))^) <> EMPTY_SPRITE then
      begin
        with TPageSprite((FSpriteStreams[intTilemaps]+
          intSprites*SizeOf(TPageSprite))^) do
        begin
          if sfEnableAlpha in Flags then
            DoDrawTexture(TextureID, X, Y, (sfFlipH in Flags), (sfFlipV in
              Flags), Alpha)
          else
            DoDrawTexture(TextureID, X, Y, (sfFlipH in Flags), (sfFlipV in
              Flags));
        end;
      end;
  end;
end;
}

procedure TPageRenderEngine.DoRenderPerLayer;
var
  intTilemapLoop, intOffset: Integer;
  SrcRect: TSDL_Rect;
begin
  SrcRect.w := TPageRenderEngineInfo(FMemoryWrapper.RenderEngineInfoPointer^).
    RenderingDimension.X;
  SrcRect.h := TPageRenderEngineInfo(FMemoryWrapper.RenderEngineInfoPointer^).
    RenderingDimension.Y;

  for intTilemapLoop := 0 to TilemapCount-1 do
  begin
    if not FTilemaps[intTilemapLoop]^.isValid then
    begin
      DoRenderTilemapToTexture(FTilemaps[intTilemapLoop],
        FRenderedBackgrounds[intTilemapLoop]);
      FTilemaps[intTilemapLoop]^.Validate;
    end;

    SrcRect.x := FTilemapOffsets[intTilemapLoop].X;
    SrcRect.y := FTilemapOffsets[intTilemapLoop].Y;

    SDL_RenderCopyEx(FMemoryWrapper.SDLRenderer,
      FRenderedBackgrounds[intTilemapLoop], @SrcRect, nil, 0, nil, 0);

    intOffset := 0;
    while (intOffset < SizeOf(TPageSprite)*(SPRITE_COUNT-1)) and
       (TPageSprite((FSpriteStreams[intTilemapLoop]+intOffset)^) <>
        EMPTY_SPRITE) do
    begin
      DoRenderSprite(TPageSprite((FSpriteStreams[intTilemapLoop]+intOffset)^));
      Inc(intOffset, SizeOf(TPageSprite));
    end;
  end;
end;

procedure TPageRenderEngine.InitializeRenderedBackgroundsTextures;
var
  intLoop: Integer;
begin
  for intLoop := 0 to PAGE_MAX_TILEMAPS-1 do
    FRenderedBackgrounds[intLoop] := SDL_CreateTexture(FMemoryWrapper.
      SDLRenderer, 0, SDL_TEXTUREACCESS_TARGET,
      TPageRenderEngineInfo(FMemoryWrapper.
      RenderEngineInfoPointer^).CanvasDimension.X,
      TPageRenderEngineInfo(FMemoryWrapper.RenderEngineInfoPointer^).
      CanvasDimension.Y);
end;

procedure TPageRenderEngine.DoDrawTileTexture(TextureID: TPageTextureID; X,
  Y: Word; FlipH, FlipV: Boolean);
var
  FlipFlags: Integer = 0;
  DestRect: TSDL_Rect;
begin
  if FlipH then
    FlipFlags := FlipFlags or SDL_FLIP_HORIZONTAL;
  if FlipV then
    FlipFlags := FlipFlags or SDL_FLIP_VERTICAL;

  DestRect.h := TileHeight;
  DestRect.w := TileHeight;
  DestRect.x := X;
  DestRect.y := Y;

  SDL_RenderCopyEx(FMemoryWrapper.SDLRenderer, FTextureManager^.
    Textures[TextureID].TexturePointer, nil, @DestRect, 0, nil, FlipFlags);
end;

procedure TPageRenderEngine.DoRenderTilemapToTexture(Tilemap: PPageTilemap;
  Texture: PSDL_Texture);
var
  OldRenderTarget: PSDL_Texture;
  intX, intY: Integer;
begin
  OldRenderTarget := SDL_GetRenderTarget(FMemoryWrapper.SDLRenderer);
  if SDL_SetRenderTarget(FMemoryWrapper.SDLRenderer, Texture) <> 0 then
    Exception.Create('Failed to change rendering target');
  SDL_RenderClear(FMemoryWrapper.SDLRenderer);
  for intX := 0 to Tilemap^.Width-1 do
  begin
    for intY := 0 to Tilemap^.Height-1 do
    begin
      if Tilemap^.Map[intX, intY].TextureID < 0 then
        Continue;
      DoDrawTileTexture(Tilemap^.Map[intX, intY].TextureID,
        TileWidth*intX, TileHeight*intY,
        tfFlipH in Tilemap^.Map[intX, intY].Flags, tfFlipV in
        Tilemap^.Map[intX, intY].Flags);
    end;
  end;
  SDL_RenderPresent(FMemoryWrapper.SDLRenderer);
  SDL_SetRenderTarget(FMemoryWrapper.SDLRenderer, OldRenderTarget);
end;

procedure TPageRenderEngine.DoRenderSprite(Sprite: TPageSprite);
var
  DestRect: TSDL_Rect;
  FlipFlags: Integer = 0;
  Alpha: Byte = 255;
begin
  if sfFlipH in Sprite.Flags then
    FlipFlags := FlipFlags or SDL_FLIP_HORIZONTAL;
  if sfFlipV in Sprite.Flags then
    FlipFlags := FlipFlags or SDL_FLIP_VERTICAL;
  if sfEnableAlpha in Sprite.Flags then
    Alpha := Sprite.Alpha;

  DestRect.h := SpriteHeight;
  DestRect.w := SpriteWidth;
  DestRect.x := Sprite.X;
  DestRect.y := Sprite.Y;

  SDL_SetTextureAlphaMod(FTextureManager^.Textures[Sprite.TextureID].
    TexturePointer, Alpha);
  SDL_RenderCopyEx(FMemoryWrapper.SDLRenderer, FTextureManager^.
    Textures[Sprite.TextureID].TexturePointer, nil, @DestRect, 0, nil,
    FlipFlags);
end;

constructor TPageRenderEngine.Create(aMemoryWrapper: IPageMemoryWrapper;
  aTextureManager: PPageTextureManager);
var
  intLoop: Integer;
begin
  FMemoryWrapper := aMemoryWrapper;
  FTextureManager := aTextureManager;
  TilemapCount := 0;
  for intLoop := 0 to PAGE_MAX_TILEMAPS-1 do
  begin
    FTilemaps[intLoop] := nil;
    FTilemapOffsets[intLoop] := PAGE_COORDINATE2D_NULL;
    FSpriteStreams[intLoop] := nil;
    FRenderedBackgrounds[intLoop] := nil;
  end;
end;

procedure TPageRenderEngine.DoRender;
begin
  DoRenderPerLayer;
end;

procedure TPageRenderEngine.SetTilemap(Number: Word; TileMap: PPageTileMap);
begin
  { TODO: Range check }
  { TODO: Free memory if slot was used before }
  FTilemaps[Number] := TileMap;
end;

procedure TPageRenderEngine.SetSpriteStreamPointer(Number: Word;
  SpriteStreamPointer: Pointer);
begin
  { TODO: Range check }
  { TODO: Free memory if slot was used before }
  FSpriteStreams[Number] := SpriteStreamPointer;
end;

end.

