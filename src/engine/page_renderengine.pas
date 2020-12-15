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
    function GetPerTileRenderingStatus: Boolean;
    function GetSpriteHeight: Word;
    function GetSpriteStream(Index: Integer): Pointer;
    function GetSpriteWidth: Word;
    function GetTileHeight: Word;
    function GetTileMap(Index: Integer): TPageTileMap;
    function GetTilemapCount: Integer;
    function GetTilemapOffset(Index: Integer): TPageCoordinate2D;
    function GetTileWidth: Word;
    procedure SetPerTileRenderingStatus(AValue: Boolean);
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
    FTilemapCount: Integer;
    FSpriteStreams: array[0..PAGE_MAX_TILEMAPS-1] of Pointer;
    FRenderer: PSDL_Renderer;
    FTextureManager: PPageTextureManager;
    FRenderEngineInfo: PPageRenderEngineInfo;
    FMemoryManagerInterface: IPageMemoryManager;

    procedure DoRenderPerTile;
    procedure DoRenderPerLayer;

    procedure DoDrawTexture(TextureID: TPageTextureID; X, Y: Word; FlipH,
      FlipV: Boolean; Alpha: Byte = 255);
  public
    constructor Create(RenderEngineInfo: Pointer; aMemoryManagerInterface:
      IPageMemoryManager; aRenderer: PSDL_Renderer);

    procedure DoRender;
    procedure SetTilemap(Number: Word; TileMap: PPageTileMap);
    procedure SetSpriteStreamPointer(Number: Word;
      SpriteStreamPointer: Pointer);

    property SpriteStreams[Index: Integer]: Pointer read GetSpriteStream
      write SetSpriteStream;
    property Tilemaps[Index: Integer]: TPageTileMap read GetTileMap;
    property TilemapCount: Integer read GetTilemapCount write SetTilemapCount;
    property PerTileRendering: Boolean read GetPerTileRenderingStatus
      write SetPerTileRenderingStatus;
    property TilemapOffsets[Index: Integer]: TPageCoordinate2D
      read GetTilemapOffset write SetTilemapOffset;
    property SpriteHeight: Word read GetSpriteHeight write SetSpriteHeight;
    property SpriteWidth: Word read GetSpriteWidth write SetSpriteWidth;
    property TileHeight: Word read GetTileHeight write SetTileHeight;
    property TileWidth: Word read GetTileWidth write SetTileWidth;
  end;

implementation

{ TPageRenderEngine }

function TPageRenderEngine.GetPerTileRenderingStatus: Boolean;
begin
  Result := FRenderEngineInfo^.PerTileRenderingEnabled;
end;

function TPageRenderEngine.GetSpriteHeight: Word;
begin
  Result := FRenderEngineInfo^.SpriteDimension.Y;
end;

function TPageRenderEngine.GetSpriteStream(Index: Integer): Pointer;
begin
  { TODO: Range check }
  Result := FSpriteStreams[Index];
end;

function TPageRenderEngine.GetSpriteWidth: Word;
begin
  Result := FRenderEngineInfo^.SpriteDimension.X;
end;

function TPageRenderEngine.GetTileHeight: Word;
begin
  Result := FRenderEngineInfo^.TileDimension.Y;
end;

function TPageRenderEngine.GetTileMap(Index: Integer): TPageTileMap;
begin
  { TODO: Range check }
  Result := FTilemaps[Index]^;
end;

function TPageRenderEngine.GetTilemapCount: Integer;
begin
  Result := FRenderEngineInfo^.TilemapCount;
end;

function TPageRenderEngine.GetTilemapOffset(Index: Integer): TPageCoordinate2D;
begin
  { TODO: Range check }
  Result := FTilemapOffsets[Index];
end;

function TPageRenderEngine.GetTileWidth: Word;
begin
  Result := FRenderEngineInfo^.TileDimension.X;
end;

procedure TPageRenderEngine.SetPerTileRenderingStatus(AValue: Boolean);
begin
  FRenderEngineInfo^.PerTileRenderingEnabled := AValue;
end;


procedure TPageRenderEngine.SetSpriteHeight(AValue: Word);
begin
  FRenderEngineInfo^.SpriteDimension.Y := AValue;
end;

procedure TPageRenderEngine.SetSpriteStream(Index: Integer; AValue: Pointer);
begin
  { TODO: Range check }
  { TODO: Free memory if slot was used before }
  FSpriteStreams[Index] := AValue;
end;

procedure TPageRenderEngine.SetSpriteWidth(AValue: Word);
begin
  FRenderEngineInfo^.SpriteDimension.X := AValue;
end;


procedure TPageRenderEngine.SetTileHeight(AValue: Word);
begin
  FRenderEngineInfo^.TileDimension.Y := AValue;
end;

procedure TPageRenderEngine.SetTilemapCount(AValue: Integer);
begin
  FRenderEngineInfo^.TilemapCount := AValue;
end;

procedure TPageRenderEngine.SetTilemapOffset(Index: Integer;
  AValue: TPageCoordinate2D);
begin
  { TODO: Range check }
  FTilemapOffsets[Index] := AValue;
end;

procedure TPageRenderEngine.SetTileWidth(AValue: Word);
begin
  FRenderEngineInfo^.TileDimension.X := AValue;
end;

procedure TPageRenderEngine.DoRenderPerTile;
var
  intTilemaps, intSprites, intX, intY: Integer;
begin
  for intTilemaps := 0 to FTilemapCount-1 do
  begin
    for intX := 0 to FTilemaps[intTilemaps]^.Width-1 do
      for intY := 0 to FTilemaps[intTilemaps]^.Height-1 do
      begin
        DoDrawTexture(FTilemaps[intTilemaps]^.Map[intX, intY].TextureID,
          TileWidth*intX+FTilemapOffsets[intTilemaps].X,
          TileHeight*intY+FTilemapOffsets[intTilemaps].Y,
          tfFlipH in FTilemaps[intTilemaps]^.Map[intX, intY].
          Flags, tfFlipV in FTilemaps[intTilemaps]^.Map[intX, intY].
          Flags);
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

procedure TPageRenderEngine.DoRenderPerLayer;
begin
  Exception.Create('Not implemented yet');
end;

procedure TPageRenderEngine.DoDrawTexture(TextureID: TPageTextureID; X,
  Y: Word; FlipH, FlipV: Boolean; Alpha: Byte);
var
  FlipFlags: Integer = 0;
  DestRect: TSDL_Rect;
begin
  if FlipH then
    FlipFlags := FlipFlags or SDL_FLIP_HORIZONTAL;
  if FlipV then
    FlipFlags := FlipFlags or SDL_FLIP_VERTICAL;

  { TODO: Change to dest in params or do two independent functions for tiles
          and sprites (latter may be better due to alpha- and blending mods
          should only be available for sprites }
  DestRect.h := SpriteHeight;
  DestRect.w := SpriteWidth;
  DestRect.x := X;
  DestRect.y := Y;

  { TODO: Maybe manipulate textures once out of rendering functions }
  SDL_SetTextureAlphaMod(FTextureManager^.Textures[TextureID].TexturePointer,
    Alpha);
  SDL_RenderCopyEx(FRenderer, FTextureManager^.Textures[TextureID].
    TexturePointer, nil, @DestRect, 0, nil, FlipFlags);
end;

constructor TPageRenderEngine.Create(RenderEngineInfo: Pointer;
  aMemoryManagerInterface: IPageMemoryManager; aRenderer: PSDL_Renderer);
var
  intLoop: Integer;
begin
  FRenderEngineInfo := RenderEngineInfo;
  FRenderer := aRenderer;
  FMemoryManagerInterface := aMemoryManagerInterface;
  TilemapCount := 0;
  for intLoop := 0 to PAGE_MAX_TILEMAPS-1 do
  begin
    FTilemaps[intLoop] := nil;
    FTilemapOffsets[intLoop] := PAGE_COORDINATE2D_NULL;
    FSpriteStreams[intLoop] := nil;
  end;
end;

procedure TPageRenderEngine.DoRender;
begin
  if PerTileRendering then
    DoRenderPerTile
  else
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

