unit page_tilemanager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PageAPI; //, page_texturemanager;

type
  { TPageTileMap }

  TPageTileMap = class
  private
    function GetMapHeight: Integer;
    function GetMapTileRecord(X, Y: Integer): TPageTileRecord;
    function GetMapWidth: Integer;
    procedure SetMapTileRecord(X, Y: Integer; AValue: TPageTileRecord);
  protected
    FMemoryManagerInterface: IPageMemoryManager;
    { FTileMap: PPageTileRecordArray;
    FWidth, FHeight: PInteger; }
    FMapInfo: PPageTilemapInfo;
  public
    constructor Create(MapInfo: PPageTilemapInfo;
      MemoryManagerInterface: IPageMemoryManager);

    procedure Clear;
    procedure Invalidate;
    procedure SetMapSize(Width, Height: Integer);

    property Map[X, Y: Integer]: TPageTileRecord read GetMapTileRecord
      write SetMapTileRecord;
    property Width: Integer read GetMapWidth;
    property Height: Integer read GetMapHeight;
  end;

const
  EMPTY_TILE: TPageTileRecord = (TextureID: -1; Flags: []);

implementation

{ TPageTileMap }

function TPageTileMap.GetMapHeight: Integer;
begin
  Result := FMapInfo^.Height;
end;

function TPageTileMap.GetMapTileRecord(X, Y: Integer): TPageTileRecord;
begin
  { TODO: Range check }
  Result := TPageTileRecord((FMapInfo^.Tilemap+Y*FMapInfo^.Width*
    SizeOf(TPageTileRecord)+X*SizeOf(TPageTileRecord))^);
end;

function TPageTileMap.GetMapWidth: Integer;
begin
  Result := FMapInfo^.Width;
end;

procedure TPageTileMap.SetMapTileRecord(X, Y: Integer; AValue: TPageTileRecord);
begin
  { TODO: Range check }
  Move(AValue, TPageTileRecord((FMapInfo^.Tilemap+Y*FMapInfo^.Width*
    SizeOf(TPageTileRecord)+X*SizeOf(TPageTileRecord))^),
    SizeOf(TPageTileRecord));
end;

constructor TPageTileMap.Create(MapInfo: PPageTilemapInfo;
  MemoryManagerInterface: IPageMemoryManager);
begin
  FMemoryManagerInterface := MemoryManagerInterface;
  FMapInfo := MapInfo;
end;

procedure TPageTileMap.Clear;
var
  intLoop: Integer;
begin
  for intLoop := 0 to (FMapInfo^.Width*FMapInfo^.Width)-1 do
    TPageTileRecord((FMapInfo^.TileMap+intLoop*SizeOf(TPageTileRecord))^) :=
    EMPTY_TILE;
end;

procedure TPageTileMap.Invalidate;
begin
  { TODO: Implement invalidate function }
end;

// Remember: Width and Height are in tile counts! (Tiles are not pixels)
procedure TPageTileMap.SetMapSize(Width, Height: Integer);
begin
  if FMapInfo^.TileMap <> nil then
    FMemoryManagerInterface.PageMMFreeMem(FMapInfo^.TileMap);

  FMapInfo^.Width := Width;
  FMapInfo^.Height := Height;

  FMapInfo^.TileMap := FMemoryManagerInterface.PageMMGetMem(Width*Height*
    SizeOf(TPageTileRecord));
  Clear;
end;

end.

