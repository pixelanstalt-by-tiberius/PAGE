unit page_tilemanager;

{$mode objfpc}{$H+}
{$inline on}

interface

uses
  Classes, SysUtils, PageAPI; //, page_texturemanager;

type
  { TPageTileMap }

  TPageTileMap = class
  private
    FBulkUpdate: Boolean;
    function GetEnabledStatus: Boolean;
    function GetMapHeight: Integer; inline;
    function GetMapTileRecord(X, Y: Integer): TPageTileRecord; inline;
    function GetMapWidth: Integer; inline;
    procedure SetEnabledStatus(AValue: Boolean);
    procedure SetMapTileRecord(X, Y: Integer; AValue: TPageTileRecord); inline;
  protected
    FMemoryManagerInterface: IPageMemoryManager;
    FMapInfo: PPageTilemapInfo;
    FisValid: Boolean;
  public
    constructor Create(MapInfo: PPageTilemapInfo;
      MemoryManagerInterface: IPageMemoryManager);

    procedure Clear;
    procedure Invalidate; inline;
    procedure Validate; inline;
    procedure SetMapSize(Width, Height: Integer);
    procedure BeginUpdate;
    procedure EndUpdate;

    property Map[X, Y: Integer]: TPageTileRecord read GetMapTileRecord
      write SetMapTileRecord;
    property Width: Integer read GetMapWidth;
    property Height: Integer read GetMapHeight;
    property isValid: Boolean read FisValid;
    property Enabled: Boolean read GetEnabledStatus write SetEnabledStatus;
  end;

  PPageTileMap = ^TPageTileMap;

implementation

{ TPageTileMap }

function TPageTileMap.GetMapHeight: Integer;
begin
  Result := FMapInfo^.Height;
end;

function TPageTileMap.GetEnabledStatus: Boolean;
begin
  Result := FMapInfo^.Enabled;
end;

function TPageTileMap.GetMapTileRecord(X, Y: Integer): TPageTileRecord;
begin
  { TODO: Range check }
  Result := TPageTileRecord((FMapInfo^.Tilemap+(Y*FMapInfo^.Width*
    SizeOf(TPageTileRecord))+(X*SizeOf(TPageTileRecord)))^);
end;

function TPageTileMap.GetMapWidth: Integer;
begin
  Result := FMapInfo^.Width;
end;

procedure TPageTileMap.SetEnabledStatus(AValue: Boolean);
begin
  FMapInfo^.Enabled := AValue;
end;

procedure TPageTileMap.SetMapTileRecord(X, Y: Integer; AValue: TPageTileRecord);
begin
  { TODO: Range check }
  if (FBulkUpdate) or (AValue <> GetMapTileRecord(X, Y)) then
  begin
    Move(AValue, TPageTileRecord((FMapInfo^.Tilemap+Y*FMapInfo^.Width*
      SizeOf(TPageTileRecord)+X*SizeOf(TPageTileRecord))^),
      SizeOf(TPageTileRecord));
    FisValid := False;
  end;
end;

constructor TPageTileMap.Create(MapInfo: PPageTilemapInfo;
  MemoryManagerInterface: IPageMemoryManager);
begin
  FMemoryManagerInterface := MemoryManagerInterface;
  FMapInfo := MapInfo;
  FisValid := False;
  FBulkUpdate := False;
end;

procedure TPageTileMap.Clear;
var
  intLoop: Integer;
begin
  for intLoop := 0 to (FMapInfo^.Height*FMapInfo^.Width)-1 do
    TPageTileRecord((FMapInfo^.TileMap+(intLoop*SizeOf(TPageTileRecord)))^) :=
    EMPTY_TILE;
  FisValid := False;
end;

procedure TPageTileMap.Invalidate;
begin
  FisValid := False;
end;

procedure TPageTileMap.Validate;
begin
  FisValid := True;
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

procedure TPageTileMap.BeginUpdate;
begin
  FBulkUpdate := True;
end;

procedure TPageTileMap.EndUpdate;
begin
  FBulkUpdate := False;
  FisValid := False;
end;

end.

