unit page_texturemanager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL2, PageAPI, SDL2_Image;

{ TODO: Reference count for textures }
{ TODO: "Texture hash"? }
{ TODO: Texture Limit will be lost on rebind }

type
  { TODO: Maybe add source and dirty flag to allow reload if renderer changes }
  { TODO: Maybe omit TextureName -> TPageTextureInfo becomes TexturePointer if
          not source and/or dirty flag added }
  { TODO: Change type of TextureName and handle 'PChars' manually }
  { TODO: Maybe change to packed record? }
  TPageTextureInfo = record
    TextureName: String;
    TexturePointer: PSDL_Texture;
  end;
  PPageTextureInfo = ^TPageTextureInfo;


  TPageInlineResourceType = (rtImagePNG, rtImageBMP, rtTexture);

  { TPageTextureManager }

  TPageTextureManager = class
  private
    { TODO: Move FCurrentTextureCount to PersistenceInfo }
    FTextureLimit: Integer;

    function GetPageTextureInfo(Index: Integer): TPageTextureInfo;
    function GetTextureCount: Integer;
    function GetTextureLimit: Integer;
    function GetTexturePointer(Index: Integer): PSDL_Texture;
    procedure SetPageTextureInfo(Index: Integer; AValue: TPageTextureInfo);
    procedure SetTextureLimit(AValue: Integer);
  protected
    //FTextureNameSearchTable: array[0..26] of array of Integer;

    FMemoryWrapper: IPageMemoryWrapper;

    function CanAddNewTexture: Boolean; inline;
    procedure CheckAndEnlargeTextureArray; //inline;

    function AddTexture(aSDLTexture: PSDL_Texture;
      aTextureName: String): Integer;

    function GetTextureIndexFromSearchTable(aTextureName: String): Integer;
    function GetTextureIndexByLinearSearch(aTextureName: String): Integer;

    function LoadInlineResourcePNG(aResource: Pointer;
      aResourceSize: Integer): PSDL_Texture;
  public
    property TextureCount: Integer read GetTextureCount;
    property TextureLimit: Integer read GetTextureLimit write SetTextureLimit;
    property TexturePointers[Index: Integer]: PSDL_Texture
      read GetTexturePointer;
    property Textures[Index: Integer]: TPageTextureInfo read GetPageTextureInfo
      write SetPageTextureInfo;

    constructor Create(aMemoryWrapper: IPageMemoryWrapper);
    destructor Destroy; override;

    function AddTextureFromInlineResource(aResource: Pointer;
      aResourceSize: Integer; aResourceType: TPageInlineResourceType;
      aTextureName: String = ''): Integer;
    function FreeTexture(Index: Integer): Boolean;
    function FreeAllTextures: Boolean;
    function GetTextureByName(aTextureName: String): PSDL_Texture;
    function GetTextureDimensions(aTextureID: Integer): TPageCoordinate2D;
  end;
  PPageTextureManager = ^TPageTextureManager;

const
  TEXTUREARRAY_ENLARGE_AMOUNT = 50;
  UNDEFINED_TEXTURE_INFO: TPageTextureInfo = (TextureName : 'undefined';
    TexturePointer : nil);

implementation

{ TPageTextureManager }

function TPageTextureManager.GetPageTextureInfo(Index: Integer
  ): TPageTextureInfo;
begin
  Result := TPageTextureInfo((FMemoryWrapper.TextureManagerInfo.
    TextureInfoPersistence.Stream+Index*SizeOf(TPageTextureInfo))^);
end;

function TPageTextureManager.GetTextureCount: Integer;
begin
  Result := FMemoryWrapper.TextureManagerInfo.CurrentTextureInfoCount;
end;

function TPageTextureManager.GetTextureLimit: Integer;
begin
  Result := FTextureLimit;
end;

function TPageTextureManager.GetTexturePointer(Index: Integer): PSDL_Texture;
begin
  Result := nil;
  if (Index > -1) and (Index <= TextureCount-1) then
    Result := Textures[Index].TexturePointer;
end;

procedure TPageTextureManager.SetPageTextureInfo(Index: Integer;
  AValue: TPageTextureInfo);
begin
  { TODO: Check boundaries }

  Move(AValue, (FMemoryWrapper.TextureManagerInfo.
    TextureInfoPersistence.Stream+Index*SizeOf(TPageTextureInfo))^,
    SizeOf(AValue));
end;

procedure TPageTextureManager.SetTextureLimit(AValue: Integer);
begin
  if AValue <= 0 then
    FTextureLimit := -1
  else
    FTextureLimit := AValue;
end;

function TPageTextureManager.CanAddNewTexture: Boolean;
begin
  Result := (FTextureLimit = -1) or (FTextureLimit < TextureCount);
end;

procedure TPageTextureManager.CheckAndEnlargeTextureArray;
var
  ptrNew: Pointer;
begin
  { TODO: Offset error? }
  if (TextureCount+1) > FMemoryWrapper.TextureManagerInfo.
    TextureInfoPersistence.EntriesOrSize then
  begin
    ptrNew := FMemoryWrapper.VRAMMemoryManagerInterface.PageMMAllocMem((
      FMemoryWrapper.TextureManagerInfo.TextureInfoPersistence.EntriesOrSize+
      TEXTUREARRAY_ENLARGE_AMOUNT)*SizeOf(TPageTextureInfo));

    Move(FMemoryWrapper.TextureManagerInfo.
      TextureInfoPersistence.Stream, ptrNew, FMemoryWrapper.TextureManagerInfo.
      TextureInfoPersistence.EntriesOrSize*SizeOf(TPageTextureInfo));
    if FMemoryWrapper.TextureManagerInfo.TextureInfoPersistence.Stream <>
      nil then
      FMemoryWrapper.VRAMMemoryManagerInterface.PageMMFreeMem(FMemoryWrapper.
        TextureManagerInfo.TextureInfoPersistence.Stream);
    TPageTextureManagerInfo(FMemoryWrapper.TextureManagerInfoPointer^).
      TextureInfoPersistence.Stream := ptrNew;

    TPageTextureManagerInfo(FMemoryWrapper.TextureManagerInfoPointer^).
      TextureInfoPersistence.EntriesOrSize :=
      (FMemoryWrapper.TextureManagerInfo.TextureInfoPersistence.EntriesOrSize+
      TEXTUREARRAY_ENLARGE_AMOUNT)*SizeOf(TPageTextureInfo)
    //SetLength(FTextures^, Length(FTextures^)+TEXTUREARRAY_ENLARGE_AMOUNT);
  end;
end;

function TPageTextureManager.AddTexture(aSDLTexture: PSDL_Texture;
  aTextureName: String): Integer;
begin
  Result := -1;
  if (CanAddNewTexture) then
  begin
    { TODO: Refactor if needed by memory manager }
    CheckAndEnlargeTextureArray;

    TPageTextureInfo((FMemoryWrapper.TextureManagerInfo.
      TextureInfoPersistence.Stream+TextureCount*
      SizeOf(TPageTextureInfo))^).TextureName := aTextureName;
    //Textures[FCurrentTextureCount].TextureName := aTextureName;
    //Textures[FCurrentTextureCount].TexturePointer := aSDLTexture;
    TPageTextureInfo((FMemoryWrapper.TextureManagerInfo.
      TextureInfoPersistence.Stream+TextureCount*
      SizeOf(TPageTextureInfo))^).TexturePointer := aSDLTexture;

    Result := TextureCount;

    Inc(TPageTextureManagerInfo(FMemoryWrapper.TextureManagerInfoPointer^).
      CurrentTextureInfoCount);
  end;
end;

function TPageTextureManager.GetTextureIndexFromSearchTable(aTextureName: String
  ): Integer;
begin
  { TODO: Implement search by table }
  // Currently not implemented - falling back to linear search
  Result := GetTextureIndexByLinearSearch(aTextureName);
end;

function TPageTextureManager.GetTextureIndexByLinearSearch(aTextureName: String
  ): Integer;
var
  intTextureLoop: Integer;
begin
  Result := -1;

  for intTextureLoop := 0 to TextureCount-1 do
  begin
    if Textures[intTextureLoop].TexturePointer = nil then
      Continue;

    if Textures[intTextureLoop].TextureName = aTextureName then
    begin
      Result := intTextureLoop;
      Exit;
    end;
  end;
end;

function TPageTextureManager.LoadInlineResourcePNG(aResource: Pointer;
  aResourceSize: Integer): PSDL_Texture;
var
  rwop: PSDL_RWops;
begin
  rwop := SDL_RWFromConstMem(aResource, aResourceSize);
  Result := IMG_LoadTextureTyped_RW(FMemoryWrapper.SDLRenderer, rwop, -1,
    'PNG');
end;

constructor TPageTextureManager.Create(aMemoryWrapper: IPageMemoryWrapper);
begin
  FMemoryWrapper := aMemoryWrapper;
  FTextureLimit := -1;
  //SetLength(FTextShadowArray, 0);
  //FTextures := @FTextShadowArray;
  {$warning SetLength methods etc. must be changed for use with provided
            memory manager }
end;

destructor TPageTextureManager.Destroy;
begin
  { ToDo: Free all other memory used }

  FreeAllTextures;
end;

function TPageTextureManager.AddTextureFromInlineResource(aResource: Pointer;
  aResourceSize: Integer; aResourceType: TPageInlineResourceType;
  aTextureName: String): Integer;
var
  texture: PSDL_Texture;
begin
  Result := -1;
  case aResourceType of
    rtImagePNG: begin
                  texture := LoadInlineResourcePNG(aResource, aResourceSize);
                  if texture <> nil then
                    Result := AddTexture(texture, aTextureName);
                end;
    { TODO: Error handling }
    end;
  end;

function TPageTextureManager.FreeAllTextures: Boolean;
var
  intTextureLoop: Integer;
begin
  Result := True;
  for intTextureLoop := 0 to TextureCount-1 do
  begin
    Result := FreeTexture(intTextureLoop);
    if not Result then
      Break;
  end;
end;

function TPageTextureManager.GetTextureByName(aTextureName: String
  ): PSDL_Texture;
var
  intTextureIndex: Integer;
begin
  Result := nil;
  intTextureIndex := GetTextureIndexByLinearSearch(aTextureName);
  if intTextureIndex > -1 then
    Result := Textures[intTextureIndex].TexturePointer;
end;

function TPageTextureManager.GetTextureDimensions(aTextureID: Integer
  ): TPageCoordinate2D;
begin
  SDL_QueryTexture(Textures[aTextureID].TexturePointer, nil, nil, @Result.X,
    @Result.Y);
end;


function TPageTextureManager.FreeTexture(Index: Integer): Boolean;
begin
  Result := False;
  if (Index > -1) and (Index <= TextureCount-1) then
  begin
    if Textures[Index].TexturePointer <> nil then
      SDL_DestroyTexture(Textures[Index].TexturePointer);
    Textures[Index] := UNDEFINED_TEXTURE_INFO;
    Result := True;
  end;
end;


end.

