unit page_texturemanager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL2, PageAPI, SDL2_Image;

{ TODO: Reference count for textures }
{ TODO: "Texture hash"? }

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
  TPageTextureInfoArray = array of TPageTextureInfo;
  PPageTextureInfoArray = ^TPageTextureInfoArray;


  TPageInlineResourceType = (rtImagePNG, rtImageBMP, rtTexture);

  { TPageTextureManager }

  TPageTextureManager = class
  private
    FCurrentTextureCount, FTextureLimit: Integer;

    function GetPageTextureInfo(Index: Integer): TPageTextureInfo;
    function GetTextureCount: Integer;
    function GetTextureLimit: Integer;
    function GetTexturePointer(Index: Integer): PSDL_Texture;
    procedure SetTextureLimit(AValue: Integer);
  protected
    FTextShadowArray: TPageTextureInfoArray;
    FTextures: PPageTextureInfoArray;
    FTextureNameSearchTable: array[0..26] of array of Integer;
    FSDLRenderer: PSDL_Renderer;
    FMemoryManager: IPageMemoryManager;

    function CanAddNewTexture: Boolean; inline;
    procedure CheckAndEnlargeTextureArray; //inline;

    function AddTexture(aSDLTexture: PSDL_Texture;
      aTextureName: String): Integer;

    function GetTextureIndexFromSearchTable(aTextureName: String): Integer;
    function GetTextureIndexByLinearSearch(aTextureName: String): Integer;

    function LoadInlineResourcePNG(aResource: Pointer;
      aResourceSize: Integer): PSDL_Texture;
  public
    property TextureCount: Integer read FCurrentTextureCount;
    property TextureLimit: Integer read GetTextureLimit write SetTextureLimit;
    property TexturePointers[Index: Integer]: PSDL_Texture
      read GetTexturePointer;
    property Textures[Index: Integer]: TPageTextureInfo read GetPageTextureInfo;

    constructor Create(aSDLRenderer: PSDL_Renderer;
      aMemoryManager: IPageMemoryManager);
    destructor Destroy; override;

    function AddTextureFromInlineResource(aResource: Pointer;
      aResourceSize: Integer; aResourceType: TPageInlineResourceType;
      aTextureName: String = ''): Integer;
    function FreeTexture(Index: Integer): Boolean;
    function FreeAllTextures: Boolean;
    function GetTextureByName(aTextureName: String): PSDL_Texture;
    procedure SetRenderer(aSDLRenderer: PSDL_Renderer);
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
  if (Index > -1) and (Index <= High(FTextures^)) then
    Result := FTextures^[Index]
  else
    Result := UNDEFINED_TEXTURE_INFO;
end;

function TPageTextureManager.GetTextureCount: Integer;
begin
  Result := FCurrentTextureCount;
end;

function TPageTextureManager.GetTextureLimit: Integer;
begin
  Result := FTextureLimit;
end;

function TPageTextureManager.GetTexturePointer(Index: Integer): PSDL_Texture;
begin
  Result := nil;
  if (Index > -1) and (Index <= High(FTextures^)) then
    Result := FTextures^[Index].TexturePointer;
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
  Result := (FTextureLimit = -1) or (FTextureLimit < FCurrentTextureCount);
end;

procedure TPageTextureManager.CheckAndEnlargeTextureArray;
begin
  if (FCurrentTextureCount+1) > High(FTextures^) then
    SetLength(FTextures^, Length(FTextures^)+TEXTUREARRAY_ENLARGE_AMOUNT);
end;

function TPageTextureManager.AddTexture(aSDLTexture: PSDL_Texture;
  aTextureName: String): Integer;
begin
  Result := -1;
  if (CanAddNewTexture) then
  begin
    { TODO: Refactor if needed by memory manager }
    CheckAndEnlargeTextureArray;

    FTextures^[FCurrentTextureCount].TextureName := aTextureName;
    FTextures^[FCurrentTextureCount].TexturePointer := aSDLTexture;

    Result := FCurrentTextureCount;

    Inc(FCurrentTextureCount);
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

  for intTextureLoop := 0 to High(FTextures^) do
  begin
    if FTextures^[intTextureLoop].TexturePointer = nil then
      Continue;

    if FTextures^[intTextureLoop].TextureName = aTextureName then
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
  Result := IMG_LoadTextureTyped_RW(FSDLRenderer, rwop, -1, 'PNG');
end;

constructor TPageTextureManager.Create(aSDLRenderer: PSDL_Renderer;
  aMemoryManager: IPageMemoryManager);
begin
  FSDLRenderer := aSDLRenderer;
  FCurrentTextureCount := 0;
  FTextureLimit := -1;
  FMemoryManager := aMemoryManager;
  SetLength(FTextShadowArray, 0);
  FTextures := @FTextShadowArray;
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
  for intTextureLoop := 0 to High(FTextures^) do
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
    Result := FTextures^[intTextureIndex].TexturePointer;
end;

procedure TPageTextureManager.SetRenderer(aSDLRenderer: PSDL_Renderer);
begin
  { TODO: Issue warning / omit if dirty flag and reloading can be done on the
          fly }
  if FSDLRenderer <> nil then
    FreeAllTextures;

  FSDLRenderer := aSDLRenderer;
end;

function TPageTextureManager.GetTextureDimensions(aTextureID: Integer
  ): TPageCoordinate2D;
begin
  SDL_QueryTexture(FTextures^[aTextureID].TexturePointer, nil, nil, @Result.X,
    @Result.Y);
end;


function TPageTextureManager.FreeTexture(Index: Integer): Boolean;
begin
  Result := False;
  if (Index > -1) and (Index <= High(FTextures^)) then
  begin
    if Textures[Index].TexturePointer <> nil then
      SDL_DestroyTexture(FTextures^[Index].TexturePointer);
    FTextures^[Index] := UNDEFINED_TEXTURE_INFO;
    Result := True;
  end;
end;


end.

