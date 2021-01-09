unit page_BitmapFontManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PageAPI, page_texturemanager, SDL2, SDL2_Image;

type
  TCharLookup = record
    CharID: Char;
    TextureID: TPageTextureID;
  end;

  { TBMFManager }

  TBMFManager = class
  private
    function GetFontHeight: Integer;
    function GetFontWidth: Integer;
    procedure SetFontHeight(AValue: Integer);
    procedure SetFontWidth(AValue: Integer);
  protected
    // Persistance -->
    FLookupTable: array of TCharLookup;
    FFontWidth, FFontHeight: Integer;
    // <-- Persistance
    { TODO: Move persistence }

    FTextureManager: PPageTextureManager;
    FRenderer: PSDL_Renderer;

    function LoadBitmapIntoTexture(FontBitmapPNG: Pointer;
      FontBitmapPNGSize: Integer): PSDL_Texture;
    function RenderCharacterToTexture(FontTexture: PSDL_Texture;
      CharacterInfo: TFNTCharInfo): PSDL_Texture;
  public
    constructor Create;
    procedure AssignTextureManager(aTextureManager: PPageTextureManager);
    procedure AssignRenderer(aRenderer: PSDL_Renderer);

    { TODO: Maybe change to pointer and size for dynamic array due to
            compatiblity for other languages/compilers }
    function LoadFont(FontBitmapPNG: Pointer; FontBitmapPNGSize: Integer;
      FNTCharInfo: TFNTCharInfoArray): Boolean;

    function TextureID(theChar: Char): Integer;

    property FontHeight: Integer read GetFontHeight write SetFontHeight;
    property FontWidth: Integer read GetFontWidth write SetFontWidth;
  end;

implementation

{ TBMFManager }

function TBMFManager.GetFontHeight: Integer;
begin
  Result := FFontHeight;
end;

function TBMFManager.GetFontWidth: Integer;
begin
  Result := FFontWidth;
end;

procedure TBMFManager.SetFontHeight(AValue: Integer);
begin
  FFontWidth := AValue;
end;

procedure TBMFManager.SetFontWidth(AValue: Integer);
begin
  FFontHeight := AValue;
end;

function TBMFManager.LoadBitmapIntoTexture(FontBitmapPNG: Pointer;
  FontBitmapPNGSize: Integer): PSDL_Texture;
var
  rwop: PSDL_RWops;
begin
  rwop := SDL_RWFromConstMem(FontBitmapPNG, FontBitmapPNGSize);
  Result := IMG_LoadTextureTyped_RW(FRenderer, rwop, -1, 'PNG');
end;

function TBMFManager.RenderCharacterToTexture(FontTexture: PSDL_Texture;
  CharacterInfo: TFNTCharInfo): PSDL_Texture;
var
  OldRenderTarget: PSDL_Texture;
  srcRect, dstRect: TSDL_Rect;
begin
  OldRenderTarget := SDL_GetRenderTarget(FRenderer);
  Result := SDL_CreateTexture(FRenderer, 0, SDL_TEXTUREACCESS_TARGET, FontWidth,
    FontHeight);
  if SDL_SetRenderTarget(FRenderer, Result) <> 0 then
    Exception.Create('Failed to change rendering target');

  SDL_RenderClear(FRenderer);

  srcRect.x := CharacterInfo.X;
  srcRect.y := CharacterInfo.Y;
  srcRect.w := CharacterInfo.W;
  srcRect.h := CharacterInfo.H;

  dstRect.x := Round(FontWidth/2-srcRect.w/2)+CharacterInfo.xOffset;
  dstRect.y := Round(FontHeight/2-srcRect.h/2)+CharacterInfo.yOffset;
  dstRect.w := srcRect.w;
  dstRect.h := srcRect.h;

  SDL_RenderCopyEx(FRenderer, FontTexture, @srcRect, @dstRect, 0, nil, 0);
  SDL_RenderPresent(FRenderer);

  SDL_SetRenderTarget(FRenderer, OldRenderTarget);
end;

constructor TBMFManager.Create;
begin
  FTextureManager := nil;
end;

procedure TBMFManager.AssignTextureManager(aTextureManager: PPageTextureManager
  );
begin
  FTextureManager := aTextureManager;
end;

procedure TBMFManager.AssignRenderer(aRenderer: PSDL_Renderer);
begin
  FRenderer := aRenderer;
end;

function TBMFManager.LoadFont(FontBitmapPNG: Pointer;
  FontBitmapPNGSize: Integer; FNTCharInfo: TFNTCharInfoArray): Boolean;
var
  wholeBitmapTexture, charTexture: PSDL_Texture;
  intCharLoop: Integer;
begin
  Result := False;
  wholeBitmapTexture := LoadBitmapIntoTexture(FontBitmapPNG, FontBitmapPNGSize);
  if wholeBitmapTexture = nil then
    Exit;
  SetLength(FLookupTable, Length(FNTCharInfo));
  for intCharLoop := 0 to High(FNTCharInfo) do
  begin
    FLookupTable[intCharLoop].CharID := FNTCharInfo[intCharLoop].CharID;
    charTexture := RenderCharacterToTexture(wholeBitmapTexture,
      FNTCharInfo[intCharLoop]);
    if charTexture = nil then
      Exit; { TODO: Issue warning }
    FLookupTable[intCharLoop].TextureID := FTextureManager^.
      AddTexture(charTexture);
  end;
  SDL_DestroyTexture(wholeBitmapTexture);
  Result := True;
end;

function TBMFManager.TextureID(theChar: Char): Integer;
var
  intLoop: Integer;
begin
  Result := -1;
  for intLoop := 0 to High(FLookupTable) do
    if FLookupTable[intLoop].CharID = theChar then
    begin
      Result := FLookupTable[intLoop].TextureID;
      Break;
    end;
end;

end.

