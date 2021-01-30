unit page_BitmapFontManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PageAPI, page_texturemanager, SDL2, SDL2_Image,
  font_manaspace16 in '../../res/fonts/font_manaspace16.pas',
  font_dogica8 in '../../res/fonts/font_dogica8.pas';

type
  TCharLookup = record
    CharID: Char;
    TextureID: TPageTextureID;
  end;

  { TPageBMFManager }

  TPageBMFManager = class
  private
    function GetFontHeight: Integer;
    function GetFontWidth: Integer;
    function GetIsFontLoaded: Boolean;
    procedure SetFontHeight(AValue: Integer);
    procedure SetFontWidth(AValue: Integer);
  protected
    // Persistance -->
    FLookupTable: array of TCharLookup;
    FFontWidth, FFontHeight: Integer;
    FisFontLoaded: Boolean;
    // <-- Persistance
    { TODO: Move persistence }

    FMemoryWrapper: IPageMemoryWrapper;
    FtextureManager: PPageTextureManager;

    function LoadBitmapIntoTexture(FontBitmapPNG: Pointer;
      FontBitmapPNGSize: Integer): PSDL_Texture;
    function RenderCharacterToTexture(FontTexture: PSDL_Texture;
      CharacterInfo: TFNTCharInfo): PSDL_Texture;
  public
    constructor Create(aMemoryWrapper: IPageMemoryWrapper);
    procedure AssignTextureManager(aTextureManager: PPageTextureManager);


    { TODO: Maybe change to pointer and size for dynamic array due to
            compatiblity for other languages/compilers }
    function LoadFont(FontBitmapPNG: Pointer; FontBitmapPNGSize: Integer;
      FNTCharInfos: Pointer; CharInfoCount: Integer): Boolean;
    function LoadIntegratedFont: Boolean;

    function TextureID(theChar: Char): Integer;

    property FontHeight: Integer read GetFontHeight write SetFontHeight;
    property FontWidth: Integer read GetFontWidth write SetFontWidth;
    property isFontLoaded: Boolean read GetIsFontLoaded;
  end;
  PPageBMFManager = ^TPageBMFManager;

implementation

{ TPageBMFManager }

function TPageBMFManager.GetFontHeight: Integer;
begin
  Result := FFontHeight;
end;

function TPageBMFManager.GetFontWidth: Integer;
begin
  Result := FFontWidth;
end;

function TPageBMFManager.GetIsFontLoaded: Boolean;
begin
  Result := FisFontLoaded;
end;

procedure TPageBMFManager.SetFontHeight(AValue: Integer);
begin
  FFontHeight := AValue;
end;

procedure TPageBMFManager.SetFontWidth(AValue: Integer);
begin
  FFontWidth := AValue;
end;

function TPageBMFManager.LoadBitmapIntoTexture(FontBitmapPNG: Pointer;
  FontBitmapPNGSize: Integer): PSDL_Texture;
var
  rwop: PSDL_RWops = nil;
begin
  rwop := SDL_RWFromConstMem(FontBitmapPNG, FontBitmapPNGSize);
  Result := IMG_LoadTextureTyped_RW(FMemoryWrapper.SDLRenderer, rwop, -1,
    'PNG');
end;

function TPageBMFManager.RenderCharacterToTexture(FontTexture: PSDL_Texture;
  CharacterInfo: TFNTCharInfo): PSDL_Texture;
var
  OldRenderTarget: PSDL_Texture;
  srcRect, dstRect: TSDL_Rect;
begin
  OldRenderTarget := SDL_GetRenderTarget(FMemoryWrapper.SDLRenderer);
  Result := SDL_CreateTexture(FMemoryWrapper.SDLRenderer, 0,
    SDL_TEXTUREACCESS_TARGET, FontWidth, FontHeight);
  if SDL_SetRenderTarget(FMemoryWrapper.SDLRenderer, Result) <> 0 then
    Exception.Create('Failed to change rendering target');

  //SDL_RenderClear(FMemoryWrapper.SDLRenderer);

  srcRect.x := CharacterInfo.X;
  srcRect.y := CharacterInfo.Y;
  srcRect.w := CharacterInfo.W;
  srcRect.h := CharacterInfo.H;

  dstRect.x := Round(FontWidth/2-srcRect.w/2);//-CharacterInfo.xOffset;
  //dstRect.y := Round(FontHeight/2-srcRect.h/2)+CharacterInfo.yOffset;
  dstRect.y := CharacterInfo.yOffset;
  dstRect.w := srcRect.w;
  dstRect.h := srcRect.h;

  SDL_SetRenderDrawBlendMode(FMemoryWrapper.SDLRenderer, SDL_BLENDMODE_BLEND);
  SDL_SetTextureBlendMode(Result, SDL_BLENDMODE_BLEND);
  SDL_RenderCopyEx(FMemoryWrapper.SDLRenderer, FontTexture, @srcRect, @dstRect,
    0, nil, 0);
  //SDL_RenderPresent(FMemoryWrapper.SDLRenderer);

  SDL_SetRenderTarget(FMemoryWrapper.SDLRenderer, OldRenderTarget);
end;

constructor TPageBMFManager.Create(aMemoryWrapper: IPageMemoryWrapper);
begin
  FMemoryWrapper := aMemoryWrapper;
  FTextureManager := nil;
  FontHeight := -1;
  FontWidth := -1;
end;

procedure TPageBMFManager.AssignTextureManager(aTextureManager: PPageTextureManager
  );
begin
  FTextureManager := aTextureManager;
end;

function TPageBMFManager.LoadFont(FontBitmapPNG: Pointer;
  FontBitmapPNGSize: Integer; FNTCharInfos: Pointer; CharInfoCount: Integer
  ): Boolean;
var
  wholeBitmapTexture, charTexture: PSDL_Texture;
  intCharLoop: Integer;
begin
  Result := False;
  wholeBitmapTexture := LoadBitmapIntoTexture(FontBitmapPNG, FontBitmapPNGSize);
  if wholeBitmapTexture = nil then
    Exit;
  SetLength(FLookupTable, CharInfoCount);
  for intCharLoop := 0 to CharInfoCount-1 do
  begin
    FLookupTable[intCharLoop].CharID := TFNTCharInfo((FNTCharInfos+
      intCharLoop*SizeOf(TFNTCharInfo))^).CharID;
    charTexture := RenderCharacterToTexture(wholeBitmapTexture,
      TFNTCharInfo((FNTCharInfos+intCharLoop*SizeOf(TFNTCharInfo))^));
    if charTexture = nil then
      Exit; { TODO: Issue warning }
    FLookupTable[intCharLoop].TextureID := FTextureManager^.
      AddTexture(charTexture);
  end;
  SDL_DestroyTexture(wholeBitmapTexture);
  Result := True;
end;

function TPageBMFManager.LoadIntegratedFont: Boolean;
begin
  Result := False;

  if ((FontHeight = -1) and (FontWidth = -1)) or (FontHeight = 16) then
  begin
    FontHeight := 16;
    FontWidth := 16;
    Result := LoadFont(@manaspace16png, SizeOf(manaspace16png),
      @bmf_manaspace16[0], Length(bmf_manaspace16));
  end;

  if (FontHeight = 8) then
  begin
    FontHeight := 8;
    FontWidth := 8;
    Result := LoadFont(@dogica8png, SizeOf(dogica8png),
      @bmf_dogica8[0], Length(bmf_dogica8));
  end;
end;

function TPageBMFManager.TextureID(theChar: Char): Integer;
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

