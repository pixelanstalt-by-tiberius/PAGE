var
  intState: Integer = 0;
  intSubState: Integer = 0;
  intSelectedMenuEntry: Integer = 0;
  intLastTick: UInt32 = 0;
  boolBlinkState: Boolean = True;
  intMenuSelectEndX: array[0..1] of Integer = (24, 8);
  RandomTextures: array[0..99] of TPageTextureID;
  CanvasDimension: TPageCoordinate2D;
  Coord: TPageCoordinate2D;
  event: TSDL_Event;
  boolDown: Boolean = False;
  boolEnter: Boolean = False;
  intTilemaps, intX, intY: Integer;
  tr: TPageTileRecord;
  // Performance counter
  Performance: array[0..(PAGE_MAX_TILEMAPS*4)-1] of array [0..2] of UInt64;
  intLastPerformanceCount, intPerfDiff, intPerfFreq: UInt64;
  intCycles: UInt64 = 0;
  intDiffSum: UInt64 = 0;
  //
  PerformanceOutFile: TextFile;
begin

  // Initialize Subsystems for performance testing

  // Create and initialize tilemaps
  // Initialize Render Engine

  for intTilemaps := 0 to High(Performance) do
  begin
    Performance[intTilemaps][0] := High(UInt64);
    Performance[intTilemaps][1] := Low(UInt64);
  end;


  intPerfFreq := SDL_GetPerformanceFrequency;

  Randomize;

  FRenderEngine.TileHeight := 8;
  FRenderEngine.TileWidth := 8;
  FBitmapFontManager.FontHeight := 8;
  FRenderEngine.TilemapCount := PAGE_MAX_TILEMAPS;

  gEventQueue.CastEventString(etNotification, psMain, psDebug, esInfo,
    PChar(Format('Total number of tilemaps: %d', [FRenderEngine.TilemapCount])));

  CanvasDimension := FMemoryWrapper.RenderEngineInfo^.RenderingDimension;
  CanvasDimension.X *= 2;
  CanvasDimension.Y *= 2;
  FMemoryWrapper.RenderEngineInfo^.CanvasDimension := CanvasDimension;

  FRenderEngine.BuildTilemaps;
  FRenderEngine.BuildSpriteStreams;

  if not FBitmapFontManager.isFontLoaded then
    FBitmapFontManager.LoadIntegratedFont;


  for intX := 0 to High(RandomTextures) do
    RandomTextures[intX] := FBitmapFontManager.TextureID(Char(
      Random(75)+48));

  while not (FMemoryWrapper.ExitGameLoop) do
  begin
    ProcessDispatchedEvents;


    case intState of
      0:  begin
            WriteText(0, 0, 0, 'Pixelanstalt Game Engine');
            WriteText(0, 0, 1, 'Performanace Test');
            WriteText(0, 43, 0, 'FPS:');

            WriteText(0, 48, 0, Format('%.4d', [FRenderEngine.FPS]));

            while (SDL_PollEvent(@event) <> 0) do
            begin
              case event.type_ of
                SDL_KEYDOWN:
                  begin
                    case event.key.keysym.sym of
                      SDLK_RETURN: boolEnter := True;
                      SDLK_DOWN: boolDown := True;
                    end;
                  end;
              end;
            end;

            if SDL_GetTicks - intLastTick >= 500 then
            begin
              boolBlinkState := not boolBlinkState;
              intLastTick := SDL_GetTicks;
            end;

            if boolBlinkState then
            begin
              WriteText(1, 1, intSelectedMenuEntry+4, '[');
              WriteText(1, intMenuSelectEndX[intSelectedMenuEntry],
                intSelectedMenuEntry+4, ']');
            end
            else
            begin
              WriteText(1, 1, intSelectedMenuEntry+4, ' ');
              WriteText(1, intMenuSelectEndX[intSelectedMenuEntry],
                intSelectedMenuEntry+4, ' ');
            end;

            if boolDown then
            begin
              if boolBlinkState then
              begin
                WriteText(1, 1, intSelectedMenuEntry+4, ' ');
                WriteText(1, intMenuSelectEndX[intSelectedMenuEntry],
                  intSelectedMenuEntry+4, ' ');
              end;
              intSelectedMenuEntry := (intSelectedMenuEntry + 1) mod 2;
              boolDown := False;
            end;

            if boolEnter then
            begin
              case intSelectedMenuEntry of
                0: begin
                     intState := 1; // Rendering Test
                     FRenderEngine.ClearAllTilemaps;
                     intLastTick := SDL_GetTicks;
                     intLastPerformanceCount := 0;
                   end;
                1: FMemoryWrapper.ExitGameLoop := True;
              end;
              boolEnter := False;
            end;

            WriteText(1, 3, 4, 'Start rendering test');
            WriteText(1, 3, 5, 'Exit');
          end;
      1: begin
           // Begin rendering test; increase substate each 10 seconds
           if SDL_GetTicks - intLastTick >= 10000 then
           begin
             Performance[intSubState][2] :=
               Round(intDiffSum/intCycles);

             gEventQueue.CastEventString(etNotification, psMain, psDebug,
               esInfo, PChar(Format('Rendering test subsequence %d ended. ' +
                 'Cycles: %d, Min: %.3f ms (%.2f fps), Max: %.3f ms ' +
                 '(%.2f fps), Avg: %.3f ms (%.2f fps)', [intSubState, intCycles,
               Performance[intSubState][0]/(intPerfFreq/1000),
               1000/(Performance[intSubState][0]/(intPerfFreq/1000)),
               Performance[intSubState][1]/(intPerfFreq/1000),
               1000/(Performance[intSubState][1]/(intPerfFreq/1000)),
               Performance[intSubState][2]/(intPerfFreq/1000),
               1000/(Performance[intSubState][2]/(intPerfFreq/1000))])));

             Inc(intSubState);


             if (intSubState mod 4 <> 0) and ((intSubState mod 2) = 0) then
             begin
               Coord.X := 0;
               Coord.Y := 0;
               for intTilemaps := 0 to FRenderEngine.TilemapCount-1 do
                 FRenderEngine.TilemapOffsets[intTilemaps] := Coord;
               FRenderEngine.SetRenderingModePerTile;
             end;

             if (intSubState mod 4) = 0 then
             begin
               Coord.X := 0;
               Coord.Y := 0;
               for intTilemaps := 0 to FRenderEngine.TilemapCount-1 do
                 FRenderEngine.TilemapOffsets[intTilemaps] := Coord;
               FRenderEngine.Tilemaps[FRenderEngine.TilemapCount-
                 (intSubState div 4)].Enabled := False;
               FRenderEngine.SetRenderingModePerLayer;

               gEventQueue.CastEventString(etNotification, psMain, psDebug,
                 esInfo, PChar(Format('Disabled Tilemap #%d',
                   [FRenderEngine.TilemapCount-(intSubState div 4)])));
             end;
             {if intSubState = 6 then
             begin
               Coord.X := 0;
               Coord.Y := 0;
               for intTilemaps := 0 to FRenderEngine.TilemapCount-1 do
                 FRenderEngine.TilemapOffsets[intTilemaps] := Coord;
               FRenderEngine.SetRenderingModePerTile;
             end;}

             intLastPerformanceCount := 0;
             intDiffSum := 0;
             intCycles := 0;

             intLastTick := SDL_GetTicks;
           end;

           // Measure performance
           if intLastPerformanceCount = 0 then
             intLastPerformanceCount := SDL_GetPerformanceCounter
           else
           begin
             intPerfDiff := SDL_GetPerformanceCounter - intLastPerformanceCount;
             if Performance[intSubState][0] > intPerfDiff then
               Performance[intSubState][0] := intPerfDiff;
             if Performance[intSubState][1] < intPerfDiff then
               Performance[intSubState][1] := intPerfDiff;

             intDiffSum := intDiffSum + intPerfDiff;
             intCycles := intCycles + 1;

             intLastPerformanceCount := SDL_GetPerformanceCounter;
           end;

           if intSubState < (PAGE_MAX_TILEMAPS*4) then
           begin
             case (intSubState mod 2) of
               0: begin
                    for intTilemaps := 0 to FRenderEngine.TilemapCount-1 do
                    begin
                      if not FRenderEngine.Tilemaps[intTilemaps].Enabled then
                        Continue;
                      FRenderEngine.Tilemaps[intTilemaps].BeginUpdate;
                      for intX := 0 to FRenderEngine.Tilemaps[intTilemaps].
                        Width-1 do
                        for intY := 0 to FRenderEngine.Tilemaps[intTilemaps].
                          Height-1 do
                        begin
                          tr.TextureID := RandomTextures[(intLastPerformanceCount+(intX+intY)+(intX*intY)) mod 100];
                          FRenderEngine.Tilemaps[intTilemaps].Map[intX, intY] :=
                            tr;
                        end;
                      FRenderEngine.Tilemaps[intTilemaps].EndUpdate;
                    end
                  end;
               1: begin
                    for intTilemaps := 0 to FRenderEngine.TilemapCount-1 do
                    begin
                      if not FRenderEngine.Tilemaps[intTilemaps].Enabled then
                        Continue;
                      FRenderEngine.Tilemaps[intTilemaps].BeginUpdate;
                      for intX := 0 to FRenderEngine.Tilemaps[intTilemaps].
                        Width-1 do
                        for intY := 0 to FRenderEngine.Tilemaps[intTilemaps].
                          Height-1 do
                        begin
                          tr.TextureID := RandomTextures[(intLastPerformanceCount+(intX+intY)+(intX*intY)) mod 100];
                          FRenderEngine.Tilemaps[intTilemaps].Map[intX, intY] :=
                            tr;
                        end;
                      FRenderEngine.Tilemaps[intTilemaps].EndUpdate;
                    end;

                    for intTilemaps := 0 to FRenderEngine.TilemapCount-1 do
                    begin
                      if not FRenderEngine.Tilemaps[intTilemaps].Enabled then
                        Continue;
                      Coord := FRenderEngine.TilemapOffsets[intTilemaps];
                      if Coord.X > 5 then
                        Coord.X := Coord.X+5*(Random(2)-1)
                      else
                        Inc(Coord.X);

                      if Coord.Y > 5 then
                        Coord.Y := Coord.Y+5*(Random(2)-1)
                      else
                        Inc(Coord.Y);

                      FRenderEngine.TilemapOffsets[intTilemaps] := Coord;
                    end;
                  end;
             end;
           end
           else
             if intSubState = (PAGE_MAX_TILEMAPS*4) then
             begin
               AssignFile(PerformanceOutFile, 'rendering.r');
               ReWrite(PerformanceOutFile);

               {WriteLn(PerformanceOutFile, 'Sequence|Min Interval [ms]|' +
                 'Max Interval [ms]|Avg Interval [ms]');

               for intTilemaps := 0 to High(Performance) do
               begin
                 WriteLn(PerformanceOutFile, Format('%d|%.3f|%.3f|%.3f',
                   [intTilemaps, Performance[intTilemaps][0]/(intPerfFreq/1000),
                   Performance[intTilemaps][1]/(intPerfFreq/1000),
                   Performance[intTilemaps][2]/(intPerfFreq/1000)]));
               end;  }

               WriteLn(PerformanceOutFile, 'variation.bar <- function(x, y, upper, lower=upper, length=0.1,...){ arrows(x,upper, x, lower, angle=90, code=3, length=length, ...)}');

               WriteLn(PerformanceOutFile, 'catnames <- c("Whole Tilemap", ' +
                 '"Whole Tilemap + Offset", "Viewport Only", "Viewport Only' +
                 ' + Offset")');

               for intTilemaps := 0 to PAGE_MAX_TILEMAPS-1 do
               begin
                 WriteLn(PerformanceOutFile, Format('t%d <- c(%.2f, %.2f, %.2f, %.2f)',
                   [PAGE_MAX_TILEMAPS-intTilemaps,
                   1000/(Performance[(intTilemaps*4)][2]/(intPerfFreq/1000)),
                   1000/(Performance[(intTilemaps*4)+1][2]/(intPerfFreq/1000)),
                   1000/(Performance[(intTilemaps*4)+2][2]/(intPerfFreq/1000)),
                   1000/(Performance[(intTilemaps*4)+3][2]/(intPerfFreq/1000))]));
                 WriteLn(PerformanceOutFile, Format('t%d_min <- c(%.2f, %.2f, %.2f, %.2f)',
                   [PAGE_MAX_TILEMAPS-intTilemaps,
                   1000/(Performance[(intTilemaps*4)][1]/(intPerfFreq/1000)),
                   1000/(Performance[(intTilemaps*4)+1][1]/(intPerfFreq/1000)),
                   1000/(Performance[(intTilemaps*4)+2][1]/(intPerfFreq/1000)),
                   1000/(Performance[(intTilemaps*4)+3][1]/(intPerfFreq/1000))]));
                 WriteLn(PerformanceOutFile, Format('t%d_max <- c(%.2f, %.2f, %.2f, %.2f)',
                   [PAGE_MAX_TILEMAPS-intTilemaps,
                   1000/(Performance[(intTilemaps*4)][0]/(intPerfFreq/1000)),
                   1000/(Performance[(intTilemaps*4)+1][0]/(intPerfFreq/1000)),
                   1000/(Performance[(intTilemaps*4)+2][0]/(intPerfFreq/1000)),
                   1000/(Performance[(intTilemaps*4)+3][0]/(intPerfFreq/1000))]));
               end;

               // Compose fps data frame
               Write(PerformanceOutFile, 'fps = data.frame(catnames');
               for intTilemaps := 0 to PAGE_MAX_TILEMAPS-1 do
                 Write(PerformanceOutFile, Format(',t%d', [PAGE_MAX_TILEMAPS-
                   intTilemaps]));
               WriteLn(PerformanceOutFile, ')');

               WriteLn(PerformanceOutFile, 'rownames(fps) <- fps[,1]');
               WriteLn(PerformanceOutFile, 'fps = as.matrix(fps[,-1])');
               Write(PerformanceOutFile, 'colnames(fps) <- c(');

               Write(PerformanceOutFile, Format('%d', [PAGE_MAX_TILEMAPS]));
               for intTilemaps := 1 to PAGE_MAX_TILEMAPS-1 do
                 Write(PerformanceOutFile, Format(',%d', [PAGE_MAX_TILEMAPS-
                   intTilemaps]));
               WriteLn(PerformanceOutFile, ')');

               // Compose fps_min data frame
               Write(PerformanceOutFile, 'fps_min = data.frame(catnames');
               for intTilemaps := 0 to PAGE_MAX_TILEMAPS-1 do
                 Write(PerformanceOutFile, Format(',t%d_min', [PAGE_MAX_TILEMAPS-
                   intTilemaps]));
               WriteLn(PerformanceOutFile, ')');

               WriteLn(PerformanceOutFile, 'rownames(fps_min) <- fps_min[,1]');
               WriteLn(PerformanceOutFile, 'fps_min = as.matrix(fps_min[,-1])');

               // Compose fps_max data frame
               Write(PerformanceOutFile, 'fps_max = data.frame(catnames');
               for intTilemaps := 0 to PAGE_MAX_TILEMAPS-1 do
                 Write(PerformanceOutFile, Format(',t%d_max', [PAGE_MAX_TILEMAPS-
                   intTilemaps]));
               WriteLn(PerformanceOutFile, ')');

               WriteLn(PerformanceOutFile, 'rownames(fps_max) <- fps_max[,1]');
               WriteLn(PerformanceOutFile, 'fps_max = as.matrix(fps_max[,-1])');
               WriteLn(PErformanceOutFile, 'lim<- ceiling(max(fps_max)/60)*60');

               WriteLn(PerformanceOutFile, 'fps_plot <- barplot(fps, beside=T, mgp=c(2,1,0), yaxp=c(0, lim , lim/60), col=c("deepskyblue", "deepskyblue3", "darkorange", "darkorange3"), main="Pixelanstalt GameEngine Rendering Performance", sub="Viewport size: 416x224px, tile size: 8x8px, canvas size of each tilemap: 832x448px, total number of tilemaps: 6", cex.sub=0.75, ylim=c(0,lim), ylab="fps", xlab="# of active tilemaps")');
               WriteLn(PerformanceOutFile, 'grid(fps_plot, nx=NA, ny=NULL, col="darkgray", lty = 3, lwd=0.8)');
               WriteLn(PerformanceOutFile, 'barplot(fps, mgp=c(2,1,0), beside=T, yaxp=c(0, lim , lim/60), col=c("deepskyblue", "deepskyblue3", "darkorange", "darkorange3"), ylim=c(0,lim), add=T)');
               WriteLn(PerformanceOutFile, 'variation.bar(fps_plot, fps, fps_max, fps_min)');
               WriteLn(PerformanceOutFile, 'legend("topleft", legend=catnames, fill=c("deepskyblue", "deepskyblue3", "darkorange", "darkorange3"), cex=1)');

               CloseFile(PerformanceOutFile);

               for intTilemaps := 0 to FRenderEngine.TilemapCount-1 do
                 FRenderEngine.Tilemaps[intTilemaps].Enabled := True;

               intState := 0;
               intSubState := 0;
             end;
         end;
    end;

    FRenderEngine.DoRender;

    gEventQueue.DoDispatchEvents;
  end;
end;

