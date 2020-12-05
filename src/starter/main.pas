// main unit of starter
unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Spin, DynLibs, LazFileUtils, pageinit, DebugDataHandler, DebugConsole,
  PAGEAPI, APIHelper;

{$include ../PAGEconst.inc}

type
  { TPAGEContextThread }

  TPAGEContextThread = class(TThread)
  private
    FptrPAGEAPI: PPAGE_APIHelper;
  protected
    procedure Execute; override;
  public
    property APIHelper: PPAGE_APIHelper read FptrPAGEAPI write FptrPAGEAPI;
  end;

  { TfrmMain }

  TfrmMain = class(TForm)
    btnGameLoopRun: TButton;
    btnGameLoopPause: TButton;
    btnGameLoopNextFrame: TButton;
    btnGameLoopX2: TButton;
    btnPageBindUnbind: TButton;
    btnPageInitializeFinalize: TButton;
    btnSelectROM: TButton;
    cbOverrideFPS: TCheckBox;
    gbGameLoopControl: TGroupBox;
    gbPAGE: TGroupBox;
    gbSettingsWRAM: TGroupBox;
    gbGameLoopSpeed: TGroupBox;
    gbSettingsVRAM: TGroupBox;
    gbROM: TGroupBox;
    lblsOverrideFPS: TLabel;
    lbldRomStatus: TLabel;
    lblsKiB: TLabel;
    lbldPageInitializedStatus: TLabel;
    lbldPageBindStatus: TLabel;
    lblsKiB1: TLabel;
    lblsPageBindStatus: TLabel;
    OpenROMDialog: TOpenDialog;
    OpenPageDialog: TOpenDialog;
    pnlRomFile: TPanel;
    pnlOverrideFPS: TPanel;
    rbVRAMUseCustomSize: TRadioButton;
    rbWRAMUseCustomSize: TRadioButton;
    rbVRAMUseRomSetting: TRadioButton;
    rbWRAMUseRomSetting: TRadioButton;
    seOverrideFPS: TSpinEdit;
    seVRAMCustomSize: TSpinEdit;
    seWRAMCustomSize: TSpinEdit;
    procedure btnGameLoopPauseClick(Sender: TObject);
    procedure btnGameLoopRunClick(Sender: TObject);
    procedure btnPageBindUnbindClick(Sender: TObject);
    procedure btnPageInitializeFinalizeClick(Sender: TObject);
    procedure btnSelectROMClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbVRAMUseCustomSizeChange(Sender: TObject);
    procedure rbWRAMUseCustomSizeChange(Sender: TObject);
  private
    FboolIsPageBound, FboolIsPageInitialized: Boolean;
    hPage: TLibHandle;
    FPAGEAPI: TPAGE_APIHelper;
    FDesiredWRAMSize: Integer;
    FDesiredVRAMSize: Integer;

    FptrWRAM, FptrVRAM, FptrROM: Pointer;

    FPAGEThread: TPAGEContextThread;
    FboolPageThreadHasTerminated: Boolean;

    function DoBindPageMethods(PageFileName: String = ''): Boolean;

    procedure DoSetGUIPageBoundUnbound(isPageBound: Boolean);
    procedure DoSetGUIPageInitializeFinalize(isPageInitialized: Boolean);
    procedure DoNilPageMethodHandles;
    procedure PageThreadTerminate(Sender: TObject);
  public

  end;

var
  frmMain: TfrmMain;

const
  PageLibName: String = {$ifdef LINUX}'./lib' +{$endif} 'page.' + SharedSuffix;
  PageROMExtension: String = '.' + SharedSuffix;

implementation

{$R *.lfm}

{ TPAGEContextThread }

procedure TPAGEContextThread.Execute;
begin
  FptrPAGEAPI^.PAGEEnterGameLoop();
end;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FPAGEAPI := TPAGE_APIHelper.Create;
  FboolIsPageBound := False;
  FboolIsPageInitialized := False;
  FDesiredVRAMSize := 612*KB;
  FDesiredWRAMSize := 2*MB;

  FptrWRAM := nil;
  FptrVRAM := nil;
  FptrROM := nil;

  OpenPageDialog.Filter := Format('Shared Library (*.%0:s)|*.%0:s',
    [SharedSuffix]);
  hPage := NilHandle;
  OpenRomDialog.Filter := OpenRomDialog.Filter + '|' +
    Format('Shared Library (*.%0:s)|*.%0:s', [SharedSuffix]);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FPAGEAPI.Free;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  frmDebugConsole.Show;
end;

procedure TfrmMain.btnPageBindUnbindClick(Sender: TObject);
var
  boolLibOpenDialogExecuted: Boolean = True;
begin
  { TODO: Refactor }
  if FptrWRAM = nil then
  begin
    // Hard-Initialize - do be done dynamically
    FptrWRAM := AllocMem(2*MB);
    FptrVRAM := AllocMem(612*KB);
  end;


  if not FboolIsPageBound then
  begin
    FboolIsPageBound := DoBindPageMethods(PageLibName);
    if not FboolIsPageBound then
    begin
      repeat
        boolLibOpenDialogExecuted := OpenPageDialog.Execute;
        PageLibName := OpenPageDialog.FileName;
        FboolIsPageBound := DoBindPageMethods(PageLibName);
      until (not boolLibOpenDialogExecuted) or (FboolIsPageBound);
    end;

    if (FboolIsPageBound) then //and (FboolIsPageInitialized) then
    begin
      if not FPAGEAPI.PAGEBindToApp(FptrWRAM, FptrVRAM, nil, 2*MB, 612*KB,
        0) then
      begin
        gDebugDataHandler.ErrorInfoText(Self, 'Failed to bind PAGE. ' +
          'PAGEBindToApp returned FALSE');
      end
    end;
  end
  else
  begin
    if UnloadLibrary(hPage) then
    begin
      if Assigned(FPAGEThread) then
        FreeAndNil(FPAGEThread);
      hPage := NilHandle;
      FboolIsPageBound := False;
      DoNilPageMethodHandles;
      gDebugDataHandler.DebugInfoText(Self, 'Unloaded library');
    end
    else
      gDebugDataHandler.ErrorInfoText(Self, 'Failed to unload library');
  end;

  DoSetGUIPageBoundUnbound(FboolIsPageBound);
end;

procedure TfrmMain.btnGameLoopRunClick(Sender: TObject);
begin
  if (Assigned(FPAGEThread)) and (FboolPageThreadHasTerminated) then
  begin
    FreeAndNil(FPAGEThread);
  end;

  if not Assigned(FPAGEThread) then
  begin
    FboolPageThreadHasTerminated := False;
    FPAGEThread := TPAGEContextThread.Create(True);
    FPAGEThread.OnTerminate := @PageThreadTerminate;
    FPAGEThread.APIHelper := @FPAGEAPI;
    TPAGE_WRAMLayout(FptrWRAM^).boolExitGameLoop := False;
    FPAGEThread.Start;
  end;
end;

procedure TfrmMain.btnGameLoopPauseClick(Sender: TObject);
begin
  TPAGE_WRAMLayout(FptrWRAM^).boolExitGameLoop := True;
end;

procedure TfrmMain.btnPageInitializeFinalizeClick(Sender: TObject);
var
  RendererInfos: TPAGE_RendererInfos;
  RenderSettings: TPAGE_RenderSettings;
  WindowSettings: TPAGE_WindowSettings;
begin
  if FboolIsPageInitialized then
  begin

  end
  else
  begin
    // GetRendererInfos and populate combobox
    FPAGEAPI.PAGEGetRendererInfos(@RendererInfos);
    frmPageInit.cbRenderer.Clear;
    frmPageInit.AddRendererInfos(RendererInfos);
    frmPageInit.DoPopulateRendererInfoCombobox;


    if frmPageInit.ShowModal = mrOk then
      with frmPageInit do
      begin
        RenderSettings.RendererNumber := cbRenderer.ItemIndex;
        RenderSettings.EnableVSync := cbSetVSync.Checked;
        RenderSettings.RenderAccelerated := rbSetRenderAccelerated.Checked;
        RenderSettings.RenderSizeHeight := sedtRenderHeight.Value;
        RenderSettings.RenderSizeWidth := sedtRenderWidth.Value;

        WindowSettings.Fullscreen := rbWindowsizeFullscreen.Checked;
        WindowSettings.WindowSizeWidth := sedtWindowWidth.Value;
        WindowSettings.WindowSizeHeight := sedtWindowHeight.Value;
        WindowSettings.WindowX := frmMain.Left+frmMain.Width;
        WindowSettings.WindowY := Top;
        WindowSettings.WindowTitle := 'Pixelanstalt Game Engine';


        FboolIsPageInitialized := FPAGEAPI.PAGEInitialize(RenderSettings,
          WindowSettings);
        DoSetGUIPageInitializeFinalize(FboolIsPageInitialized);
      end;
  end;
end;

procedure TfrmMain.btnSelectROMClick(Sender: TObject);
begin
  if OpenRomDialog.Execute then
  begin
    { TODO: Handle ROM file input }
  end;
end;

procedure TfrmMain.rbVRAMUseCustomSizeChange(Sender: TObject);
begin
  seVRAMCustomSize.Enabled := rbVRAMUseCustomSize.Checked;
  lblsKiB.Enabled := rbVRAMUseCustomSize.Checked;
end;

procedure TfrmMain.rbWRAMUseCustomSizeChange(Sender: TObject);
begin
  seWRAMCustomSize.Enabled := rbWRAMUseCustomSize.Checked;
  lblsKiB1.Enabled := rbWRAMUseCustomSize.Checked;
end;

function TfrmMain.DoBindPageMethods(PageFileName: String): Boolean;
begin
  Result := False;
  hPage := LoadLibrary(PageFileName);
  gDebugDataHandler.DebugInfoText(Self, '(DoBindPage) Load Library handle: $' +
    IntToHex(hPage, 8));
  if hPage <> NilHandle then
  begin
    FPAGEAPI.GetMethodPointersFromLibrary(hPage);

    gDebugDataHandler.DebugInfoText(Self, '(DoBindPage) Method handles: ' +
      FPAGEAPI.ReturnAddressesAsString);

    Result := FPAGEAPI.isMethodPointerArrayValid;

    if Result then
    begin
      FPAGEAPI.PAGEAddEventQueueListener(@EventQueueDispatch, [psDebug]);
      frmDebugConsole.CastEvent := FPAGEAPI.PAGECastEvent;
    end;
  end;
end;

procedure TfrmMain.DoSetGUIPageBoundUnbound(isPageBound: Boolean);
begin
  if isPageBound then
  begin
    btnPageBindUnbind.Caption := 'Unbind';
    lbldPageBindStatus.Caption := 'Bound';
    lbldPageBindStatus.Font.Color := clGreen;
    btnPageInitializeFinalize.Enabled := True;
  end
  else
  begin
    btnPageBindUnbind.Caption := 'Bind';
    lbldPageBindStatus.Caption := 'Unbound';
    lbldPageBindStatus.Font.Color := clRed;
    btnPageInitializeFinalize.Enabled := False;
  end;

  DoSetGUIPageInitializeFinalize(FboolIsPageInitialized);
end;

procedure TfrmMain.DoSetGUIPageInitializeFinalize(isPageInitialized: Boolean);
begin
  if isPageInitialized then
  begin
    btnPageInitializeFinalize.Caption := 'Finalize';
    lbldPageInitializedStatus.Caption := 'Initialized';
    lbldPageInitializedStatus.Font.Color := clGreen;
    btnGameLoopRun.Enabled := FboolIsPageBound;
    btnGameLoopPause.Enabled := FboolIsPageBound;
  end
  else
  begin
    btnPageInitializeFinalize.Caption := 'Initialize ...';
    lbldPageInitializedStatus.Caption := 'Uninitialized';
    lbldPageInitializedStatus.Font.Color := clRed;
    btnGameLoopRun.Enabled := False;
    btnGameLoopPause.Enabled := False;
  end;
end;

procedure TfrmMain.DoNilPageMethodHandles;
begin
  FPAGEAPI.DoNilPointerArray;
end;

procedure TfrmMain.PageThreadTerminate(Sender: TObject);
begin
  FboolPageThreadHasTerminated := True;
  gDebugDataHandler.DebugInfoText(Self, '(PAGEThread) Thread terminated');
end;

end.

