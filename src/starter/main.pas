unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Spin, DynLibs, LazFileUtils, pageinit, DebugDataHandler, DebugConsole,
  PAGEAPI;

//{$include ../PAGEAPI.inc}
{$include ../PAGEconst.inc}

type

  { TPAGE_Methods }

  TPAGE_Methods = record
    PAGEInitialize: TPAGE_Initialize;
    PAGEFinalize: TPAGE_Finalize;
    PAGEBindToApp: TPAGE_BindToApp;
    PAGEGetRendererInfos: TPAGE_GetRendererInfos;
    PAGEEnterGameLoop: TPAGE_EnterGameLoop;
    PAGEAddEventQueueListener: TPAGE_AddEventQueueListener;
  end;

  { TPAGEContextThread }

  TPAGEContextThread = class(TThread)
  private
    FPageMethods: TPAGE_Methods;
  protected
    procedure Execute; override;
  public
    property PageMethods: TPAGE_Methods read FPageMethods write FPageMethods;
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
    procedure FormShow(Sender: TObject);
    procedure rbVRAMUseCustomSizeChange(Sender: TObject);
    procedure rbWRAMUseCustomSizeChange(Sender: TObject);
  private
    FboolIsPageBound, FboolIsPageInitialized: Boolean;
    hPage: TLibHandle;
    FPageMethods: TPAGE_Methods;
    FDesiredWRAMSize: Integer;
    FDesiredVRAMSize: Integer;

    FptrWRAM, FptrVRAM, FptrROM: Pointer;

    FPAGEThread: TPAGEContextThread;

    function DoBindPageMethods(PageFileName: String = ''): Boolean;

    procedure DoSetGUIPageBoundUnbound(isPageBound: Boolean);
    procedure DoSetGUIPageInitializeFinalize(isPageInitialized: Boolean);
    procedure DoNilPageMethodHandles;
    {function DoUnbindPage: Boolean;
    function DoInitializePage: Boolean;
    function DoFinalizePage: Boolean; }
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
  FPageMethods.PAGEEnterGameLoop;
end;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
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

    { DONE: Overthink if memory allocation should be placed in PAGE or in
            initialize }
    if (FboolIsPageBound) then //and (FboolIsPageInitialized) then
    begin
      if not FPAGEMethods.PAGEBindToApp(FptrWRAM, FptrVRAM, nil, 2*MB, 612*KB,
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
      hPage := NilHandle;
      FboolIsPageBound := False;
      DoNilPageMethodHandles;
      FPAGEThread.Free;
      gDebugDataHandler.DebugInfoText(Self, 'Unloaded library');
    end
    else
      gDebugDataHandler.ErrorInfoText(Self, 'Failed to unload library');
  end;

  DoSetGUIPageBoundUnbound(FboolIsPageBound);
end;

procedure TfrmMain.btnGameLoopRunClick(Sender: TObject);
begin
  FPAGEThread := TPAGEContextThread.Create(True);
  FPAGEThread.FreeOnTerminate := True;
  FPAGEThread.PageMethods := FPageMethods;
  TPAGE_WRAMLayout(FptrWRAM^).boolExitGameLoop := False;
  FPAGEThread.Start;
end;

procedure TfrmMain.btnGameLoopPauseClick(Sender: TObject);
begin
  TPAGE_WRAMLayout(FptrWRAM^).boolExitGameLoop := True;
end;

procedure TfrmMain.btnPageInitializeFinalizeClick(Sender: TObject);
var
  RendererInfos: TPAGE_RendererInfos;
  intLoop: Integer;
begin
  if FboolIsPageInitialized then
  begin

  end
  else
  begin
    // GetRendererInfos and populate combobox
    FPAGEMethods.PAGEGetRendererInfos(@RendererInfos);
    frmPageInit.cbRenderer.Clear;
    { TODO: Make more pretty }
    for intLoop := 0 to High(RendererInfos) do
    begin
      frmPageInit.RendererInfos[frmPageInit.AddRendererInfo] :=
        RendererInfos[intLoop];
    end;
    frmPageInit.DoPopulateRendererInfoCombobox;


    if frmPageInit.ShowModal = mrOk then
      with frmPageInit do
      begin
        FboolIsPageInitialized := FPageMethods.PAGEInitialize(
          cbRenderer.ItemIndex, rbSetRenderAccelerated.Checked,
          cbSetVSync.Checked, rbWindowsizeFullscreen.Checked, Left+Width, Top,
          sedtWindowWidth.Value, sedtWindowHeight.Value);
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
    { TODO: Encapsulate methods in class and enumarate method pointer and
            names in array(s) }
    FPAGEMethods.PAGEInitialize := TPAGE_Initialize(GetProcAddress(hPage,
      PAGE_INITIALIZE_METHODNAME));
    FPAGEMethods.PAGEFinalize := TPAGE_Finalize(GetProcAddress(hPage,
      PAGE_FINALIZE_METHODNAME));
    FPAGEMethods.PAGEBindToApp := TPAGE_BindToApp(GetProcAddress(hPage,
      PAGE_BINDTOAPP_METHODNAME));
    FPAGEMethods.PAGEGetRendererInfos := TPAGE_GetRendererInfos(GetProcAddress(
      hPage, PAGE_GETRENDERERINFOS_METHODNAME));
    FPAGEMethods.PAGEEnterGameLoop := TPAGE_EnterGameLoop(GetProcAddress(
      hPage, PAGE_ENTERGAMELOOP_METHODNAME));
    FPAGEMethods.PAGEAddEventQueueListener := TPAGE_AddEventQueueListener(
      GetProcAddress(hPage, PAGE_ADDEVENTQUEUELISTENER_METHODNAME));

    gDebugDataHandler.DebugInfoText(Self, '(DoBindPage) Method handles: ' +
      Format('$%x $%x $%x $%x $%x $%x', [PtrUInt(FPAGEMethods.PAGEInitialize),
      PtrUInt(FPAGEMethods.PAGEFinalize), PtrUInt(FPAGEMethods.
      PAGEBindToApp), PtrUInt(FPAGEMethods.PAGEGetRendererInfos),
      PtrUInt(FPAGEMethods.PAGEEnterGameLoop),
      PtrUInt(FPAGEMethods.PAGEAddEventQueueListener)]));

    Result := (FPAGEMethods.PAGEInitialize <> nil) and
      (FPAGEMethods.PAGEFinalize <> nil) and
      (FPAGEMethods.PAGEBindToApp <> nil) and
      (FPAGEMethods.PAGEGetRendererInfos <> nil) and
      (FPAGEMethods.PAGEEnterGameLoop <> nil) and
      (FPAGEMethods.PAGEAddEventQueueListener <> nil);

    if Result then
      FPAGEMethods.PAGEAddEventQueueListener(@EventQueueDispatch, [psDebug]);
  end;
end;

procedure TfrmMain.DoSetGUIPageBoundUnbound(isPageBound: Boolean);
begin
  { TODO: If PAGE is initialized and then unbound, controls for game loop must
          be disabled }
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
  with FPageMethods do
  begin
    PAGEInitialize := nil;
    PAGEFinalize := nil;
    PAGEBindToApp := nil;
    PAGEGetRendererInfos := nil;
  end;
end;

end.

