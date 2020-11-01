unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Spin, DynLibs, LazFileUtils, pageinit, DebugDataHandler, DebugConsole;

{$include ../PAGEAPI.inc}
{$include ../PAGEconst.inc}

type
  TPAGE_Methods = record
    PAGEInitialize: TPAGE_Initialize;
    PAGEFinalize: TPAGE_Finalize;
    PAGEBindToApp: TPAGE_BindToApp;
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

    function DoBindPage(PageFileName: String = ''): Boolean;

    procedure DoSetGUIPageBoundUnbound(isPageBound: Boolean);
    procedure DoSetGUIPageInitializeFinalize(isPageInitialized: Boolean);
    {function DoUnbindPage: Boolean;
    function DoInitializePage: Boolean;
    function DoFinalizePage: Boolean; }
  public

  end;

var
  frmMain: TfrmMain;

const
  PageLibName: String = 'page.' + SharedSuffix;
  PageROMExtension: String = '.' + SharedSuffix;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FboolIsPageBound := False;
  FboolIsPageInitialized := False;
  FDesiredVRAMSize := 612*KB;
  FDesiredWRAMSize := 2*MB;

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
  if not FboolIsPageBound then
  begin
    FboolIsPageBound := DoBindPage(PageLibName);
    if not FboolIsPageBound then
    begin
      repeat
        boolLibOpenDialogExecuted := OpenPageDialog.Execute;
        FboolIsPageBound := DoBindPage(OpenPageDialog.FileName);
      until (not boolLibOpenDialogExecuted) or (FboolIsPageBound);
    end;

    if (FboolIsPageBound) and (FboolIsPageInitialized) then
      FPAGEMethods.PAGEBindToApp(nil, nil, nil, 0, 0, 0);
  end
  else
  begin
    if UnloadLibrary(hPage) then
    begin
      hPage := NilHandle;
      FboolIsPageBound := False;
      { TODO: Set method handles to nil }
    end
    else
      MessageDlg('Failed to unload library', mtError, [mbOk], 0);
  end;

  DoSetGUIPageBoundUnbound(FboolIsPageBound);
end;

procedure TfrmMain.btnPageInitializeFinalizeClick(Sender: TObject);
begin
  if frmPageInit.ShowModal = mrOk then
  begin

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

function TfrmMain.DoBindPage(PageFileName: String): Boolean;
begin
  Result := False;
  hPage := LoadLibrary(PageFileName);
  gDebugDataHandler.DebugInfoText(Self, '(DoBindPage) Load Library handle: $' +
    IntToHex(hPage, 8));
  if hPage <> NilHandle then
  begin
    { TODO: Bind functions and check handles }
    FPAGEMethods.PAGEInitialize := TPAGE_Initialize(GetProcAddress(hPage,
      PAGE_INITIALIZE_METHODNAME));
    FPAGEMethods.PAGEFinalize := TPAGE_Finalize(GetProcAddress(hPage,
      PAGE_FINALIZE_METHODNAME));
    FPAGEMethods.PAGEBindToApp := TPAGE_BindToApp(GetProcAddress(hPage,
      PAGE_BINDTOAPP_METHODNAME));

    gDebugDataHandler.DebugInfoText(Self, '(DoBindPage) Method handles: ' +
      Format('$%x $%x $%x', [PtrUInt(FPAGEMethods.PAGEInitialize),
      PtrUInt(FPAGEMethods.PAGEFinalize), PtrUInt(FPAGEMethods.
      PAGEBindToApp)]));


    Result := (FPAGEMethods.PAGEInitialize <> nil) and
      (FPAGEMethods.PAGEFinalize <> nil) and
      (FPAGEMethods.PAGEBindToApp <> nil);
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
  end
  else
  begin
    btnPageInitializeFinalize.Caption := 'Initialize ...';
    lbldPageInitializedStatus.Caption := 'Uninitialized';
    lbldPageInitializedStatus.Font.Color := clRed;
  end;

end;

end.

