unit pageinit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Spin, PAGEAPI;

type

  { TfrmPageInit }

  TfrmPageInit = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    cbIsAcceleratedRenderer: TCheckBox;
    cbIsVSyncPresent: TCheckBox;
    cbRenderer: TComboBox;
    cbIsSoftwareRenderer: TCheckBox;
    cbSetVSync: TCheckBox;
    gbRendererCapabilities: TGroupBox;
    gbRendererOptions: TGroupBox;
    gbWindowSize: TGroupBox;
    lblRenderer: TLabel;
    pcPageInit: TPageControl;
    pnlDialogButtons: TPanel;
    rbSetRenderAccelerated: TRadioButton;
    rbSetRenderSoftware: TRadioButton;
    rbWindowSizeFixed: TRadioButton;
    rbWindowsizeFullscreen: TRadioButton;
    sedtWindowWidth: TSpinEdit;
    sedtWindowHeight: TSpinEdit;
    tsDisplay: TTabSheet;
    tsRendering: TTabSheet;
    procedure cbRendererChange(Sender: TObject);
  private
    FRendererInfos: TPAGE_RendererInfos;

    function GetRendererInfo(Index: Integer): TPAGE_RendererInfo;
    procedure SetRendererInfo(Index: Integer; AValue: TPAGE_RendererInfo);

  public
    property RendererInfos[Index: Integer]: TPAGE_RendererInfo read
      GetRendererInfo write SetRendererInfo;

    function AddRendererInfo: Integer;
    procedure AddRendererInfos(Infos: TPAGE_RendererInfos);

    procedure DoPopulateRendererInfoCombobox;
  end;

var
  frmPageInit: TfrmPageInit;

implementation

{$R *.lfm}

{ TfrmPageInit }

procedure TfrmPageInit.cbRendererChange(Sender: TObject);
begin
  cbIsSoftwareRenderer.Checked :=
    FRendererInfos[cbRenderer.ItemIndex].isSoftware;

  cbIsAcceleratedRenderer.Checked :=
    FRendererInfos[cbRenderer.ItemIndex].isAccelerated;

  cbIsVSyncPresent.Checked :=
    FRendererInfos[cbRenderer.ItemIndex].isVSyncPresent;
end;

function TfrmPageInit.GetRendererInfo(Index: Integer): TPAGE_RendererInfo;
begin
  if (Index > High(FRendererInfos)) or (Index < Low(FRendererInfos)) then
    Exception.CreateFmt('RendererInfos index out of bounds (%d)', [Index]);

  Result := FRendererInfos[Index];
end;

procedure TfrmPageInit.SetRendererInfo(Index: Integer;
  AValue: TPAGE_RendererInfo);
begin
  if (Index > High(FRendererInfos)) or (Index < Low(FRendererInfos)) then
    Exception.CreateFmt('RendererInfos index out of bounds (%d)', [Index]);

  FRendererInfos[Index] := AValue;
end;

function TfrmPageInit.AddRendererInfo: Integer;
begin
  SetLength(FRendererInfos, Length(FRendererInfos)+1);
  Result := High(FRendererInfos);
end;

procedure TfrmPageInit.AddRendererInfos(Infos: TPAGE_RendererInfos);
begin
  SetLength(FRendererInfos, Length(Infos));
  Move(Infos[0], FRendererInfos[0], Length(Infos)*SizeOf(TPAGE_RendererInfo));
end;

procedure TfrmPageInit.DoPopulateRendererInfoCombobox;
var
  intLoop: Integer;
begin
  cbRenderer.Clear;
  for intLoop := 0 to High(FRendererInfos) do
    cbRenderer.Items.Add(FRendererInfos[intLoop].Name);
  cbRenderer.ItemIndex := 0;
  cbRendererChange(cbRenderer);
end;

end.

