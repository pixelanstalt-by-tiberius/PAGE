unit pageinit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls;

type

  { TfrmPageInit }

  TfrmPageInit = class(TForm)
    btnOk: TButton;
    Button1: TButton;
    pcPageInit: TPageControl;
    pnlDialogButtons: TPanel;
    tsDisplay: TTabSheet;
    tsRendering: TTabSheet;
  private

  public

  end;

var
  frmPageInit: TfrmPageInit;

implementation

{$R *.lfm}

end.

