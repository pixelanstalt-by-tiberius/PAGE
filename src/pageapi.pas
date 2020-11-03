unit PAGEAPI;

{$mode objfpc}{$H+}

interface

// Structures
type
  TPAGE_RendererInfo = record
    Name: PChar;
    isSoftware: Boolean;
    isAccelerated: Boolean;
    isVSyncPresent: Boolean;
  end;

  TPAGE_RendererInfos = array of TPAGE_RendererInfo;
  PPAGE_RendererInfos = ^TPAGE_RendererInfos;

// Methods
type
  TPAGE_Initialize = function(): Boolean;
  TPAGE_Finalize = function(): Boolean;
  TPAGE_BindToApp = function(WRAM, VRAM, ROM: Pointer; WRAMSize, VRAMSize,
    ROMSize: Integer): Boolean;
  TPAGE_GetRendererInfos = function(RendererInfos: PPAGE_RendererInfos): Boolean;


const
  PAGE_INITIALIZE_METHODNAME = 'PAGE_Do_Initialize';
  PAGE_FINALIZE_METHODNAME = 'PAGE_Do_Finalize';
  PAGE_BINDTOAPP_METHODNAME = 'PAGE_Do_BindToApp';
  PAGE_GETRENDERERINFOS_METHODNAME = 'PAGE_Do_GetRendererInfos';

implementation

end.

