type
  TPAGE_Initialize = function(): Boolean;
  TPAGE_Finalize = function(): Boolean;
  TPAGE_BindToApp = function(WRAM, VRAM, ROM: Pointer; WRAMSize, VRAMSize,
    ROMSize: Integer): Boolean;

const
  PAGE_INITIALIZE_METHODNAME = 'PAGE_Do_Initialize';
  PAGE_FINALIZE_METHODNAME = 'PAGE_Do_Finalize';
  PAGE_BINDTOAPP_METHODNAME = 'PAGE_Do_BindToApp';
