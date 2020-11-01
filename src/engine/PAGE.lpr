library PAGE;

{$mode objfpc}{$H+}

uses
  Classes
  { you can add units after this };

{$include ../PAGEAPI.inc}

function PAGE_Do_Initialize: Boolean;
begin
end;

function PAGE_Do_Finalize: Boolean;
begin
end;

exports
  PAGE_Do_Initialize, PAGE_Do_Finalize;

end.

