program pIRCc;

{$mode objfpc}{$H+}

uses
    {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}{$ENDIF}
    Interfaces, // this includes the LCL widgetset
    Forms, unit_frmServer, cmdbox, lnetvisual, LResources, unit_frmChannel,
    unit_TOptionManager;

{$IFDEF WINDOWS}{$R pIRCc.rc}{$ENDIF}

begin
    //{$I pIRCc.lrs}
    Application.Initialize;
    Application.CreateForm(TfrmServer, frmServer);
    Application.Run;
end.

