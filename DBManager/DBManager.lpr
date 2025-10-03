program DBManager;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, DatabaseWindow
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='RabitHole Database Manager';
  Application.Scaled:=True;
  {$PUSH}{$WARN 5044 OFF}
  Application.MainFormOnTaskbar:=True;
  {$POP}
  Application.Initialize;
  Application.CreateForm(TDatabaseForm, DatabaseForm);
  Application.Run;
end.

