program example02;

{$mode objfpc}

uses
  BrowserApp, JS, Classes, SysUtils, Web, rbapp6502;

type

  { TExampleApp }

  TExampleApp = class(TRabitHole6502App)

  end;

var
  Application : TExampleApp;

{ TExampleApp }

begin
  Application:=TExampleApp.Create(Nil);
  Application.Initialize;
  Application.SetTabBody('TabBody');
  Application.Run;
end.

