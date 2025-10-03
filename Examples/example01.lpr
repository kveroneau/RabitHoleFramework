program example01;

{$mode objfpc}

uses
  BrowserApp, JS, Classes, SysUtils, Web, rbapp;

type

  { TExample1 }

  TExample1 = class(TRabitHoleApp)
  protected
    procedure DoRun; override;
  public
  end;

{ TExample1 }


var
  Application : TExample1;

procedure TExample1.DoRun;
begin
  inherited DoRun;
  {{ Code to run in example after framework is done initialization, although
     is optional. }}
end;

begin
  Application:=TExample1.Create(nil);
  Application.Initialize;
  Application.TabID:='tabs';
  Application.Run;
end.
