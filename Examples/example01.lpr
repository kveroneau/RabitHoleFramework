program example01;

{$mode objfpc}

uses
  BrowserApp, JS, Classes, SysUtils, Web, rbapp, rbmodels;

type

  { TExample1 }

  TExample1 = class(TRabitHoleApp)
  protected
    procedure DoRun; override;
    procedure DoFailure(aMessage: string); override;
    procedure GlobalsLoaded; override;
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

procedure TExample1.DoFailure(aMessage: string);
begin
  GetHTMLElement('content').innerHTML:=aMessage;
end;

procedure TExample1.GlobalsLoaded;
begin
  GetHTMLElement('content').innerHTML:=GetGlobal('test');
end;

begin
  Application:=TExample1.Create(nil);
  Application.Initialize;
  Application.DBFile:='website';
  Application.SaveFlags:=True;
  {Application.TabID:='tabs';}
  Application.Run;
end.
