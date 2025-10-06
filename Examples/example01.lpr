program example01;

{$mode objfpc}

uses
  BrowserApp, JS, Classes, SysUtils, Web, rbapp, rbmodels, rbpage;

type

  { TExample1 }

  TExample1 = class(TRabitHoleApp)
  private
    procedure HomeTab;
    procedure ExampleTab;
    procedure AboutTab;
  protected
    procedure DoRun; override;
    procedure DoFailure(aMessage: string); override;
    procedure GlobalsLoaded; override;
  public
  end;

{ TExample1 }


var
  Application : TExample1;

procedure TExample1.HomeTab;
begin
  TabBody.Write('This is an example of the home tab, welcome!');
end;

procedure TExample1.ExampleTab;
begin
  TabBody.setContent('It is best to start with setContent over write...<br/>');
  TabBody.Write('But, write() can be used after to add additional content.');
end;

procedure TExample1.AboutTab;
begin
  TabBody.setContent('This an example for the Rabit Hole web Framework.');
end;

procedure TExample1.DoRun;
begin
  FTabs.AddTab('Home', 'HomeTab', @HomeTab);
  FTabs.AddTab('Example', 'ExampleTab', @ExampleTab);
  FTabs.AddTab('About', 'AboutTab', @AboutTab);
  inherited DoRun;
  {{ Code to run in example after framework is done initialization, although
     is optional. }}
end;

procedure TExample1.DoFailure(aMessage: string);
begin
  TabBody.setContent(aMessage);
end;

procedure TExample1.GlobalsLoaded;
begin
  TabBody.setContent(GetGlobal('test'));
end;

begin
  Application:=TExample1.Create(nil);
  Application.Initialize;
  Application.DBFile:='website';
  Application.SaveFlags:=True;
  Application.TabID:='RBTabs';

  { Initialize rbpage before starting the application. }
  SetTabBody('TabBody');

  Application.Run;
end.
