## Installation Guide

To get started with this framework, you will need to download a copy of the Lazarus IDE, on Linux, check your package manager, and for Windows, it can be downloaded as a typical installer from [their website](https://lazarus-ide.org/).  After you have it successfully installed and working, continue following this guide.

### Download and install pas2js

In modern versions of Lazarus, 4.0 and higher, check in your **Tools** menu for **Install/Update Pas2Js**, upgrade if you are using an older version of Lazarus, or perform a manual installation as per the official **pas2js** guide.

### Download and install my other Lazarus package jslib

This framework uses some units from my [jslib](https://github.com/kveroneau/jslib), and as of this writing you will need to manually remove the requirement of `pas2js_rtl` from the package, or the compiler will fail to find it and error out, with modern pas2js it is no longer required.

### Install the `RabitHole.lpk` into Lazarus

And finally, locate the `RabitHole.lpk` package file from within this repository, and add it as a requirement to your own program, and then you'll be-able to use all the classes and functions mentioned in this guide.

### New Pas2Js Web Browser Application skeleton

```
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
  { This is called then the "Home" tab is clicked, if it wasn't obvious. }
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
  { Create all the tabs before `inherited DoRun;` as it DoRun will render
    the tabs.  If course, you can always call FTabs.RenderHTML after you
    add tabs if they are added after DoRun. }
  FTabs.AddTab('Home', 'HomeTab', @HomeTab);
  FTabs.AddTab('Example', 'ExampleTab', @ExampleTab);
  FTabs.AddTab('About', 'AboutTab', @AboutTab);
  inherited DoRun;
  { Inless database has been embeeded into the application, no database or
    any tables, such as any of the globals will be available here. The main
    advantage of using the embedded mode is that you have instant access to
    those databases without any additional loads from the server, allowing
    some early logic to take place if needed. }
  HomeTab;
  {{ Code to run in example after framework is done initialization, although
     is optional. }}
end;

procedure TExample1.DoFailure(aMessage: string);
begin
  { This callback API will definately change in the future, as it's too generic,
    and it would be too difficult for someone working with the Framework to know
    what the error was to react to the error, instead of just being able to
    display it, which might be pointless to an end-user. }
  TabBody.setContent(aMessage);
end;

procedure TExample1.GlobalsLoaded;
begin
  { This is called once the globals have been loaded and are ready to be used,
    this means that it can run before the part after `inherited DoRun;` if the
    globals table is embedded into the application. }
  TabBody.setContent(GetGlobal('test'));
end;

begin
  { This initializes the application class, and sets up some settings. }
  Application:=TExample1.Create(nil);
  Application.Initialize;
  Application.DBFile:='website'; { Here we specify a database for the application to use. }
  Application.SaveFlags:=True; { This will allow the saving of the flags which get set. }
  Application.TabID:='RBTabs'; { Bulma specific, allows the app to manage the tabs, not required. }

  { Initialize rbpage before starting the application. }
  SetTabBody('TabBody'); { Can be any div id, does not need to be the body of a tab. }

  Application.Run;
end.
```
