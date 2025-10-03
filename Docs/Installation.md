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
```
