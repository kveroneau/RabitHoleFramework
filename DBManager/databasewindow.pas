unit DatabaseWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls;

type

  { TDatabaseForm }

  TDatabaseForm = class(TForm)
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    ExportMenu: TMenuItem;
    ExitMenu: TMenuItem;
    HelpMenu: TMenuItem;
    AboutMenu: TMenuItem;
    Separator1: TMenuItem;
    StatusBar: TStatusBar;
    procedure AboutMenuClick(Sender: TObject);
  private

  public

  end;

var
  DatabaseForm: TDatabaseForm;

implementation

const
  VERSION = '0.1b';

{$R *.lfm}

{ TDatabaseForm }

procedure TDatabaseForm.AboutMenuClick(Sender: TObject);
begin
  ShowMessage('RabitHole Database Manager v'+VERSION);
end;

end.

