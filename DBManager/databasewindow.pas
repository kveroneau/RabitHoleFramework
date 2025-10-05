unit DatabaseWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  DBGrids, models, DB, dbftojson, fpjson;

type

  { TDatabaseForm }

  TDatabaseForm = class(TForm)
    GlobalsGrid: TDBGrid;
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    ExportMenu: TMenuItem;
    ExitMenu: TMenuItem;
    HelpMenu: TMenuItem;
    AboutMenu: TMenuItem;
    Tabs: TPageControl;
    Separator1: TMenuItem;
    StatusBar: TStatusBar;
    GlobalsTab: TTabSheet;
    procedure AboutMenuClick(Sender: TObject);
    procedure ExportMenuClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure GlobalsTabResize(Sender: TObject);
    procedure TabsResize(Sender: TObject);
  private
    {$IFNDEF DATABASE}
    procedure ExportToJSON(DataSet: TDataSet; AFilename: string);
    {$ENDIF}
  public

  end;

var
  DatabaseForm: TDatabaseForm;

implementation

const
  VERSION = '0.1b';
  OUTPUT_PATH = '../Examples/';
  {$IFDEF DATABASE}
  DB_FILENAME = 'website.json';
  {$ENDIF}

{$R *.lfm}

{ TDatabaseForm }

procedure TDatabaseForm.AboutMenuClick(Sender: TObject);
begin
  ShowMessage('RabitHole Database Manager v'+VERSION);
end;

procedure TDatabaseForm.ExportMenuClick(Sender: TObject);
{$IFDEF DATABASE}
var
  jsondb: TJSONObject;
  tables: TJSONArray;
{$ENDIF}
begin
  { If you add any additional databases/tables to your custom static website,
    you will need to add the appropriate export calls here.
    Future video tutorial on how to add a new table to the website will show. }
  {$IFDEF DATABASE}
  jsondb:=TJSONObject.Create;
  tables:=TJSONArray.Create;
  try
    jsondb.Add('globals', ExportDBF(DBModels.GlobalsDBF));
    tables.Add('globals');
    jsondb.Add('tables', tables);
    with TStringStream.Create(jsondb.AsJSON) do
    try
      SaveToFile(OUTPUT_PATH+DB_FILENAME);
    finally
      Free;
    end;
  finally
    jsondb.Free;
  end;
  {$ELSE}
  ExportToJSON(DBModels.GlobalsDBF, 'globals');
  {$ENDIF}
end;

procedure TDatabaseForm.FormResize(Sender: TObject);
begin
  Tabs.Width:=ClientWidth;
  Tabs.Height:=ClientHeight-StatusBar.Height; { Might need fixes for use on macOS. }
end;

procedure TDatabaseForm.GlobalsTabResize(Sender: TObject);
begin
  GlobalsGrid.Width:=GlobalsTab.ClientWidth;
  GlobalsGrid.Height:=GlobalsTab.ClientHeight;
end;

procedure TDatabaseForm.TabsResize(Sender: TObject);
begin
  GlobalsTab.Width:=Tabs.ClientWidth;
  GlobalsTab.Height:=Tabs.ClientHeight;
end;

{$IFNDEF DATABASE}
{ Technically "SmartLinking" should prevent this from being in the output if
  it isn't used, but I generally prefer to be explicit in my coding and using
  a DEFINE to alter the output, as it is more clear on what's being compiled. }
procedure TDatabaseForm.ExportToJSON(DataSet: TDataSet; AFilename: string);
var
  json: TJSONObject;
begin
  json:=ExportDBF(DataSet);
  with TStringStream.Create(json.AsJSON) do
  try
    SaveToFile(OUTPUT_PATH+AFilename+'.json');
  finally
    Free;
  end;
  json.Free;
end;
{$ENDIF}

end.

