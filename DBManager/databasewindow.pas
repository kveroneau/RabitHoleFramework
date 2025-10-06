unit DatabaseWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  DBGrids, StdCtrls, DBCtrls, models, DB, dbftojson, fpjson;

type

  { TDatabaseForm }

  TDatabaseForm = class(TForm)
    SaveVFSBtn: TButton;
    CancelVFSBtn: TButton;
    VFSData: TMemo;
    VFSType: TComboBox;
    VFSTitle: TDBEdit;
    VFSLocation: TDBEdit;
    GlobalsGrid: TDBGrid;
    IconList: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    VFSCreated: TLabel;
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    ExportMenu: TMenuItem;
    ExitMenu: TMenuItem;
    HelpMenu: TMenuItem;
    AboutMenu: TMenuItem;
    DeleteMenu: TMenuItem;
    RefreshMenu: TMenuItem;
    NewMenu: TMenuItem;
    Separator2: TMenuItem;
    VFSMenu: TPopupMenu;
    Tabs: TPageControl;
    Separator1: TMenuItem;
    StatusBar: TStatusBar;
    GlobalsTab: TTabSheet;
    VFSTree: TTreeView;
    VFSTab: TTabSheet;
    procedure AboutMenuClick(Sender: TObject);
    procedure CancelVFSBtnClick(Sender: TObject);
    procedure DeleteMenuClick(Sender: TObject);
    procedure ExportMenuClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GlobalsTabResize(Sender: TObject);
    procedure NewMenuClick(Sender: TObject);
    procedure RefreshMenuClick(Sender: TObject);
    procedure SaveVFSBtnClick(Sender: TObject);
    procedure TabsResize(Sender: TObject);
    procedure VFSTabResize(Sender: TObject);
    procedure VFSTreeDblClick(Sender: TObject);
  private
    {$IFNDEF DATABASE}
    procedure ExportToJSON(DataSet: TDataSet; AFilename: string);
    {$ENDIF}
    procedure FilterDir(const aDir: string);
    procedure LocateFileInTree;
    function AddDirectory(aParent: TTreeNode; aDir: string): TTreeNode;
    procedure ReloadTree;
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
  DATE_FORMAT = 'dddd mmmm d, yyyy "at" hh:nn';
  {$IFDEF EXTDOCS}
  DOC_DIR = 'vfs/';
  {$ENDIF}

{$R *.lfm}

{ TDatabaseForm }

procedure TDatabaseForm.AboutMenuClick(Sender: TObject);
begin
  ShowMessage('RabitHole Database Manager v'+VERSION);
end;

procedure TDatabaseForm.CancelVFSBtnClick(Sender: TObject);
begin
  DBModels.VFSDBF.Cancel;
end;

procedure TDatabaseForm.DeleteMenuClick(Sender: TObject);
var
  r: TModalResult;
begin
  if VFSTree.Selected = Nil then
    Exit;
  LocateFileInTree;
  with DBModels.VFSDBF do
  begin
    r:=MessageDlg('Delete File', 'Are you sure you wanna delete '+string(FieldValues['title'])+'?', mtConfirmation, mbYesNo, 0);
    if r = mrYes then
    begin
      Delete;
      VFSTree.Selected.Delete;
    end;
  end;
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
    jsondb.Add('vfs', ExportDBF(DBModels.VFSDBF));
    tables.Add('vfs');
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
  ExportToJSON(DBModels.VFSDBF, 'vfs');
  {$ENDIF}
end;

procedure TDatabaseForm.FormResize(Sender: TObject);
begin
  Tabs.Width:=ClientWidth;
  Tabs.Height:=ClientHeight-StatusBar.Height; { Might need fixes for use on macOS. }
end;

procedure TDatabaseForm.FormShow(Sender: TObject);
begin
  if DBModels.VFSDBF.EOF then
  begin
    DBModels.CreateFile('Welcome', 'FSRoot', 0, 'Welcome to the File System!');
  end;
  ReloadTree;
end;

procedure TDatabaseForm.GlobalsTabResize(Sender: TObject);
begin
  GlobalsGrid.Width:=GlobalsTab.ClientWidth;
  GlobalsGrid.Height:=GlobalsTab.ClientHeight;
end;

procedure TDatabaseForm.NewMenuClick(Sender: TObject);
var
  node: TTreeNode;
begin
  if VFSTree.Selected = Nil then
    Exit;
  with DBModels.VFSDBF do
  begin
    Append;
    FieldValues['title']:='NewItem';
    FieldValues['typ']:=0;
    if VFSTree.Selected.ImageIndex = 1 then
      FieldValues['wh']:=VFSTree.Selected.Text
    else if (VFSTree.Selected.Parent <> Nil) and (VFSTree.Selected.Parent.ImageIndex = 1) then
      FieldValues['wh']:=VFSTree.Selected.Parent.Text
    else
      FieldValues['wh']:='FSRoot';
    FieldValues['created']:=Now;
    Post;
    VFSCreated.Caption:=FormatDateTime(DATE_FORMAT, FieldValues['created']);
    Edit;
  end;
  node:=VFSTree.Items.AddChild(VFSTree.Selected, 'NewItem');
  node.ImageIndex:=0;
  node.SelectedIndex:=0;
end;

procedure TDatabaseForm.RefreshMenuClick(Sender: TObject);
begin
  ReloadTree;
end;

procedure TDatabaseForm.SaveVFSBtnClick(Sender: TObject);
begin
  with DBModels.VFSDBF do
  begin
    FieldValues['typ']:=VFSType.ItemIndex;
    {$IFDEF EXTDOCS}
    VFSData.Lines.SaveToFile(DOC_DIR+FieldValues['data']);
    {$ELSE}
    FieldValues['data']:=VFSData.Text;
    {$ENDIF}
    Post;
  end;
  with VFSTree.Selected do
  begin
    Text:=VFSTitle.Text;
    ImageIndex:=VFSType.ItemIndex;
    SelectedIndex:=VFSType.ItemIndex;
  end;
end;

procedure TDatabaseForm.TabsResize(Sender: TObject);
begin
  GlobalsTab.Width:=Tabs.ClientWidth;
  GlobalsTab.Height:=Tabs.ClientHeight;
  VFSTab.Width:=Tabs.ClientWidth;
  VFSTab.Height:=Tabs.ClientHeight;
end;

procedure TDatabaseForm.VFSTabResize(Sender: TObject);
begin
  VFSTree.Height:=VFSTab.ClientHeight;
end;

procedure TDatabaseForm.VFSTreeDblClick(Sender: TObject);
begin
  DBModels.VFSDBF.Cancel;
  if (VFSTree.Selected = Nil) or (VFSTree.Selected.Parent = Nil) then
    Exit;
  LocateFileInTree;
  DBModels.VFSDBF.Edit;
  VFSCreated.Caption:=FormatDateTime(DATE_FORMAT, DBModels.VFSDBF.FieldValues['created']);
  VFSType.ItemIndex:=DBModels.VFSDBF.FieldValues['typ'];
  {$IFDEF EXTDOCS}
  VFSData.Lines.LoadFromFile(DOC_DIR+DBModels.VFSDBF.FieldValues['data']);
  {$ELSE}
  VFSData.Text:=DBModels.VFSDBF.FieldValues['data'];
  {$ENDIF}
end;

procedure TDatabaseForm.FilterDir(const aDir: string);
begin
  with DBModels.VFSDBF do
  begin
    Filtered:=False;
    Filter:='wh='+QuotedStr(aDir);
    Filtered:=True;
  end;
end;

procedure TDatabaseForm.LocateFileInTree;
begin
  FilterDir(VFSTree.Selected.Parent.Text);
  DBModels.VFSDBF.Locate('title', VFSTree.Selected.Text, []);
end;

function TDatabaseForm.AddDirectory(aParent: TTreeNode; aDir: string
  ): TTreeNode;
var
  root, node: TTreeNode;
  dirList: Array of TTreeNode;
  typ, i: Integer;
begin
  FilterDir(aDir);
  if aParent = Nil then
  begin
    root:=VFSTree.Items.Add(Nil, aDir);
    root.ImageIndex:=1;
    root.SelectedIndex:=1;
  end
  else
    root:=aParent;
  SetLength(dirList, 0);
  with DBModels.VFSDBF do
  begin
    if not EOF then
      First;
    repeat
      node:=VFSTree.Items.AddChild(root, FieldValues['title']);
      typ:=FieldValues['typ'];
      node.ImageIndex:=typ;
      node.SelectedIndex:=typ;
      if typ = 1 then
      begin
        SetLength(dirList, Length(dirList)+1);
        dirList[Length(dirList)-1]:=node;
      end;
      Next;
    until EOF;
  end;
  for i:=0 to Length(dirList)-1 do
    AddDirectory(dirList[i], dirList[i].Text);
  SetLength(dirList, 0);
  Result:=root;
end;

procedure TDatabaseForm.ReloadTree;
begin
  VFSTree.Items.Clear;
  AddDirectory(Nil, 'FSRoot');
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

