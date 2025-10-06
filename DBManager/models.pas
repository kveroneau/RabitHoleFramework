unit models;

{ This unit will store all the default shipped databases.
  To add additional tables for your custom website using the Framework,
  it would be best to create a new DataModule, and place your custom components
  there instead of using this, that will make doing an update of the overall
  framework, including this database manager much easier. }

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, dbf, DB;

type

  { TDBModels }

  TDBModels = class(TDataModule)
    VFSDS: TDataSource;
    VFSDBF: TDbf;
    GlobalsDS: TDataSource;
    GlobalsDBF: TDbf;
  private

  public
    procedure CreateFile(ATitle, ALocation: string; AType: Integer; AData: string);
    procedure CreateDir(ATitle, ALocation: string);
  end;

var
  DBModels: TDBModels;

implementation

{$R *.lfm}

{ TDBModels }

procedure TDBModels.CreateFile(ATitle, ALocation: string; AType: Integer;
  AData: string);
begin
  with VFSDBF do
  begin
    Filtered:=False;
    Append;
    FieldValues['title']:=ATitle;
    FieldValues['wh']:=ALocation;
    FieldValues['typ']:=AType;
    FieldByName('created').AsDateTime:=Now;
    FieldValues['data']:=AData;
    Post;
  end;
end;

procedure TDBModels.CreateDir(ATitle, ALocation: string);
begin
  CreateFile(ATitle, ALocation, 1, 'Directory created in the Rabit Hole Database Manager');
end;

end.

