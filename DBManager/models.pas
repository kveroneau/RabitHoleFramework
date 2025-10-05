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
    GlobalsDS: TDataSource;
    GlobalsDBF: TDbf;
  private

  public

  end;

var
  DBModels: TDBModels;

implementation

{$R *.lfm}

end.

