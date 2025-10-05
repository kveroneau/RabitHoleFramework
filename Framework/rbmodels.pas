unit rbmodels;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, jsontable;

var
  RabitVars: TJSONTable;

function GetGlobal(AKey: string): string;
procedure SetGlobal(AKey, AValue: string);

implementation

function GetGlobal(AKey: string): string;
begin
  Result:=String(RabitVars.DataSet.Lookup('KEY', AKey, 'VALUE'));
end;

procedure SetGlobal(AKey, AValue: string);
begin
  with RabitVars.DataSet do
  begin
    if Locate('KEY', AKey, []) then
    begin
      Edit;
      FieldByName('VALUE').AsString:=AValue;
      Post;
    end
    else
    begin
      Append;
      FieldByName('KEY').AsString:=AKey;
      FieldByName('VALUE').AsString:=AValue;
      Post;
    end;
  end;
end;

initialization
  RabitVars:=Nil;

end.

