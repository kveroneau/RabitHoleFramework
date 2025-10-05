unit rbapp;

{$mode objfpc}

interface

uses
  BrowserApp, JS, Classes, SysUtils, Web, bulma, jsontable, rbmodels;

type

  { Setting compile-time DEFINEs for packages is too tricky.
    So, options will be set and used during runtime at the moment. }
  TDatabaseType = (dtTable, dtDatabase);

  { TRabitHoleApp }

  TRabitHoleApp = class(TBrowserApplication)
  private
    FDBFile: string;
    FTabs: TBulmaTabs;
    FTabID: string;
    FDatabaseType: TDatabaseType;
    FDatabase: TJSONDatabase;
    FFlags: TStringList;
    FSaveFlags: Boolean;
    procedure SetDBFile(AValue: string);
    procedure SetTabID(AValue: string);
    procedure DatabaseLoaded;
    procedure DatabaseFailed;
    procedure RestoreGlobals;
  protected
    procedure DoRun; override;
    procedure DoFailure(aMessage: string); virtual; abstract;
    procedure GlobalsLoaded; virtual; abstract;
  public
    property TabID: string read FTabID write SetTabID;
    property DatabaseType: TDatabaseType read FDatabaseType write FDatabaseType;
    property DBFile: string read FDBFile write SetDBFile;
    property SaveFlags: Boolean read FSaveFlags write FSaveFlags;
    constructor Create(aOwner: TComponent); override;
    procedure SaveGlobals;
    procedure AddFlag(AFlag: string);
    procedure DelFlag(AFlag: string);
    function HasFlag(AFlag: string): Boolean;
  end;

implementation

{ TRabitHoleApp }

procedure TRabitHoleApp.SetTabID(AValue: string);
begin
  if FTabID=AValue then Exit;
  if Assigned(FTabs) then
    FTabs.Free;
  FTabs:=TBulmaTabs.Create(Self, AValue);
  FTabID:=AValue;
end;

procedure TRabitHoleApp.DatabaseLoaded;
begin
  if not Assigned(FDatabase) then
    Exit; { This should never happen in regular use, added just-in-case. }
  if not Assigned(RabitVars) then
    RabitVars:=FDatabase.Table['globals'];
  GlobalsLoaded;
end;

procedure TRabitHoleApp.DatabaseFailed;
begin
  DoFailure('Database Failed to Load.');
end;

procedure TRabitHoleApp.RestoreGlobals;
var
  reg, reg2: string;
begin
  { This function here will attempt to check and load in existing globals from
    the browser's local storage.  A bit messy, but works. }
  reg:=window.localStorage.getItem('globals');
  reg2:=String(reg)+'x';
  if reg2 <> 'nullx' then
  begin
    RabitVars:=TJSONTable.Create(Self);
    RabitVars.ParseTable(reg);
    if FSaveFlags then
    begin
      reg:=GetGlobal('flags');
      if reg <> 'null' then
        FFlags.Text:=reg;
    end;
  end;
end;

procedure TRabitHoleApp.SetDBFile(AValue: string);
begin
  if Assigned(FDatabase) then
    Exit;
  if FDBFile=AValue then Exit;
  FDatabaseType:=dtDatabase;
  FDBFile:=AValue;
end;

procedure TRabitHoleApp.DoRun;
begin
  if Assigned(FTabs) then
    FTabs.renderHTML;
  RestoreGlobals;
  if FDatabaseType = dtTable then
  begin
    if not Assigned(RabitVars) then
    begin
      RabitVars:=TJSONTable.Create(Self);
      RabitVars.Datafile:='globals';
      RabitVars.OnSuccess:=@GlobalsLoaded;
      RabitVars.OnFailure:=@DatabaseFailed;
      RabitVars.Active:=True;
    end;
  end
  else if FDatabaseType = dtDatabase then
  begin
    FDatabase:=TJSONDatabase.Create(Self);
    FDatabase.Datafile:=FDBFile;
    FDatabase.OnSuccess:=@DatabaseLoaded;
    FDatabase.OnFailure:=@DatabaseFailed;
    FDatabase.Active:=True;
  end;
end;

constructor TRabitHoleApp.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FTabs:=Nil;
  FDatabaseType:=dtTable;
  FFlags:=TStringList.Create;
end;

procedure TRabitHoleApp.SaveGlobals;
begin
  if FSaveFlags then
    SetGlobal('flags', FFlags.Text);
  window.localStorage.setItem('globals', RabitVars.GetJSON);
end;

procedure TRabitHoleApp.AddFlag(AFlag: string);
begin
  if not HasFlag(AFlag) then
    FFlags.Add(AFlag);
end;

procedure TRabitHoleApp.DelFlag(AFlag: string);
var
  idx: Integer;
begin
  idx:=FFlags.IndexOf(AFlag);
  if idx > -1 then
    FFlags.Delete(idx);
end;

function TRabitHoleApp.HasFlag(AFlag: string): Boolean;
begin
  if FFlags.IndexOf(AFlag) > -1 then
    Result:=True
  else
    Result:=False;
end;

end.

