unit rbapp;

{$mode objfpc}

interface

uses
  BrowserApp, JS, Classes, SysUtils, Web, bulma;

type

  { TRabitHoleApp }

  TRabitHoleApp = class(TBrowserApplication)
  private
    FTabs: TBulmaTabs;
    FTabID: string;
    procedure SetTabID(AValue: string);
  protected
    procedure DoRun; override;
  public
    property TabID: string read FTabID write SetTabID;
    constructor Create(aOwner: TComponent); override;
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

procedure TRabitHoleApp.DoRun;
begin
  if Assigned(FTabs) then
    FTabs.renderHTML;
end;

constructor TRabitHoleApp.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FTabs:=Nil;
end;

end.

