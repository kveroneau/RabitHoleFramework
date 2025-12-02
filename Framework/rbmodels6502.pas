unit rbmodels6502;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Card6502, rbmodels;

type

  TSaveGlobals = reference to Procedure;
  TFlagProc = reference to Procedure(AFlag: string);
  TFlagFunc = reference to Function(AFlag: string): Boolean;

  { T6502RBModelCard }

  T6502RBModelCard = class(T6502Card)
  private
    FSaveGlobals: TSaveGlobals;
    FAddFlag, FDelFlag: TFlagProc;
    FHasFlag: TFlagFunc;
    procedure CheckFlag;
  protected
    function GetCardType: byte; override;
  public
    property SaveGlobals: TSaveGlobals write FSaveGlobals;
    property AddFlag: TFlagProc write FAddFlag;
    property DelFlag: TFlagProc write FDelFlag;
    property HasFlag: TFlagFunc write FHasFlag;
    procedure CardRun; override;
  end;

implementation

{ T6502RBModelCard }

procedure T6502RBModelCard.CheckFlag;
begin
  if FHasFlag(GetStringPtr(2)) then
    Memory[1]:=$ff
  else
    Memory[1]:=$00;
end;

function T6502RBModelCard.GetCardType: byte;
begin
  Result:=$d9;
end;

procedure T6502RBModelCard.CardRun;
var
  op: byte;
begin
  op:=Memory[0];
  if op = 0 then
    Exit;
  case op of
    $f0: SysMemory.LoadString(GetGlobal(GetStringPtr(2))+#0, GetWord(4));
    $f1: SetGlobal(GetStringPtr(2), GetStringPtr(4));
    $f2: FSaveGlobals;
    $f3: FAddFlag(GetStringPtr(2));
    $f4: FDelFlag(GetStringPtr(2));
    $f5: CheckFlag;
  end;
  Memory[0]:=0;
end;

end.

