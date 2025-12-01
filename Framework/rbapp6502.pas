unit rbapp6502;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, rtl.BrowserLoadHelper, rbapp, MOS6502, Memory6502, rom6502,
  CardSlots6502, rbpage6502, rbvfs6502;

type

  { TRabitHole6502App }

  TRabitHole6502App = class(TRabitHoleApp)
  private
    F6502: TMOS6502;
    FMemory: T6502Memory;
    FROM: T6502ROM;
    FSlots: T6502CardSlots;
    FPage: T6502RBPageOutput;
    FVFS: T6502RBVFSCard;
  protected
    procedure DoRun; override;
    procedure DoFailure(aMessage: string); override;
    procedure GlobalsLoaded; override;
    procedure VFSLoaded; override;
  public
    constructor Create(aOwner: TComponent); override;
    procedure SetTabBody(target: string);
  end;

implementation

{$R ROM0.bin}

{ TRabitHole6502App }

procedure TRabitHole6502App.DoRun;
begin
  inherited DoRun;
  F6502.Active:=True;
end;

procedure TRabitHole6502App.DoFailure(aMessage: string);
begin

end;

procedure TRabitHole6502App.GlobalsLoaded;
begin

end;

procedure TRabitHole6502App.VFSLoaded;
begin
  F6502.Running:=True;
end;

constructor TRabitHole6502App.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  VFSTable:='vfs';
  DBFile:='website';
  F6502:=TMOS6502.Create(Self);
  FMemory:=T6502Memory.Create(Self);
  FROM:=T6502ROM.Create(Self);
  FROM.Address:=$f000;
  FROM.ROMFile:='rom0';
  FROM.Active:=True;
  FMemory.ROM[0]:=FROM;
  FMemory.Active:=True;
  F6502.Memory:=FMemory;
  F6502.ResetVector:=FROM.RST_VEC;
  F6502.HaltVector:=$fff0;
  FSlots:=T6502CardSlots.Create(Self);
  FPage:=T6502RBPageOutput.Create(Self);
  FVFS:=T6502RBVFSCard.Create(Self);
  FSlots.Card[0]:=FPage;
  FSlots.Card[6]:=FVFS;
  F6502.Device:=FSlots;
end;

procedure TRabitHole6502App.SetTabBody(target: string);
begin
  FPage.SetTabBody(target);
end;

end.

