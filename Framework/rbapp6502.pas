unit rbapp6502;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, rbapp, MOS6502, Memory6502, rom6502, CardSlots6502;

type

  { TRabitHole6502App }

  TRabitHole6502App = class(TRabitHoleApp)
  private
    F6502: TMOS6502;
    FMemory: T6502Memory;
    FROM: T6502ROM;
    FSlots: T6502CardSlots;
  protected
    procedure DoRun; override;
    procedure DoFailure(aMessage: string); override;
    procedure GlobalsLoaded; override;
    procedure VFSLoaded; override;
  public
    constructor Create(aOwner: TComponent); override;
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
  F6502.Device:=FSlots;
end;

end.

