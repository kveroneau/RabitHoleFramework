unit rbpage6502;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dom6502, rbpage;

type

  { T6502RBPageOutput }

  T6502RBPageOutput = class(T6502DOMOutput)
  public
    procedure SetTabBody(target: string);
  end;

implementation

{ T6502RBPageOutput }

procedure T6502RBPageOutput.SetTabBody(target: string);
begin
  rbpage.SetTabBody(target);
  AddElement(target);
end;

end.

