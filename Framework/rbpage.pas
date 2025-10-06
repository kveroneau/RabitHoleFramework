unit rbpage;

{ This unit file will provide various helpers for rendering information to the
  page.  Although some of these use the TBulmaWidget class, this class, and most
  of the non-Bulma specific classes which will be used here, do not require Bulma
  to be used with the framework, and any CSS framework or HTML/CSS template will
  be perfectly fine to use. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Web, bulma;

var
  TabBody: TBulmaWidget; { Module-level variable for ease of use anywhere. }

procedure SetTabBody(target: string);

implementation

{ Helper function to be used during initialization, does not need to be set to
  specifically a "Tab Body", but rather where the main content should be place
  regardless if Tabs are used.  Currently not used by the Framework, but might
  one day. }
procedure SetTabBody(target: string);
begin
  if Assigned(TabBody) then
    TabBody.Free;
  TabBody:=TBulmaWidget.Create(Nil, target);
end;

end.

