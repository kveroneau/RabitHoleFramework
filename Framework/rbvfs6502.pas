unit rbvfs6502;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Card6502, rbvfs, marked, Web, webrouter;

type

  { T6502RBVFSCard }

  T6502RBVFSCard = class(T6502Card)
  private
    FWebFile: TBytesStream;
    FCurDir: string;
    FLoadText: boolean;
    procedure HandleVFS(data: string);
    procedure HandleRoute(URL: String; aRoute: TRoute; Params: TStrings);
    procedure GetTypeInfo;
    procedure LoadTextFile(AFile: string);
    procedure FileLoaded(Sender: TObject);
    procedure LoadError(Sender: TObject; const AError: string);
    procedure LoadFile;
    procedure LoadVFSFile;
    procedure SetCurDirectory;
    procedure CheckRoute;
    procedure PushRoute;
  protected
    function GetCardType: byte; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CardRun; override;
  end;

implementation

{ T6502RBVFSCard }

procedure T6502RBVFSCard.HandleVFS(data: string);
var
  typ: Integer;
begin
  FLoadText:=False;
  typ:=GetVFSFileType;
  case typ of
    0: TJSHTMLElement(document.getElementById(GetStringPtr(4))).innerHTML:=data;
    4: TJSHTMLElement(document.getElementById(GetStringPtr(4))).innerHTML:=markdown(data);
  end;
end;

procedure T6502RBVFSCard.HandleRoute(URL: String; aRoute: TRoute;
  Params: TStrings);
begin
  SysMemory.LoadString(URL+#0, GetWord($20));
  IRQ;
end;

procedure T6502RBVFSCard.GetTypeInfo;
var
  fname: string;
begin
  Memory[0]:=0;
  fname:=GetStringPtr(2);
  if LocateFile(fname, FCurDir) then
  begin
    Memory[$a]:=GetVFSFileType;
    Memory[1]:=0;
  end
  else
    Memory[1]:=$ff;
end;

procedure T6502RBVFSCard.LoadTextFile(AFile: string);
begin
  FLoadText:=True;
  GetVFSFile(AFile, FCurDir, @HandleVFS);
end;

procedure T6502RBVFSCard.FileLoaded(Sender: TObject);
var
  typ: Integer;
begin
  typ:=GetVFSFileType;
  case typ of
    2: SysMemory.LoadInto(FWebFile, GetWord(4));
    3: SetWord(4, SysMemory.LoadPRG(FWebFile));
  end;
  Memory[0]:=0;
  FreeAndNil(FWebFile);
  Memory[1]:=$00;
end;

procedure T6502RBVFSCard.LoadError(Sender: TObject; const AError: string);
begin
  Memory[0]:=0;
  FreeAndNil(FWebFile);
  Memory[1]:=$ff;
end;

procedure T6502RBVFSCard.LoadFile;
begin
  FWebFile:=TBytesStream.Create;
  FWebFile.LoadFromURL(GetVFSDataURL, True, @FileLoaded, @LoadError);
end;

procedure T6502RBVFSCard.LoadVFSFile;
var
  fname: string;
  typ: Integer;
begin
  fname:=GetStringPtr(2);
  if not LocateFile(fname, FCurDir) then
  begin
    Memory[0]:=$00;
    Memory[1]:=$ff;
    Exit;
  end;
  typ:=GetVFSFileType;
  Memory[$a]:=typ;
  case typ of
    0: LoadTextFile(fname);
    2: LoadFile;
    3: LoadFile;
    4: LoadTextFile(fname);
  else
    Memory[0]:=$00;
    Memory[1]:=$7f;
  end;
end;

procedure T6502RBVFSCard.SetCurDirectory;
begin
  FCurDir:=GetStringPtr(2);
  Memory[0]:=$00;
  Memory[1]:=$00;
end;

procedure T6502RBVFSCard.CheckRoute;
var
  route: string;
begin
  route:=Router.RouteFromURL;
  if route = '' then
    Memory[1]:=0
  else
    Memory[1]:=$ff;
  Memory[0]:=0;
end;

procedure T6502RBVFSCard.PushRoute;
begin
  Router.Push(GetStringPtr(2));
  Memory[0]:=0;
  Memory[1]:=0;
end;

function T6502RBVFSCard.GetCardType: byte;
begin
  Result:=$d8;
end;

constructor T6502RBVFSCard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWebFile:=Nil;
  SetVFSDocRoot('vfs/');
  SetInitVFSCallback(@HandleVFS);
  FCurDir:='FSRoot';
  FLoadText:=False;
  Router.InitHistory(hkHash);
  Router.RegisterRoute('/vfs/:wh/:file', @RouteVFS);
  Router.RegisterRoute('/:path', @HandleRoute, True);
end;

procedure T6502RBVFSCard.CardRun;
var
  op: byte;
begin
  op:=Memory[0];
  if op = 0 then
    Exit;
  if FLoadText then
    Exit;
  if Assigned(FWebFile) then
    Exit;
  case op of
    $40: PushRoute;
    $41: CheckRoute;
    $d0: GetTypeInfo;
    $d1: SetCurDirectory;
    $d2: LoadVFSFile;
    $d4: LoadVFSFile;
    $d6: LoadVFSFile;
    $d7: LoadVFSFile;
    $d8: LoadVFSFile;
  else
    Memory[0]:=$00;
    Memory[1]:=$ff;
  end;
end;

end.

