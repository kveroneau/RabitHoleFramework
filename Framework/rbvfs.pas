unit rbvfs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, jsontable, ajaxlib, webrouter;

type
  TVFSFileCallback = reference to procedure(data: string);

var
  WebVFS: TJSONTable;

procedure SetVFSDocRoot(aDir: string);
procedure GetVFSFile(AFile, ALocation: string; ACallback: TVFSFileCallback);
function GetVFSFileType: Integer;
procedure RouteVFS(URL: string; aRoute: TRoute; Params: TStrings);
procedure SetInitVFSCallback(ACallback: TVFSFileCallback);

{ Advanced VFS Functions }
function LocateFile(AFile, ALocation: string): Boolean;
function GetVFSDataURL: string;

implementation

var
  doc_root: string;
  req: TWebRequest;
  cb: TVFSFileCallback;
  dirmode: Boolean;

procedure SetCurDir(aDir: string);
begin
  with WebVFS.DataSet do
  begin
    Filtered:=False;
    Filter:='wh='+QuotedStr(aDir);
    Filtered:=True;
  end;
end;

procedure RenderDirList(ACallback: TVFSFileCallback);
var
  url, wh: string;
begin
  wh:=WebVFS.DataSet.FieldByName('title').AsString;
  SetCurDir(wh);
  with TStringList.Create do
  try
    Add('<ul>');
    with WebVFS.DataSet do
    begin
      First;
      repeat
        url:='/vfs/'+wh+'/'+FieldByName('title').AsString;
        Add('<li><a href="#'+url+'">'+FieldByName('title').AsString+'</a></li>');
        Next;
      until EOF;
    end;
    Add('</ul>');
    ACallback(Text);
  finally
    Free;
  end;
end;

procedure SetVFSDocRoot(aDir: string);
begin
  if doc_root <> '' then
    Exit;
  doc_root:=aDir;
end;

procedure LoadFileDone;
begin
  if not req.Complete then
    Exit;
  if req.Status = 200 then
    cb(req.responseText);
  req.Free;
  req:=Nil;
end;

procedure GetVFSFile(AFile, ALocation: string; ACallback: TVFSFileCallback);
begin
  dirmode:=False;
  SetCurDir(ALocation);
  with WebVFS.DataSet do
  begin
    if Locate('title', AFile, []) then
    begin
      if GetVFSFileType = 1 then
      begin
        dirmode:=True;
        cb:=ACallback;
        RenderDirList(ACallback);
        Exit;
      end;
      if doc_root = '' then
        ACallback(FieldByName('data').AsString)
      else if (req = Nil) then
      begin
        req:=TWebRequest.Create(Nil, 'get', doc_root+FieldByName('data').AsString);
        req.OnChange:=@LoadFileDone;
        cb:=ACallback;
        req.DoRequest;
      end;
    end
    else
      ACallback('<b>404</b>: <mark>VFS File Not Located.</mark>');
  end;
end;

function GetVFSFileType: Integer;
begin
  if dirmode then
    Result:=2 { Directory listings should always render as the HTML type. }
  else
    Result:=WebVFS.DataSet.FieldByName('typ').AsInteger;
end;

procedure RouteVFS(URL: string; aRoute: TRoute; Params: TStrings);
begin
  if cb = Nil then
    Exit;
  GetVFSFile(Params.Values['file'], Params.Values['wh'], cb);
end;

procedure SetInitVFSCallback(ACallback: TVFSFileCallback);
begin
  cb:=ACallback;
end;

function LocateFile(AFile, ALocation: string): Boolean;
begin
  SetCurDir(ALocation);
  if WebVFS.DataSet.Locate('title', AFile, []) then
    Result:=True
  else
    Result:=False;
end;

function GetVFSDataURL: string;
begin
  Result:=doc_root+WebVFS.Strings['data'];
end;

initialization
  doc_root:='';
  req:=Nil;
  cb:=Nil;

end.

