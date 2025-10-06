unit rbvfs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, jsontable, ajaxlib;

type
  TVFSFileCallback = reference to procedure(data: string);

var
  WebVFS: TJSONTable;

procedure SetVFSDocRoot(aDir: string);
procedure GetVFSFile(AFile, ALocation: string; ACallback: TVFSFileCallback);

implementation

var
  doc_root: string;
  req: TWebRequest;
  cb: TVFSFileCallback;

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
  cb:=Nil;
end;

procedure GetVFSFile(AFile, ALocation: string; ACallback: TVFSFileCallback);
begin
  with WebVFS.DataSet do
  begin
    Filtered:=False;
    Filter:='wh='+QuotedStr(ALocation);
    Filtered:=True;
    if Locate('title', AFile, []) then
    begin
      if doc_root = '' then
        ACallback(FieldByName('data').AsString)
      else if (req = Nil) and (cb = Nil) then
      begin
        req:=TWebRequest.Create(Nil, 'get', doc_root+FieldByName('data').AsString);
        req.OnChange:=@LoadFileDone;
        cb:=ACallback;
        req.DoRequest;
      end;
    end;
  end;
end;

initialization
  doc_root:='';
  req:=Nil;
  cb:=Nil;

end.

