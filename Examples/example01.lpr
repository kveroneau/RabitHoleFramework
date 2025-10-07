program example01;

{$mode objfpc}

uses
  BrowserApp, JS, Classes, SysUtils, Web, rbapp, rbmodels, rbpage, bulma, rbvfs,
  webrouter;

type

  { TExample1 }

  TExample1 = class(TRabitHoleApp)
  private
    inp: TBulmaInput;
    procedure HomeTab;
    procedure DoVisitorName;
    procedure ExampleTab;
    procedure ShowVFS(data: string);
    procedure AboutTab;
  protected
    procedure DoRun; override;
    procedure DoFailure(aMessage: string); override;
    procedure GlobalsLoaded; override;
    procedure VFSLoaded; override;
  public
  end;

{ TExample1 }


var
  Application : TExample1;

procedure TExample1.HomeTab;
begin
  { This is called then the "Home" tab is clicked, if it wasn't obvious. }
  TabBody.Write('This is an example of the home tab, welcome!');
end;

procedure TExample1.DoVisitorName;
var
  visitor: string;
begin
  visitor:=inp.Value;
  if (visitor = '') or (visitor = 'null') then
  begin
    window.alert('It is imparative for this example that you supply your name.');
    Exit;
  end;
  SetGlobal('visitor_name', visitor);
  ExampleTab; { Re-render the tab. }
  {SaveGlobals;} { This can be set to also persist it immediately into the browser. }
end;

procedure TExample1.ExampleTab;
var
  visitor: string;
  btn: TBulmaButton;
begin
  { Example of how to use globals. }
  visitor:=GetGlobal('visitor_name');
  if visitor = 'null' then
  begin
    { This will create a basic form which will call another function in this
      application code, just above this one actually when clicked.  I hope to
      make the creation of forms easier than this soon with this framework, as
      the below might be a bit too difficult for someone. }
    if Assigned(inp) then
      inp.Free;
    inp:=TBulmaInput.Create(Self, 'What is your name?', 'visitor_name');
    TabBody.setContent(inp.renderHTML);
    btn:=TBulmaButton.Create(Self, 'Okay', 'okayBtn', @DoVisitorName);
    TabBody.Write(btn.renderHTML);
    btn.Bind;
  end
  else
  begin
    TabBody.setContent('It is best to start with setContent over write...<br/>');
    TabBody.Write('But, write() can be used after to add additional content.<br/>');
    TabBody.Write('Oh, hi there '+visitor+', and how are you today?');
  end;
end;

procedure TExample1.ShowVFS(data: string);
begin
  { This is the callback used with the GetVFSFile API call. }
  case GetVFSFileType of
    0: TabBody.setContent('<pre>'+data+'</pre>');
    2: TabBody.setContent(data);
  end;
end;

procedure TExample1.AboutTab;
begin
  TabBody.setContent('This an example for the Rabit Hole web Framework.');
  { This an example of using the contents of a file in the VFS. }
  GetVFSFile('Documents', 'FSRoot', @ShowVFS);
end;

procedure TExample1.DoRun;
begin
  { Create all the tabs before `inherited DoRun;` as it DoRun will render
    the tabs.  If course, you can always call FTabs.RenderHTML after you
    add tabs if they are added after DoRun. }
  FTabs.AddTab('Home', 'HomeTab', @HomeTab);
  FTabs.AddTab('Example', 'ExampleTab', @ExampleTab);
  FTabs.AddTab('About', 'AboutTab', @AboutTab);
  inherited DoRun;
  { Unless database has been embeeded into the application, no database or
    any tables, such as any of the globals will be available here. The main
    advantage of using the embedded mode is that you have instant access to
    those databases without any additional loads from the server, allowing
    some early logic to take place if needed.
  HomeTab;
  {{ Code to run in example after framework is done initialization, although
     is optional. }} }
end;

procedure TExample1.DoFailure(aMessage: string);
begin
  { This callback API will definately change in the future, as it's too generic,
    and it would be too difficult for someone working with the Framework to know
    what the error was to react to the error, instead of just being able to
    display it, which might be pointless to an end-user. }
  TabBody.setContent(aMessage);
end;

procedure TExample1.GlobalsLoaded;
begin
  { This is called once the globals have been loaded and are ready to be used,
    this means that it can run before the part after `inherited DoRun;` if the
    globals table is embedded into the application. }
  if not EnableRouter then { This may overwrite what the router may write. }
    TabBody.setContent(GetGlobal('test'));
end;

procedure TExample1.VFSLoaded;
begin
  { Close to be run after the VFS has been loaded and is ready for use. }
  SetInitVFSCallback(@ShowVFS); { Needs to be set to use router. }
  if EnableRouter and (Router.RouteFromURL = '') then
    HomeTab; { This is an example of enabling external links to the site. }
end;

begin
  { This initializes the application class, and sets up some settings. }
  Application:=TExample1.Create(nil);
  Application.Initialize;
  Application.VFSTable:='vfs'; { Enables VFS support. }
  Application.EnableRouter:=True; { Enables URL routing for use with with VFS directory links. }
  Application.DBFile:='website'; { Here we specify a database for the application to use. }
  Application.SaveFlags:=True; { This will allow the saving of the flags which get set. }
  Application.TabID:='RBTabs'; { Bulma specific, allows the app to manage the tabs, not required. }

  { Initialize rbpage before starting the application. }
  SetTabBody('TabBody'); { Can be any div id, does not need to be the body of a tab. }

  Application.Run;
end.
