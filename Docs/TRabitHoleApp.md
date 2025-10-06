# rbapp.TRabitHoleApp

This class is meant to be sub-classed for proper use:

```pascal
uses rbapp;

type
  TMyRadApp = class(TRabitHoleApp)
  protected
    procedure DoFailure(aMessage: string); override;
    procedure GlobalsLoaded; override;
  end;
```

It's current most minimal form is demonstrated above.  Those should always be overridden.  Below is the public API available to your subclass:

```pascal
  TRabitHoleApp = class(TBrowserApplication)
  protected
    FTabs: TBulmaTabs;
    procedure DoRun; override;
    procedure DoFailure(aMessage: string); virtual; abstract;
    procedure GlobalsLoaded; virtual; abstract;
  public
    property TabID: string read FTabID write SetTabID;
    property DatabaseType: TDatabaseType read FDatabaseType write FDatabaseType;
    property DBFile: string read FDBFile write SetDBFile;
    property SaveFlags: Boolean read FSaveFlags write FSaveFlags;
    constructor Create(aOwner: TComponent); override;
    procedure SaveGlobals;
    procedure AddFlag(AFlag: string);
    procedure DelFlag(AFlag: string);
    function HasFlag(AFlag: string): Boolean;
  end;
```

In order of appearance in class above:

`FTabs` access to the Bulma tabs if you use the Bulma CSS with this framework.  Might be removed in the future, be aware.

`DoRun` can be overridden if you need extra functionality, but is optional.

`DoFailure` API is subject to change, currently provides a textual error message.

`GlobalsLoaded` is called once the Globals are available and ready to use in the code.

`TabID` is a string to set the div id of the tabs which FTabs will be associated with, might be removed in the future.

`DatabaseType` is a specific enum type and can be set to either `dtTable`, or `dtDatabase`.

`DBFile` is the unified database filename, if this is set, then `DatabaseType` will change to be `dtDatabase`.

`SaveFlags` if you plan on saving persistent session data between visits, then this will also save any flags.

`Create` called from the main program to initialize the class.

`SaveGlobals` can be called by your code to save the current globals state to the browser for persistence.

`AddFlag` is used to add a new flag to the current session.

`DelFlag` is used to remove a flag from the current session.

`HasFlag` is used to check if a flag has been set in the current session.

## Database or Tables?

If you are embedding, choose tables, and add them as resources to the application.

If you are not embedding, then choose database if you only use the built-in tables already provided.

Choose tables without embedding only if you have a lot of extra data that doesn't need to be loaded all at once, this can make it much easier on the user's bandwidth, as only tables for data of which the visitor will be consuming will be downloaded.  However, you can choose either one.

## Flags?

These are 100% user created, and the framework doesn't add or remove flags, these flags are entirely based on the content in the website.  For example, it could be used to say signify that the user has completed a lession, unlocking further lessions by checking which flags are currently set.  These flags can be optionally persisted in the browser, if `SaveGlobals` is called.

## Globals?

These are global variables which can also be easily saved into the browser's local storage.  For example, if a visitor would like to have their name persist between visits, a `SetGlobal('visitor_name', inp.Value);` would set it, following a `SaveGlobals`, then when the page reloads in the future, this global will already be set, so you can check for a specific visitor with ease, `visitor:=GetGlobal('visitor_name')`, and then `if visitor = 'null' then`.  I'll have an example of this working shortly.
