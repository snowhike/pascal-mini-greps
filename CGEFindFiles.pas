// CGEFindFiles.pas - Castle Game Engine version of FindFiles, by yardshop
// 2025-02-01 09:35:40

program CGEFindFiles;

{$mode objfpc}{$H+}

uses SysUtils,
    CastleUtils,        // nl
    CastleParameters,   // Parameters
    CastleFindFiles,    // FindFiles
    CastleStringUtils,  // TCastleStringList
    CastleFilesUtils,   // FileToString
    RegExpr;            // TRegExpr

const Version = '0.1';

//----------

procedure WriteFm(const fmt: string; const args: array of const);
begin
    WriteLn(Format(fmt, args));
end;

//----------

type
    TApp = class(TObject)
    private
        FResults : TCastleStringList;

        FFilePath    : string;
        FFileMask    : string;
        FTextPattern : string;

        FReturnMatchingFiles : Boolean;
        FReturnNonMatchingFiles : Boolean;

        FMatchWholeWords : Boolean;
        FRecurseSubfolders : Boolean;

        FShowHelp : Boolean;
        FShowParams : Boolean;

    public
        constructor Create;
        destructor  Destroy; override;

        procedure GetParameters;
        procedure ShowCommandLineParams;

        procedure FileCallback(const FileInfo: TFileInfo; var StopSearch: boolean);

        procedure FindFiles;
        procedure ShowResults;

        property FilePath               : string  read FFilePath                write FFilePath;
        property FileMask               : string  read FFileMask                write FFileMask;
        property TextPattern            : string  read FTextPattern             write FTextPattern;

        property ReturnMatchingFiles    : boolean read FReturnMatchingFiles     write FReturnMatchingFiles;
        property ReturnNonMatchingFiles : boolean read FReturnNonMatchingFiles  write FReturnNonMatchingFiles;

        property MatchWholeWords        : boolean read FMatchWholeWords         write FMatchWholeWords;
        property RecurseSubfolders      : boolean read FRecurseSubfolders       write FRecurseSubfolders;

        property ShowHelp               : boolean read FshowHelp                write FshowHelp;
        property ShowParams             : boolean read FshowParams              write FshowParams;
    end;

constructor TApp.Create;
begin
    inherited Create;
    FResults := TCastleStringList.Create;

    FReturnMatchingFiles := True;
    FReturnNonMatchingFiles := False;

    FMatchWholeWords := False;
    FRecurseSubfolders := True;

    FShowHelp := False;
    FShowParams := False;
end;

destructor TApp.Destroy;
begin
    FResults.Free;
end;

// stand-alone record for command line parameters
type
    TParams = record
        filePath : string;
        fileMask : string;
        textPattern : string;

        returnMatchingFiles    : boolean;
        returnNonMatchingFiles : boolean;

        matchWholeWords        : boolean;
        recurseSubfolders      : boolean;

        showHelp               : boolean;
        showParams             : boolean;
    end;

var
    Params : TParams;

const
    Options: array [0..5] of TOption = (
        (Short:'h'; Long:'help';              Argument: oaNone),
        (Short:'l'; Long:'matchingFiles';     Argument: oaNone),
        (Short:'L'; Long:'nonMatchingFiles';  Argument: oaNone),
        (Short:'w'; Long:'wholeWord';         Argument: oaNone),
        (Short:'n'; Long:'noRecurse';         Argument: oaNone),
        (Short:'p'; Long:'showParams';        Argument: oaNone)
    );

const HelpText =
    'CGEFindFiles v' + Version +nl+
    '' +nl+
    'CGEFindFiles <filePath> <fileMask> <textPattern> [-l] [-L] [-w] [-n] [-h] [-p]' +nl+
    '' +nl+
    '<filePath> the starting path to search from, eg: ..\src' +nl+
    '' +nl+
    '<fileMask> the filename filter to apply, eg: *.pas' +nl+
    '' +nl+
    '<textPattern> the text to match within each file, eg: regex' +nl+
    '' +nl+
    '-l' +nl+
    '--matchingFiles' +nl+
    '' +nl+
    '    output a list of files that contain the text pattern' +nl+
    '' +nl+
    '-L' +nl+
    '--nonMatchingFiles' +nl+
    '' +nl+
    '    output a list of files that do not contain the text pattern' +nl+
    '' +nl+
    '-w' +nl+
    '--wholeWord' +nl+
    '' +nl+
    '    force the text pattern to be matched only as a whole word' +nl+
    '' +nl+
    '-n' +nl+
    '--noRecurse' +nl+
    '' +nl+
    '    do not recurse into subfolders' +nl+
    '' +nl+
    '-h' +nl+
    '--help' +nl+
    '' +nl+
    '    show this help and exit' +nl+
    '' +nl+
    '-p' +nl+
    '--params' +nl+
    '' +nl+
    '    show the value of the given parameters and exit';

procedure TApp.ShowCommandLineParams;
const
    NoYes : array[boolean] of string = ('no', 'yes');
begin
    WriteLn;
    WriteFm('CGEFindFiles v%s', [Version]);
    WriteLn;
    WriteLn('command line parameters:');
    WriteLn('------------------------------------------------------------------------------');
    WriteFm('filePath               = %s', [FFilePath]);
    WriteFm('fileMask               = %s', [FFileMask]);
    WriteFm('textPattern            = %s', [FTextPattern]);
    WriteFm('returnMatchingFiles    = %s', [NoYes[FReturnMatchingFiles]]);
    WriteFm('returnNonMatchingFiles = %s', [NoYes[FReturnNonMatchingFiles]]);
    WriteFm('matchWholeWords        = %s', [NoYes[FMatchWholeWords]]);
    WriteFm('recurseSubfolders      = %s', [NoYes[FRecurseSubfolders]]);
end;

// OptionProc sets command line choices in stand-alone Params record
procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
    const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
begin
    case OptionNum of
        0: Params.showHelp := True;

        1: begin
            Params.returnMatchingFiles := True;
            Params.returnNonMatchingFiles := False;
        end;

        2: begin
            Params.returnNonMatchingFiles := True;
            Params.returnMatchingFiles := False;
        end;

        3: Params.matchWholeWords := True;

        4: Params.recurseSubfolders := False;

        5: Params.showParams := True;

        else begin
            WriteFm('option %d not implemented', [OptionNum])
        end;
    end;
end;

procedure TApp.GetParameters;
begin
    Params.returnMatchingFiles := True; // default operation

    Parameters.Parse(Options, @OptionProc, nil);

    // copy stand-alone Params values into app object
    FReturnMatchingFiles := Params.returnMatchingFiles;
    FReturnNonMatchingFiles := Params.returnNonMatchingFiles;

    FMatchWholeWords := Params.matchWholeWords;
    FRecurseSubfolders := Params.recurseSubfolders;

    FShowHelp := Params.showHelp;
    FShowParams := Params.showParams;

    if Parameters.High < 3 then
        FShowHelp := True
    else begin
        Parameters.CheckHigh(3);
        FFilePath    := Parameters[1];
        FFileMask    := Parameters[2];
        FTextPattern := Parameters[3];
    end;

    if FMatchWholeWords then begin
        FTextPattern := Format('\W(%s)\W', [FTextPattern])
    end;
end;

procedure TApp.FileCallback(const FileInfo: TFileInfo; var StopSearch: boolean);
var
    fulltext : string;
    re : TRegExpr;
begin
    fulltext := FileToString(FileInfo.AbsoluteName);

    re := TRegExpr.Create(TextPattern);
    if re.Exec(fulltext) then begin
        if returnMatchingFiles then begin
            FResults.Add(FileInfo.AbsoluteName); // or URL
        end;
    end
    else begin
        if returnNonMatchingFiles then begin
            FResults.Add(FileInfo.AbsoluteName); // or URL
        end;
    end;
    re.Free;
end;

procedure TApp.FindFiles;
begin
    CastleFindFiles.FindFiles(FilePath, FileMask, false, @FileCallback, [ffRecursive]);
end;

procedure TApp.ShowResults;
begin
    Write(FResults.Text);
end;

//---------- Main

var
    app : TApp;

begin
    app := TApp.Create;
    app.GetParameters;

    if app.ShowHelp then
        WriteLn(HelpText)

    else if app.ShowParams then
        app.ShowCommandLineParams

    else begin
        app.FindFiles;
        app.ShowResults;
    end;

    app.Free;
end.

//===
