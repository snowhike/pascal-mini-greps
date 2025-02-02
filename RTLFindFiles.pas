{
RTLFindFiles.pas - Run-Time-Library-only version of FindFiles, by yardshop
2025-01-28 20:08:50 - v0.1, first Gist release
}

program RTLFindFiles;

{$mode objfpc}{$H+}

uses
    Classes,
    SysUtils,
    StrUtils, // IsWild
    RegExpr;  // TRegExpr

const Version = '0.1';

//----------

procedure WriteFm(const s : string; const args : array of const);
begin
    WriteLn(Format(s, args));
end;

function ReadStringFromFile(const FileName: string): RawByteString;
var
    M: TMemoryStream;
    Count: SizeInt;
begin
    M := TMemoryStream.Create;
    M.LoadFromFile(FileName);
    try
        Count := M.Size;
        SetLength(Result, Count);
        M.Read(Result[1], Count);
    finally
        M.Free;
    end;
end;

//----------

type
    TParams = record
        filePath : string;
        fileMask : string;
        textPattern : string;
        // returnMatchingLines : Boolean; - not implemented yet
        returnMatchingFiles : Boolean;
        returnNonMatchingFiles : Boolean;
        matchWholeWords : Boolean;
        recurseSubfolders : Boolean;
        showHelp : Boolean;
        showParams : Boolean;
        message : string;
    end;

var
    Params : TParams;

procedure GetParams;
var
    pIndex : integer;
    pValue : string;
    unnamedCount : integer;
begin
    Params.filePath := '';
    Params.fileMask := '*.*';
    Params.textPattern := '';
    // Params.returnMatchingLines := False;
    Params.returnMatchingFiles := True;
    Params.returnNonMatchingFiles := False;
    Params.matchWholeWords := False;
    Params.recurseSubfolders := True;
    Params.showHelp := False;
    Params.showParams := False;
    Params.message := '';

    pIndex := 1;
    unnamedCount := 0;

    while pIndex <= ParamCount do begin

        pValue := ParamStr(pIndex);

        if pValue = '-l' then begin
            // Params.returnMatchingLines := False;
            Params.returnMatchingFiles := True;
            Params.returnNonMatchingFiles := False;
        end

        else if pValue = '-L' then begin
            // Params.returnMatchingLines := False;
            Params.returnMatchingFiles := False;
            Params.returnNonMatchingFiles := True;
        end

        else if pValue = '-w' then begin
            Params.matchWholeWords := True;
        end

        else if pValue = '-nr' then begin
            Params.recurseSubfolders := False;
        end

        else if pValue = '-h' then begin
            Params.showHelp := True;
        end

        else if pValue = '-p' then begin
            Params.showParams := True;
        end

        else if unnamedCount = 0 then begin
            Params.filePath := pValue;
            unnamedCount += 1;
        end
        else if unnamedCount = 1 then begin
            Params.fileMask := pValue;
            unnamedCount += 1;
        end
        else if unnamedCount = 2 then begin
            Params.textPattern := pValue;
            unnamedCount += 1;
        end;

        pIndex += 1;
    end;

    if Params.matchWholeWords then begin
        Params.textPattern := Format('\W(%s)\W', [Params.textPattern])
    end;

    if Params.textPattern = '' then begin
        Params.message := 'no text pattern given';
        Params.showHelp := True;
    end;
end;

procedure ShowParams;
const
    NoYes : array[boolean] of string = ('no', 'yes');
begin
    WriteLn;
    WriteFm('Findfiles v%s', [Version]);
    WriteLn;
    WriteLn('parameters');
    WriteLn('----------------------------------------------------------------------');
    WriteFm('filePath               = %s', [Params.filePath]);
    WriteFm('fileMask               = %s', [Params.fileMask]);
    WriteFm('textPattern            = %s', [Params.textPattern]);
    // WriteFm('returnMatchingLines    = %s', [NoYes[Params.returnMatchingLines]]);
    WriteFm('returnMatchingFiles    = %s', [NoYes[Params.returnMatchingFiles]]);
    WriteFm('returnNonMatchingFiles = %s', [NoYes[Params.returnNonMatchingFiles]]);
    WriteFm('matchWholeWords        = %s', [NoYes[Params.matchWholeWords]]);
    WriteFm('recurseSubfolders      = %s', [NoYes[Params.recurseSubfolders]]);
    WriteFm('showHelp               = %s', [NoYes[Params.showHelp]]);
    WriteFm('showParams             = %s', [NoYes[Params.showParams]]);
    WriteFm('message                = %s', [Params.message]);
    WriteLn;
end;

procedure ShowHelp;
begin
    WriteLn;
    WriteFm('RTLFindfiles v%s', [Version]);
    WriteLn;
    WriteLn('usage:');
    WriteLn;
    WriteLn('RTLFindFiles [filePath] [fileMask] [textPattern] [-l] [-L] [-w] [-nr] [-f] [-h] [-p]' );
    WriteLn;
    WriteLn('    filePath       starting folder, if blank then use current folder');
    WriteLn;
    WriteLn('    fileMask       mask to filter files, if blank then use "*.*"');
    WriteLn;
    WriteLn('    textPattern    text to search for in files');
    WriteLn;
    WriteLn('    -l             return matching filenames instead of file lines (default)');
    WriteLn;
    WriteLn('    -L             return non-matching filenames');
    WriteLn;
    WriteLn('    -w             match textPattern as whole word');
    WriteLn;
    WriteLn('    -nr            do not recurse into subfolders');
    WriteLn;
    WriteLn('    -h             show this help');
    WriteLn;
    WriteLn('    -p             show command line params');

    if (Params.message > '') then begin
        WriteLn;
        WriteFm('parameter error: %s', [Params.message])
    end;
end;

//----------

procedure FindMatchingFiles; // use global Params
var
    sr : TSearchRec;
    basename : string;
    fullname : string;
    path : string;

    rc : integer;
    re : TRegExpr;

    paths : TStringList; // "queue" to track subfolders to go into instead of recursion
    lines : TStringList;
    line  : string;

    fulltext : string;

begin
    paths := TStringList.Create;
    lines := TStringList.Create;

    paths.Add(Params.filePath);
    while paths.Count > 0 do begin

        path := paths[0]; // "pop" first path
        paths.Delete(0);

        rc := SysUtils.FindFirst(path + DirectorySeparator + '*.*', faAnyFile, sr);
        if rc = 0 then begin
            repeat

                // check if special files to skip
                if (sr.Name = '.') or (sr.Name = '..') or (sr.Name = '') then
                    continue;

                // else file or directory name
                basename := sr.Name;
                fullname := path + DirectorySeparator + basename;

                if (sr.Attr and faDirectory) <> 0 then begin
                    if Params.recurseSubfolders then
                        paths.Add(fullname); // add path to list
                end

                else begin // filename
                    if IsWild(basename, Params.fileMask, True { ignore case }) then begin

                        // load file contents
                        fulltext := ReadStringFromFile(fullname);

                        // match input
                        re := TRegExpr.Create(Params.textPattern);
                        if re.Exec(fulltext) then begin

                            if Params.returnMatchingFiles then
                                lines.Add(fullname);

                            // if Params.returnMatchingLines then
                            //     lines.Add( GetWholeLine(fulltext, re.MatchPos[0]) );
                            //     while re.ExecNext do begin
                            //         lines.Add( GetWholeLine(fulltext, re.MatchPos[0]));
                            //     end;

                        end
                        else begin
                            if Params.returnNonMatchingFiles then
                                lines.Add(fullname);
                        end;
                        re.Free;

                    end; // filename matches mask
                end; // filename

            until SysUtils.FindNext(sr) <> 0;
        end;

        SysUtils.FindClose(sr);
    end;

    // send results to output
    for line in lines do begin
        WriteLn(line);
    end;

    paths.Free;
    lines.Free;
end;

begin
    GetParams;

    if Params.showHelp then
        ShowHelp

    else if Params.showParams then
        ShowParams

    else
        FindMatchingFiles;
end.

//===
