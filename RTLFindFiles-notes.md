### overview

RTLFindFiles is a [FreePascal](https://www.freepascal.org) run-time-library-only version of a very basic "grep" utility. This project came about as a way to do some basic "grep" operations with a Pascal program. The only grep features implemented here so far are:

    -l      return filenames that contain a text pattern (default)
    -L      return filenames that don't contain a text pattern
    -w      match on whole words only

By default it will produce a list of filenames on standard output that match a text pattern.

With the `-L` switch, it will produce a list of filenames that do not match the text pattern.

With the `-w` switch, it will match the text pattern only as a whole word.

`-nr` prevents the search from recursing into subfolders.

The "RTL" name is to distinguish this program from CGEFindFiles.pas which focuses on using [Castle Game Engine](https://castle-engine.io) routines and libraries to do this, such as CastleFindFiles, CastleParameters and CastleStringUtils.

### basic usage

`RTLFindFiles [filePath] [fileMask] [textPattern] [-l] [-L] [-w] [-nr] `
	`[-h] [-p]`

    filePath       starting folder

    fileMask       mask to filter files

    textPattern    text to search for in files

    -l             return matching filenames instead of file lines (default)

    -L             return non-matching filenames

    -w             match textPattern as whole word

    -nr            do not recurse into subfolders

    -h             show this help

    -p             show command line params


`-p` shows the contents of the global Params record that is used to map command line parameters to the values and flags used by the find process. Specify all of the other parameters, then end with `-p` and the program will show these values and stop instead of running the search operation.

---
### matching on whole words

To accomplish matching on whole words, I use the regular expression `\Wtext\W` to match text that is surrounded by non word characters. This works, but I don't think it's the correct way to do it. This actually includes the preceding and following non-word characters in the found string. This doesn't matter so much right now where I'm only trying to see if there are any matches in the file.

Please let me know if there is a better way to do this.

---
### other notes

- I use 4-space indents for my own readability, sorry but as an elderly gentleman with challenged eye sight, 2 spaces are just not enough
	- what are commonly used Pascal formatters?

- units used in RTL version: Classes, Sysutils, 
	- StrUtils for IsWild
	- RegExpr for TRegExpr

    - are there better units or routines to use for these?    

- developed on Windows, I don't have a way to compile for Linux or Mac at the moment
	- I will get a Raspberry Pi or Puppy Linux PC going again at some point, or see if Free Pascal can be installed on my web hosting shell account

---
### queue based processing of subfolders

I use a queue to track paths instead of recursion, just because it's a trick I developed in Python years ago and like it more because it seems simpler and cleverer all at the same time!

As each subfolder is encountered, it is added to the end of a stringlist, and then each path is "popped" from the front of the list and its files are iterated over with FindFirst and FindNext. This continues until the subfolder queue is empty.

---
### questionable command line parameter handling

In the source code, filePath defaults to "`.`" and fileMask defaults to "`*.*`" and textPattern requires a value, but textPattern is the third parameter to be matched so it is required to specify filePath and fileMask anyway.

NCVWTO! (not currently very well thought out (but good enough for a first draft!))

===
