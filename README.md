# pascal-mini-greps
Pascal code for minimal grep-like programs

---

This repo contains two Free Pascal programs to perform some basic file searching and matching operations based on these grep options:

- `-l    return list of files matching a keyword`
- `-L    return list of files not matching a keyword`
- `-w    keyword must be matched as a whole word`

`RTLFindFiles.pas` uses all [Free Pascal](https://www.freepascal.org) Run Time Library routines to perform the search.

A standard install of Free Pascal or Lazarus should be all that's needed to compile this. Copy to a folder, and compile with:

    fpc RTLFindFiles.pas


`CGEFindFiles.pas` uses as many [Castle Game Engine](https://castle-engine.io) routines as I could find to perform the search.

This program requires Castle Game Engine to be [downloaded](https://castle-engine.io/download) and the packages [installed[(https://castle-engine.io/fpmake) to compile from the commandline:

    fpc CGEFindFiles.pas


Both programs use a similar command line format:

![image](https://github.com/user-attachments/assets/21ba75f1-bc84-4757-9247-b8257244d7b6)
> RTLFindFiles command line help


![image](https://github.com/user-attachments/assets/ff87e5d9-2be4-4de9-a4bd-7c1f4927a96a)
> CGEFindFiles uses the CastleParameters unit and therefore supports long option names as well.


