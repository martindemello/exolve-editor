# Exolve Editor

A simple text editor to import [qxw](https://www.quinapalus.com/qxw.html) files into [exolve](https://github.com/viresh-ratnakar/exolve).

Loaded qxw files are converted to exolve format and merged into `exolve.html`. (`exolve.html` needs to be downloaded separately.)

## Download

### All platforms

* Install [racket](https://download.racket-lang.org/)
* Make sure racket is in your path (you will need to run the `gracket` or `Gracket.exe` executable)
* Install the `functional-lib` package (this should be the only dependency, but you will get prompted for recursive dependencies).
* Clone this repository and run the editor:

```
    $ git clone https://github.com/martindemello/exolve-editor.git
    $ cd exolve-editor
    $ raco pkg install functional-lib
    $ gracket exolve-editor.rkt
```

### Windows

There is a windows binary under the
[releases](https://github.com/martindemello/exolve-editor/releases) page.
Download the latest `exolve-editor.zip` and you should get a folder with an
executable.

### Other platforms

If anyone would like a linux binary, open an issue for it and I'll add one. I
would also welcome volunteers to package and upload mac binaries.

## Usage

1. Click `Load QXW` to load a qxw file. The grid will be converted to exolve,
   and filled into the left pane.
1. Click `Copy to clipboard` if you want to paste the converted grid into an
   html file in a separate editor.
1. Click `Load exolve template` and then `Save Exolve` if you want the editor
   to merge the converted grid into an exolve html file for you.

Both the crossword and the html template panes are editable, but it's probably
easier to just save the exolve file and then edit it in your favourite text
editor.

## License

MIT License (see included file).
