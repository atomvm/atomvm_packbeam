# packbeam

An escript and OTP library used to generate an <a href="http://github.com/bettio/AtomVM">AtomVM</a> AVM file from a set of files (beam files, previously built AVM files, or even arbitrary data files).

This tool roughly approximates the functionality of the AtomVM `PackBEAM` utility, except:

* Support for multiple data types, include beam files, text files, etc
* "Smart" extraction of beams from AVM files, so that only the beams that are needed are packed (TODO)
* Support for specifying directories of beam or AVM files (TODO)
* Rebar integration (TODO)

# Prerequisites

Building Packbeam requires Erlang/OTP 22 or later, for compatibility with AtomVM, as well as a local installation of `rebar3`.  Optionally, any recent version of `make` may be used to simplify builds.

# Build

To build, run

    shell$ rebar3 escriptize

or

    shell$ make escript

These commands will create an Erlang `escript` file called `packbeam` under

    _build/default/bin/packbeam

in your local working directory.

> TODO release

# `packbeam` command

The `packbeam` command is used to create an AVM file from a list of beam and other file types, or to list the contents of an AVM file.

The general syntax of the `packbeam` command takes the form

    shell$ packbeam <sub-command> <args>

On-line help is available via the `help` sub-command:

    shell$ packbeam help
    Syntax:
        packbeam <sub-command> <options>

    The following sub-commands are supported:
        * create -out <output-avm-file-path> [<input-file>]+
        * list -in <input-avm-file-path>
        * delete -in <input-avm-file-path> [-out <output-avm-file-path>] [<name>]+
        * help  print this help

The `packbeam` command will return an exit status of 0 on successful completion of a command.  An unspecified non-zero value is returned in the event of an error.

The `packbeam` sub-sommands are described in more detail below.

## `create` sub-command

To create an AVM file from a list of beam files, use the `create` subcommand to create an AVM file.  Use the `-out` flag to specify the name of the output AVM file, following by the files you would like to add, e.g.,

    shell$ packbeam create -out mylib.avm mylib/ebin/mylib.beam mylib/ebin/foo.beam mylib/ebin/bar.beam

This command will create an AtomVM AVM file suitable for use with AtomVM.  Note that any beam files specified are stripped of their path information, inside of the generated AVM file.

At least one beam module in an AVM file should contain a `start/0` entrypoint, i.e., a function called `start` with arity 0.  AtomVM will use this entrypoint as the first function to execute, when starting.

> Note.  It is conventional, but not required, that the first beam file in an AVM file contains the `start/0` entrypoint.

A previously created AVM file file may be supplied as input (including the same file specified as output, for example).  The contents of any input AVM files will be included in the output AVM file.

In addition, you may specify a "normal" (i.e., non-beam or non-AVM) file.  Normal files are labeled with the path specified on the command line.

    shell$ packbeam create -out mylib.avm mylib.avm mylib/priv/sample.txt

> Note.  It is conventional in AtomVM for normal files to have the path `<module-name>/priv/<file-name>`.

## `list` sub-command

The `list` subcommand will print the contents of an AVM file to the standard output stream.  If a beam file contain an exported `start/0` function, it will be marked with an asterisk (`*`).  The size in bytes of each module is also printed in square brackets (`[]`).

To list the contents of an AVM file, specify the location of the AVM file to input using the `-in` option:

    shell$ packbeam list -in mylib.avm
    mylib.beam * [284]
    foo.beam [276]
    bar.beam [252]
    mylib/priv/sample.txt [29]

## `delete` sub-command

The `delete` sub-command can be used to remove elements from an AVM file.

To delete one or more elements from an AVM file, specify the location of the AVM file from which to remove elements using the `-in` option, followed by the list of elements (as displayed via the `list` sub-command) to remove.  You may optionally specify an output AVM file using the `-out` option, which will contain the contents of the input AVM file, minus the specified elements.  If no output AVM is specified, the input AVM file will be overwritten.

For example:

    shell$ packbeam delete -in mylib.avm -out mylib2.avm foo.beam bar.beam
    shell$ packbeam list -in mylib2.avm
    mylib.beam * [284]
    mylib/priv/sample.txt [29]

# `packbeam` API

> TODO to be used by rebar plugin
