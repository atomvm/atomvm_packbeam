# packbeam

An Erlang Escript and OTP library used to generate an <a href="http://github.com/bettio/AtomVM">AtomVM</a> AVM file from a set of files (beam files, previously built AVM files, or even arbitrary data files).

This tool roughly approximates the functionality of the AtomVM `PackBEAM` utility, except:

* Support for multiple data types, include beam files, text files, etc
* "Smart" extraction of beams from AVM files, so that only the beams that are needed are packed

The `packbeam` tool may be used on its own.  More typically, it is used internally as part of the <a href="https://github.com/fadushin/atomvm_rebar3_plugin">atomvm_rebar3_plugin</a> `rebar3` plugin.

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
        create [-prune] [-start <module>] <output-avm-file> [<input-file>]+
        list <input-avm-file-path>
        delete [-out <output-avm-file-path>] <input-avm-file-path> [<name>]+
        help  print this help

The `packbeam` command will return an exit status of 0 on successful completion of a command.  An unspecified non-zero value is returned in the event of an error.

The `packbeam` sub-commands are described in more detail below.

## `create` sub-command

To create an AVM file from a list of beam files, use the `create` sub-command to create an AVM file.  The first argument is take to be the output AVM file, following by the files you would like to add, e.g.,

    shell$ packbeam create mylib.avm mylib/ebin/mylib.beam mylib/ebin/foo.beam mylib/ebin/bar.beam

This command will create an AtomVM AVM file suitable for use with AtomVM.

> Note that any beam files specified are stripped of their path information, inside of the generated AVM file.  Any files that have the same name will be added in the order they are listed on the command line.  However, AtomVM will only resolve the first such file when loading modules at run-time.

If you are building an application (as opposed to a library, suitable for inclusion in another AVM file), then at least one beam module in an AVM file should contain a `start/0` entry-point, i.e., a function called `start` with arity 0.  AtomVM will use this entry-point as the first function to execute, when starting.

> Note.  It is conventional, but not required, that the first beam file in an AVM file contains the `start/0` entry-point.  AtomVM will use the first BEAM file that contains an exported `start/0` function as the entry-point for the application.

If your application has multiple modules with exported `start/0` functions, you may use the `-start <module>` option to specify the module you would like placed first in your AVM file.  The `<module>` parameter should be the module name (without the `.beam` suffix, e.g., `main`).

A previously created AVM file file may be supplied as input (including the same file specified as output, for example).  The contents of any input AVM files will be included in the output AVM file.  For example, if you are building a library of BEAM files (for example, none of which contain a `start/0` entry-point), you may want to archive these into an AVM file, which can be used for downstream applications.

In addition, you may specify a "normal" (i.e., non-beam or non-AVM) file.  Normal files are labeled with the path specified on the command line.

    shell$ packbeam create mylib.avm mylib.avm mylib/priv/sample.txt

> Note.  It is conventional in AtomVM for normal files to have the path `<module-name>/priv/<file-name>`.

If you specify the `-prune` flag, then `packbeam` will only include beam files that are transitively dependent on the entry-point beam.  Transitive dependencies are determined by imports, as well as use of an atom in a module (e.g, as the result of a dynamic function call, based on a module name).

If there is no beam file with a `start/0` entry-point defined in the list of input modules and the `-prune` flag is used, the command will fail.  You should _not_ use the `-prune` flag if you are trying to build libraries suitable for inclusion on other AtomVM applications.

## `list` sub-command

The `list` sub-command will print the contents of an AVM file to the standard output stream.  If a beam file contain an exported `start/0` function, it will be marked with an asterisk (`*`).  The size in bytes of each module is also printed in square brackets (`[]`).

To list the contents of an AVM file, specify the location of the AVM file to input as the first argument:

    shell$ packbeam list mylib.avm
    mylib.beam * [284]
    foo.beam [276]
    bar.beam [252]
    mylib/priv/sample.txt [29]

## `delete` sub-command

The `delete` sub-command can be used to remove elements from an AVM file.

To delete one or more elements from an AVM file, specify the location of the AVM file from which to remove elements, followed by the list of elements (as displayed via the `list` sub-command) to remove.  You may optionally specify an output AVM file using the `-out` option, which will contain the contents of the input AVM file, minus the specified elements.  If no output AVM is specified, the input AVM file will be overwritten.

For example:

    shell$ packbeam delete -out mylib2.avm mylib.avm foo.beam bar.beam
    shell$ packbeam list mylib2.avm
    mylib.beam * [284]
    mylib/priv/sample.txt [29]

# `packbeam` API

> TODO Used by rebar plugin
