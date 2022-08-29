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

# `packbeam` command

The `packbeam` command is used to create an AVM file from a list of beam and other file types, to list the contents of an AVM file, or to delete elements from an AVM file.

The general syntax of the `packbeam` command takes the form:

    packbeam <sub-command> <args>

On-line help is available via the `help` sub-command:

    shell$ packbeam help
    Syntax:
    packbeam <sub-command> <options> <args>

    The following sub-commands are supported:

        create <options> <output-avm-file> [<input-file>]+
            where:
            <output-avm-file> is the output AVM file,
            [<input-file>]+ is is a list of one or more input files,
            and <options> are among the following:
                [--prune|-p]           Prune dependencies
                [--start|-s <module>]  Start module

        list <options> <avm-file>
            where:
            <avm-file> is an AVM file,
            and <options> are among the following:
                [--format|-f csv|bare|default]  Format output

        delete <options> <avm-file> [<element>]+
            where:
            <avm-file> is an AVM file,
            [<element>]+ is is a list of one or more elements to delete,
            and <options> are among the following:
                [-out <output-avm-file>]    Output AVM file

        help  print this help

The `packbeam` command will return an exit status of 0 on successful completion of a command.  An unspecified non-zero value is returned in the event of an error.

The `packbeam` sub-commands are described in more detail below.

## `create` sub-command

To create an AVM file from a list of beam files, use the `create` sub-command to create an AVM file.  The first argument is take to be the output AVM file, following by the files you would like to add, e.g.,

    shell$ packbeam create mylib.avm mylib/ebin/mylib.beam mylib/ebin/foo.beam mylib/ebin/bar.beam

This command will create an AtomVM AVM file suitable for use with AtomVM.

The input files specified in the create subcommand may be among the following types:

* compiled BEAM files (typically ending in `.beam`)
* Previously created AVM files
* "Normal" files, e.g., text files, binary files, etc.

Note that beam files specified are stripped of their path information, inside of the generated AVM file.  Any files that have the same name will be added in the order they are listed on the command line.  However, AtomVM will only resolve the first such file when loading modules at run-time.

### Start Entrypoint

If you are building an application that provides a start entrypoint (as opposed to a library, suitable for inclusion in another AVM file), then at least one beam module in an AVM file must contain a `start/0` entry-point, i.e., a function called `start` with arity 0.  AtomVM will use this entry-point as the first function to execute, when starting.

> Note.  It is conventional, but not required, that the first beam file in an AVM file contains the `start/0` entry-point.  AtomVM will use the first BEAM file that contains an exported `start/0` function as the entry-point for the application.

If your application has multiple modules with exported `start/0` functions, you may use the `--start <module>`  (alternatively, `-s <module>`) option to specify the module you would like placed first in your AVM file.  The `<module>` parameter should be the module name (without the `.beam` suffix, e.g., `main`).

A previously created AVM file file may be supplied as input (including the same file specified as output, for example).  The contents of any input AVM files will be included in the output AVM file.  For example, if you are building a library of BEAM files (for example, none of which contain a `start/0` entry-point), you may want to archive these into an AVM file, which can be used for downstream applications.

In addition, you may specify a "normal" (i.e., non-beam or non-AVM) file.  Normal files are labeled with the path specified on the command line.

    shell$ packbeam create mylib.avm mylib.avm mylib/priv/sample.txt

> Note.  It is conventional in AtomVM for normal files to have the path `<module-name>/priv/<file-name>`.

### Pruning

If you specify the `--prune` (alternatively, `-p`) flag, then `packbeam` will only include beam files that are transitively dependent on the entry-point beam.  Transitive dependencies are determined by imports, as well as use of an atom in a module (e.g, as the result of a dynamic function call, based on a module name).

If there is no beam file with a `start/0` entry-point defined in the list of input modules and the `--prune` flag is used, the command will fail.  You should _not_ use the `--prune` flag if you are trying to build libraries suitable for inclusion on other AtomVM applications.

## `list` sub-command

The `list` sub-command will print the contents of an AVM file to the standard output stream.

To list the elements of an AVM file, specify the location of the AVM file to input as the first argument:

    shell$ packbeam list mylib.avm
    mylib.beam * [284]
    foo.beam [276]
    bar.beam [252]
    mylib/priv/sample.txt [29]

The elements in the AVM file are printed to the standard output stream and are listed on each line.  If a beam file contain an exported `start/0` function, it will be marked with an asterisk (`*`).  The size in bytes of each module is also printed in square brackets (`[]`).

You may use the `--format` (alternatively, `-f`) option to specify an output format.  The supported formats are:

* `csv`  Output elements in comma-separated value format.  Fields include the module name, whether the element is a BEAM file, whether the element provides a `start/0` entrypoint, and the size (in bytes) of the element.
* `bare`  Output just the module name, with no annotations.
* `default`  Output the module name, size (in brackets), and whether the file provides a `start/0` entrypoint, indicated by an asterisk (`*`).  The `default` output is used if the `--format` option is not specified.


## `delete` sub-command

The `delete` sub-command can be used to remove elements from an AVM file.

To delete one or more elements from an AVM file, specify the location of the AVM file from which to remove elements, followed by the list of elements (as displayed via the `list` sub-command) to remove.  You may optionally specify an output AVM file using the `-out` option, which will contain the contents of the input AVM file, minus the specified elements.  If no output AVM is specified, the input AVM file will be overwritten.

For example:

    shell$ packbeam delete -out mylib2.avm mylib.avm foo.beam bar.beam
    shell$ packbeam list mylib2.avm
    mylib.beam * [284]
    mylib/priv/sample.txt [29]

# `packbeam_api` API

> TODO Used by rebar plugin
