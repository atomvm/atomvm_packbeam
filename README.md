<!--
 Copyright 2020 Fred Dushin <fred@dushin.net>

 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->
# `atomvm_packbeam`

An Erlang Escript and library used to generate an [AtomVM](http://github.com/atomvm/AtomVM) AVM file from a set of files (beam files, previously built AVM files, or even arbitrary data files).

This tool roughly approximates the functionality of the AtomVM `PackBEAM` utility, except:

* Support for multiple data types, include beam files, text files, etc
* "Pruned" extraction of beams from AVM files, so that only the beams that are needed are packed
* Support for embedded OTP applications in your PackBEAM files.

The `packbeam` tool may be used on its own as a stand-alone command-line utility.  More typically, it is used internally as part of the [`atomvm_rebar3_plugin`](https://github.com/atomvm/atomvm_rebar3_plugin) [`rebar3`](https://rebar3.org) plugin.

## Prerequisites

Building `packbeam` requires a version of Erlang/OTP compatible with [AtomVM](https://github.com/atomvm/AtomVM), as well as a local installation of [`rebar3`](https://rebar3.org).  Optionally, any recent version of `make` may be used to simplify builds.  Consult the [AtomVM Documentation](https://www.atomvm.net/doc/master/) for information about supported OTP versions.

## Build

To build a release, run the following commands:

    shell$ rebar3 release
    shell$ rebar3 as prod tar

These commands will create an Erlang tar archive containing a versioned release of the `atomvm_packbeam` tool, e.g.,

    ...
    ===> Tarball successfully created: _build/prod/rel/atomvm_packbeam/atomvm_packbeam-0.6.2.tar.gz

in your local working directory.

> IMPORTANT!  The files in this tar archive do not contain the `atomvm_packbeam` prefix, so extracting these files without care will create a `bin` and `lib` directory in the location into which files from the archive is extracted.  See the example below before proceeding!

You can use the `install.sh` script to install the `atomvm_packbeam` utility into a location on your local machine.  You will need to specify the prefix location into which you want to install the utility, together with it's current version.

    shell$ ./install.sh /opt/atomvm_packbeam 0.6.2
    atomvm_packbeam version 0.6.2 installed in /opt/atomvm_packbeam.

> Note.  Some prefix locations may require `root` permissions to write files to.

Set your `PATH` environment variable to include the `bin` directory of the installation prefix (if not already set), and you should then be able to run the `packbeam` command included therein.

For example:

    shell$ export PATH=/opt/atomvm_packbeam/bin:$PATH
    shell$ packbeam help
    Syntax:
        packbeam <sub-command> <options> <args>
        ...

## `packbeam` command

The `packbeam` command is used to create an AVM file from a list of beam and other file types, to list the contents of an AVM file, or to delete elements from an AVM file.

The general syntax of the `packbeam` command takes the form:

    packbeam <sub-command> <args>

On-line help is available via the `help` sub-command:

    shell$ packbeam help

    packbeam version 0.7.2

    Syntax:
        packbeam <sub-command> <options> <args>

    The following sub-commands are supported:

        create <options> <output-avm-file> [<input-file>]+
            where:
            <output-avm-file> is the output AVM file,
            [<input-file>]+ is a list of one or more input files,
            and <options> are among the following:
                [--prune|-p]           Prune dependencies
                [--start|-s <module>]  Start module
                [--remove_lines|-r]    Remove line number information from AVM files

        list <options> <avm-file>
            where:
            <avm-file> is an AVM file,
            and <options> are among the following:
                [--format|-f csv|bare|default]  Format output

        extract <options> <avm-file> [<element>]*
            where:
            <avm-file> is an AVM file,
            [<element>]+ is a list of one or more elements to extract
                (if empty, then extract all elements)
            and <options> are among the following:
                [--out|-o <output-directory>]   Output directory into which to write elements
                (if unspecified, use the current working directory)

        delete <options> <avm-file> [<element>]+
            where:
            <avm-file> is an AVM file,
            [<element>]+ is a list of one or more elements to delete,
            and <options> are among the following:
                [--out|-o <output-avm-file>]    Output AVM file

        version
            Print version and exit

        help
            Print this help

The `packbeam` command will return an exit status of 0 on successful completion of a command.  An unspecified non-zero value is returned in the event of an error.

The `packbeam` sub-commands are described in more detail below.

### `create` sub-command

To create an AVM file from a list of beam files, use the `create` sub-command to create an AVM file.  The first argument is take to be the output AVM file, following by the files you would like to add, e.g.,

    shell$ packbeam create mylib.avm mylib/ebin/mylib.beam mylib/ebin/foo.beam mylib/ebin/bar.beam

This command will create an AtomVM AVM file suitable for use with AtomVM.

The input files specified in the create subcommand may be among the following types:

* compiled BEAM files (typically ending in `.beam`)
* Previously created AVM files
* "Normal" files, e.g., text files, binary files, etc.

Note that beam files specified are stripped of their path information, inside of the generated AVM file.  Any files that have the same name will be added in the order they are listed on the command line.  However, AtomVM will only resolve the first such file when loading modules at run-time.

#### Start Entrypoint

If you are building an application that provides a start entrypoint (as opposed to a library, suitable for inclusion in another AVM file), then at least one beam module in an AVM file must contain a `start/0` entry-point, i.e., a function called `start` with arity 0.  AtomVM will use this entry-point as the first function to execute, when starting.

> Note.  It is conventional, but not required, that the first beam file in an AVM file contains the `start/0` entry-point.  AtomVM will use the first BEAM file that contains an exported `start/0` function as the entry-point for the application.

If your application has multiple modules with exported `start/0` functions, you may use the `--start <module>`  (alternatively, `-s <module>`) option to specify the module you would like placed first in your AVM file.  The `<module>` parameter should be the module name (without the `.beam` suffix, e.g., `main`).

A previously created AVM file file may be supplied as input (including the same file specified as output, for example).  The contents of any input AVM files will be included in the output AVM file.  For example, if you are building a library of BEAM files (for example, none of which contain a `start/0` entry-point), you may want to archive these into an AVM file, which can be used for downstream applications.

In addition, you may specify a "normal" (i.e., non-beam or non-AVM) file.  Normal files are labeled with the path specified on the command line.

    shell$ packbeam create mylib.avm mylib.avm mylib/priv/sample.txt

> Note.  It is conventional in AtomVM for normal files to have the path `<module-name>/priv/<file-name>`.

#### Pruning

If you specify the `--prune` (alternatively, `-p`) flag, then `packbeam` will only include beam files that are transitively dependent on the entry-point beam.  Transitive dependencies are determined by imports, as well as use of an atom in a module (e.g, as the result of a dynamic function call, based on a module name).

If there is no beam file with a `start/0` entry-point defined in the list of input modules and the `--prune` flag is used, the command will fail.  You should _not_ use the `--prune` flag if you are trying to build libraries suitable for inclusion on other AtomVM applications.

#### Line number information

By default, the `packbeam` tool will generate line number information for embedded BEAM files.  Line number information is included in Erlang stacktraces, giving developers more clues into bugs in their programs.  However, line number information does increase the size of AVM files, and in some cases can have an impact on memory in running applications.

For production applications that have no need for line number information, we recommend using the `-r` (or `--remove_lines`) flags, which will strip line number information from embedded BEAM files.

### `list` sub-command

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

### `extract` sub-command

The `extract` sub-command can be used to extract elements from an AVM file.

To extract one or more elements from an AVM file, specify the location of the AVM file from which to extract elements, followed by the list of elements (as displayed via the `list` sub-command) to extract.  If no elements are listed, then all elements from the AVM file will be extracted.

Non-BEAM ("normal") files that contain paths in their names will be extracted into a directory tree that reflects the path used in the element name.  For example, if the element name is `mylib/priv/sample.txt`, then the `sample.txt` file will be extracted into the `mylib/priv` directory (relative to the output directory, detailed below).

You may optionally specify an output directory using the `--out` option, which will contain the extracted contents of the input AVM file.  This directory must exist beforehand, or a runtime error will occur.  If no output directory is specified, elements will be extracted into the current working directory.


For example:

    shell$ mkdir mydir
    shell$ packbeam extract -out mydir mylib.avm foo.beam mylib/priv/sample.txt
    Writing to mydir ...
    x foo.beam
    x mylib/priv/sample.txt


### `delete` sub-command

The `delete` sub-command can be used to remove elements from an AVM file.

To delete one or more elements from an AVM file, specify the location of the AVM file from which to remove elements, followed by the list of elements (as displayed via the `list` sub-command) to remove.  You may optionally specify an output AVM file using the `--out` option, which will contain the contents of the input AVM file, minus the specified elements.  If no output AVM is specified, the input AVM file will be overwritten.

For example:

    shell$ packbeam delete -out mylib2.avm mylib.avm foo.beam bar.beam
    shell$ packbeam list mylib2.avm
    mylib.beam * [284]
    mylib/priv/sample.txt [29]

## `packbeam_api` API

In addition to being an `escript` command-line utility, this project provides an Erlang API and library for manipulating AVM files.  Simply include `atomvm_packbeam` as a dependency in your `rebar.config`, and you will have access to this API.

> For more detailed information about this API, see the [`packbeam_api` Reference](packbeam_api.html).

### Creating PackBEAM files

To create a PackBEAM file, use the `packbeam_api:create/2` function.  Specify the output path of the AVM you would like to create, followed by a list of paths to the files that will go into the AVM file.  Typically, these paths are a list of BEAM files, though you can also include plain data files, in addition to previously created AVM files.  Previously-created AVM files will be copied into the output AVM file.

> Note.  Specify the file system paths to all files.  BEAM file path information will be stripped from the AVM element path data.  Any plain data files (non-BEAM files) will retain their path information.  See the [AtomVM Documentation](https://www.atomvm.net/doc/master/) about how to create plain data files in AVM files that users can retrieved via the `atomvm:read_priv/2` function.

    %% erlang
    ok = packbeam_api:create(
        "/path/to/output.avm", [
            "/path/to/foo.beam",
            "/path/to/bar.beam",
            "/path/to/myapp/priv/sample.txt",
            "/path/to/some_lib.avm"
        ]
    ).

Alternatively, you may specify a set of options with the `packbeam_api:create/3` function, which takes a map as the third parameter.

| Key | Type | Deafult | Description |
|-----|------|---------|-------------|
| `prune` | `boolean()` | `false` | Specify whether to prune the output AVM file.  Pruned AVM files can take considerably less space and hence may lead to faster development times. |
| `start` | `module()` | n/a | Specify the start module, if it can't be determined automatically from the application. |
| `application` | `module()` | n/a | Specify the application module.  The `<application>.app` file will be encoded and included as an element in the AVM file with the path `<module>/priv/application.bin` |
| `include_lines` | `boolean()` | `true` | Specify whether to include line number information in generated AVM files. |

### Listing the contents of PackBEAM files

You can list the contents of PackBEAM files using the `packbeam_api:list/1` function.  Specify the file system path to the PackBEAM file you would like to list:

    %% erlang
    AVMElements = packbeam_api:list("/path/to/input.avm").

The returned `AVMElements` is list of an opaque data structures and should not be interpreted by user applications.  However, several functions are exposed to retrieve information about elements in this list.

To get the element name, use the `packbeam_api:get_element_name/1` function, passing in an AVM element.  The return type is a `string()` and represents the path in the AVM file for the AVM element.

    %% erlang
    AVMElementName = packbeam_api:get_element_name(AVMElement).

To get the element data (as a binary) use the `packbeam_api:get_element_data/1` function, passing in an AVM element.  The return type is a `binary()` containing the actual data in the AVM element.

    %% erlang
    AVMElementData = packbeam_api:get_element_data(AVMElement).

To get the element module (as an atom) use the `packbeam_api:get_element_module/1` function, passing in an AVM element.  The return type is a `module()` and the module name of the AVM element.

Note that if the AVM element is not a BEAM file, this function returns `undefined`.

    %% erlang
    AVMElementModule = packbeam_api:get_element_module(AVMElement).

To determine if the element is a BEAM file, use the `packbeam_api:is_beam/1` function, passing in an AVM element.  The return value is a `boolean()`.

    %% erlang
    IsBEAM = packbeam_api:is_beam(AVMElement).

To determine if the element is an entrypoint BEAM (i.e., it exports a `start/0` function), use the `packbeam_api:is_entrypoint/1` function, passing in an AVM element.  The return value is a `boolean()`.

    %% erlang
    IsEntrypoint = packbeam_api:is_entrypoint(AVMElement).

### Deleting entries from PackBEAM files

You can delete entries from an AVM file using the `packbeam_api:delete/3` function. Specify the file system path to the PackBEAM file you would like to delete from, the output path you would like to write the new AVM file to, and a list of AVM elements you would like to delete:

    %% erlang
    ok = packbeam_api:delete(
        "/path/to/input.avm",
        "/path/to/ouput.avm",
        ["foo.beam", "myapp/priv/sample.txt"]
    ).

> Note.  You may specify the same values for the input and output paths.  In this case, the input AVM file will be _over-written_ by the new AVM file.

### Extracting entries from PackBEAM files

You can extract elements from an AVM file using the `packbeam_api:extract/3` function. Specify the file system path to the PackBEAM file you would like to extract from, a list of AVM elements you would like to extract, and the output directory into which would like to extract the files:

    %% erlang
    ok = packbeam_api:extract(
        "/path/to/input.avm",
        ["foo.beam", "myapp/priv/sample.txt"],
        "/tmp"
    ).
