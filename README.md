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

[Releases](https://hex.pm/packages/atomvm_packbeam) and
[accompanying documentation](https://hexdocs.pm/atomvm_packbeam/readme.html) are available on
[hex.pm](https://hex.pm). The documentation for the current master branch is always available from
the [`atomvm_packbeam` GitHub pages](https://atomvm.github.io/atomvm_packbeam/readme.html).

## Prerequisites

Building `packbeam` requires a version of Erlang/OTP compatible with [AtomVM](https://github.com/atomvm/AtomVM), as well as a local installation of [`rebar3`](https://rebar3.org).  Optionally, any recent version of `make` may be used to simplify builds.  Consult the [AtomVM Documentation](https://doc.atomvm.org/latest/) for information about supported OTP versions.

## Build

To build a release, run the following commands:

    shell$ rebar3 as prod release
    shell$ rebar3 as prod tar

These commands will create an Erlang tar archive containing a versioned release of the `atomvm_packbeam` tool, e.g.,

    ...
    ===> Tarball successfully created: _build/prod/rel/atomvm_packbeam/atomvm_packbeam-0.8.2.tar.gz

in your local working directory.

> IMPORTANT!  The files in this tar archive do not contain the `atomvm_packbeam` prefix, so extracting these files without care will create a `bin` and `lib` directory in the location into which files from the archive is extracted.  See the example below before proceeding!

You can use the `install.sh` script to install the `atomvm_packbeam` utility into a location on your local machine.  You will need to specify the prefix location into which you want to install the utility, together with it's current version.

    shell$ ./install.sh /opt/atomvm_packbeam 0.8.2
    atomvm_packbeam version 0.8.2 installed in /opt/atomvm_packbeam.

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

    packbeam version 0.8.2

    Syntax:
        packbeam <sub-command> <options> <args>

    The following sub-commands are supported:

        create <options> <output-avm-file> [<input-file>]+
            where:
            <output-avm-file> is the output AVM file,
            [<input-file>]+ is a list of one or more input files,
            and <options> are among the following:
                [--prune|-p]           Prune functions from init:boot/1
                [--prune-modules]      Legacy module-only pruning
                [--prune-functions]    Remove unreachable functions and literals
                [--reference <avm>]    Analyze library without bundling it (repeatable)
                [--external|-e <avm>]  Bundle library AVM (repeatable)
                [--keep M:F/A]         Additional reachable function (repeatable)
                [--suggest-drivers]    Suggest unused port drivers and NIFs
                [--precision <mode>]   full (default), adaptive, insensitive or coarse
                [--jit-types]          Trust the analysis: add types for the JIT, drop proven tests
                [--lib|-l]             Create a library avm, with no start module
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

The input files specified in the create sub-command may be among the following types:

* compiled BEAM files (typically ending in `.beam`)
* Previously created AVM files
* "Normal" files, e.g., text files, binary files, etc.

Note that beam files specified are stripped of their path information, inside of the generated AVM file.  Any files that have the same name will be added in the order they are listed on the command line.  However, AtomVM will only resolve the first such file when loading modules at run-time.

#### Start Entrypoint

If you are building an application that provides a start entrypoint (as opposed to a library,
suitable for inclusion in another AVM file), then at least one beam module in an AVM file must
contain a `start/0` (i.e., a function called `start` with arity 0) or `main/1` entry-point.  AtomVM
will use this entry-point as the first function to execute, when starting.

When explicitly setting the `--start` module no other module will be tagged as an entrypoint, even
if multiple modules export `start/0`.

> Note.  It is conventional, but not required, that the first beam file in an AVM file contains the
`start/0` entry-point.

If your application has multiple modules with exported `start/0` functions, or you are creating an
escript with a `main/1`, you may use the `--start <module>`  (alternatively, `-s <module>`) option
to specify the entrypoint module.  The `<module>` parameter should be the module name (without the
`.beam` suffix, e.g., `main`).

A previously created AVM file file may be supplied as input (including the same file specified as output, for example).  The contents of any input AVM files will be included in the output AVM file.  For example, if you are building a library of BEAM files (for example, none of which contain a `start/0` entry-point), you may want to archive these into an AVM file, which can be used for downstream applications.

In addition, you may specify a "normal" (i.e., non-beam or non-AVM) file.  Normal files are labeled with the path specified on the command line.

    shell$ packbeam create mylib.avm mylib.avm mylib/priv/sample.txt

> Note.  It is conventional in AtomVM for normal files to have the path `<module-name>/priv/<file-name>`.

#### Creating Libraries

Libraries can be packed using the `--lib` (alternatively, `-l`) option. This will create a packed
avm archive without auto selecting a start module, even if one or more of the included modules
export `start/0`.

#### Pruning

If you specify the `--prune` (alternatively, `-p`) flag, then `packbeam` will only include the functions and literals your application can reach, including from the libraries you bundle with it.  See [Function and literal pruning](#function-and-literal-pruning).

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

> Note.  Specify the file system paths to all files.  BEAM file path information will be stripped from the AVM element path data.  Any plain data files (non-BEAM files) will retain their path information.  See the [AtomVM Documentation](https://doc.atomvm.org/latest/) about how to create plain data files in AVM files that users can retrieved via the `atomvm:read_priv/2` function.

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

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `prune` | `boolean() \| functions \| modules` | `false` | `true` or `functions` prunes functions and literals; `modules` selects legacy module pruning. |
| `lib` | `boolean()` | `false` | Create a library (no entrypoint) |
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

### Function and literal pruning

`--prune` (or `-p`) keeps only the code your application can actually run.
Starting from the way AtomVM boots it, packbeam follows every call and removes
the functions, and the literals, that nothing reaches. Libraries are pruned
along with your own code, so a small application no longer carries the whole
standard library.

packbeam needs the library your application runs with. If you flash the
library together with the application, bundle it with `-e` (or `--external`)
and it is pruned too:

```sh
packbeam create -p --start my_app -e atomvmlib.avm my_app.avm my_app.beam
```

If the library is flashed separately, pass it with `--reference`: it is
analyzed but not included in the output.

```sh
packbeam create -p --start my_app --reference atomvmlib.avm my_app.avm my_app.beam
```

Both options can be repeated. A pruned package only works with the start
module and the library it was built for. As on the device, the first copy of a
module wins: bundled modules shadow referenced ones, and earlier references
shadow later ones.

Without the library, packbeam assumes each call into it may call back the
functions and modules it is given, and send any message. The package is
usually larger, and packbeam warns once for each library module that is
called.

`--prune-modules` only drops whole modules that nothing imports or names.

#### When a call cannot be resolved

Some calls are only known at run time, for example a module name read from a
file or received in a message. packbeam keeps every function such a call could
plausibly reach and prints a warning naming the call and how it is reached.
Functions the analysis cannot find at all, because nothing in the code names
them, are removed. If your application needs such a function, keep it
explicitly:

```sh
packbeam create -p --start my_app --keep my_callbacks:handle_event/2 ...
```

This is also needed with distributed Erlang: a function that only other nodes
call, with `rpc:call/4` or `erpc:call/4` for example, is not called by
anything in the package, so it must be kept with `--keep`.

To prune a library with no start module, combine `--lib` with `--keep`.

#### Options

* `--precision <mode>` trades precision for speed. Pruning a typical
  application with its library takes about 20 to 25 seconds with the default,
  `full`, which is meant for release builds. `adaptive` gives the same result
  on most applications. `insensitive` and `coarse` are faster and keep more
  code; use them for very large programs, such as a bundled Erlang compiler.
* `--jit-types` also makes the output faster and smaller once compiled to
  native code by AtomVM's JIT: it passes what the analysis learned about
  types to the JIT, and removes tests the analysis proves always pass
  (typically 3% less native code). It trusts the analysis: should the
  analysis be wrong, native code could crash instead of raising an exception,
  so it is off by default. It only has an effect with `--prune`.
* `--suggest-drivers` lists the port drivers and NIFs the application never
  uses, and the build switches that would remove them from a dedicated AtomVM
  build.

#### Using pruning from a build tool

The same options are available through `packbeam_api:create/3`:

```erlang
packbeam_api:create("my_app.avm", BeamPaths ++ BundledAvmPaths, #{
    prune => true,
    start_module => my_app,
    references => ["atomvmlib.avm"],
    keep => [{my_callbacks, handle_event, 2}],
    precision => full,
    jit_types => false,
    suggest_drivers => false,
    diagnostics => fun(Report) -> io:format("~p~n", [Report]) end,
    precompile => fun(Module, Beam) -> my_aot:compile(Module, Beam) end
}).
```

`diagnostics` receives a report with the reachable functions, the warnings
and the drivers in use; without it, warnings are printed to standard error.
`precompile` is called with each module after pruning, to compile it ahead of
time: modules that are already compiled to native code cannot be pruned.

The input BEAM files must be supported by the OTP version running packbeam.
