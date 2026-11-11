<!---
  Copyright 2023 Fred Dushin <fred@dushin.net>

  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# AtomVM Update Instructions

## 0.8.* -> 0.9.*

- `--prune` (`-p`), and `prune => true` in `packbeam_api:create/3`, now prune by function instead of
by module: within the modules that are kept, the functions and literals nothing in the package
reaches are removed, including exported functions. An application that relies on functions only
called from outside the package must keep them with `--keep M:F/A` (`keep` option). This is the
case for functions called by other nodes through distributed Erlang (`rpc:call/4`, `erpc:call/4`),
from a remote shell, or through a module or function name the analysis cannot see, such as one read
from a file. See the README for details.
- Pruning now needs the library the application runs with: bundle it with `-e` (`--external`), or
pass it with `--reference` if it is flashed separately. Without it, more code is kept and a warning
is printed for each library module that is called.
- Pruning a library with `--lib` requires the functions it provides to be given with `--keep`.
- BEAM files precompiled to native code cannot be pruned by function.
- The previous behavior is available with `--prune-modules` (`prune => modules`). The deprecated
`create/4` and `create/5` functions keep pruning by module.

## 0.7.* -> 0.8.*

- No changes have been made to default behaviors that should affect any existing applications that
depend on `atomvm_packbeam` or its escript usage. New options have been added, consult the
documentation for details.

## 0.6.* -> 0.7.*

- The default behavior of not generating line number information in BEAM files has changed.  By default, line number information will be generated in BEAM files.  You can remove line number information using from BEAM files by using the `-r` (or `--remove_lines`) flags to the `create` subcommand.  Note that in versions 0.6 of this tool, the `--include_lines` flag was ignored due to a bug in the code.
