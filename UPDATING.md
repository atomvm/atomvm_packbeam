<!---
  Copyright 2023 Fred Dushin <fred@dushin.net>

  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# AtomVM Update Instructions

## 0.6.* -> 0.7.*

- The default behavior of not generating line number information in BEAM files has changed.  By default, line number information will be generated in BEAM files.  You can remove line number information using from BEAM files by using the `-r` (or `--remove_lines`) flags to the `create` subcommand.  Note that in versions 0.6 of this tool, the `--include_lines` flag was ignored due to a bug in the code.
