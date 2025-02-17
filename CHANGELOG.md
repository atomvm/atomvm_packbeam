<!--
 Copyright 2022 Fred Dushin <fred@dushin.net>

 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->
# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.7.4] - Unreleased


## [0.7.3] (2024.06.06)

- Fix broken `create` task in the cli tool.

## [0.7.2] (2023.11.24)

- Make use of profiles to minimize downstream dependencies
- Fix install.sh script on FreeBSD platform

## [0.7.1] (2023.10.22)

- Enhanced `packbeam_api` to make it more maintainable.
- Changed documentation to use [`rebar3_ex_doc`](https://hexdocs.pm/rebar3_ex_doc/readme.html)

## [0.7.0] (2022.10.17)

- Added `version` sub-command to print version to the console
- Added `-r`, `--remove` option and removed the `-i`, `--include` option, which was ineffective due to a bug.  See the [Updating](UPDATING.md) notes on the impact of these changes.

## [0.6.2] (2023.10.14)

- Added `relx` stanzas to create a standalone release of the `packbeam` utility
- Added install scripts to simplify installation for users

## [0.6.1] (2023.07.16)

- Added `extract` sub-command

## [0.6.0] (2022.12.18)

### Added
- Added ability to include `<<"Line">>` chunks in BEAM files in generated AVM files
- Added CI build

### Changed
- Changed the `packbeam_api:create` function to take a single map for optional
parameters, instead of coding paramters into function arguments.  Previous
versions of the `packbeam_api:create` function that take optional parameters
have been deprecated.

## [0.5.0] (2022.08.28)

### Added
- Added ability to specify a module name for ordinary (non-BEAM) files (API-only).
- Added support for tracking dependencies using application spec files as binaries
(API-only)
- Added PropEr test
- Added new `format` option to the `list` subcommand, supporting `csv`, `bare`,
and `default` options.

### Fixed
- Fixed a bug in parsing non-BEAM files in included AVM files, which would cause
  non-BEAM file contents to be loaded incorrectly.

### Changed
- Changed the command line syntax to support long and short option names using
GNU-style conventions; deprecated single-hyphen short options.
- Moved `packbeam` API functionality into `packbeam_api` module.
Previous `packbeam` API functions now call corresponding `packbeam_api`
functions and are deprecated.

## [0.4.1] (2022.06.19)

### Added
- Added unit tests

### Fixed
- Fixed a bug that failed to track atoms that occur in BEAM LitT tables

### Changed
- Weakened the test for finding a start BEAM file such that it only requires that the `?BEAM_START_FLAG` be set, for compatibility with [ExAtomVM](https://github.com/atomvm/ExAtomVM).

## [0.4.0] (2022.05.21)

### Added
- Added `erlfmt` plugin and formatted code.

### Fixed
- Fixed a bug that prevented packbeam files that include priv files from being properly loaded.

## [0.3.0] (2022.01.15)

### Fixed
- Fixed a bug in pulling in dependent BEAM files based on the atoms table

## [0.2.0] (2021.04.03)

### Added
- Added support for deployment to hex

### Fixed
- Uncompressed literals table

## [0.1.0] (2020.05.17)
- Initial Release
