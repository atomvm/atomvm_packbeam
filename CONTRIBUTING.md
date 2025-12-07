<!--
 Copyright 2022 Fred Dushin <fred@dushin.net>

 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Contributing

Before contributing, please read carefully our [Code of Conduct](CODE_OF_CONDUCT.md) and
the following contribution guidelines.

Please, also make sure to understand the [Apache 2.0 license](LICENSE) and the
[Developer Certificate of Origin](https://developercertificate.org/).

Last but not least, **do not use GitHub issues for vulnerability reports**, read instead the
[security policy](SECURITY.md) for instructions.

## Git Recommended Practices

* Commit messages should have a
* [summary and a description](https://github.com/erlang/otp/wiki/writing-good-commit-messages)
* Avoid trailing white spaces
* Always `git pull --rebase`
* [Clean up your branch history](https://git-scm.com/book/id/v2/Git-Tools-Rewriting-History) with
`git rebase -i`
* All your intermediate commits should build

## Coding Style

### Erlang Code

Format with `erlfmt` enforced style:

```shell
    $ rebar3 fmt
```
