# `do` scripts

_2022-01-31_

The simplest task runner.  Why overcomplicate things.

Just place a series of executable scripts in a `do` directory.  Done.  Invoke
as `do/script-name`.  Arbitrarily nest them if desired
(e.g. `do/database/start`).  This directory contains examples.

Since coming up with this, many of my projects now follow this format, often
using [execline](https://skarnet.org/software/execline/) or POSIX Shell as the
scripting language.

### Pros

- Easily find available commands, `ls do`, `tree do` or `tree do -C --prune --noreport`.
- No dedicated task runner tools required.
- Any scripting language(s) can be used, incluing a mix of them.
- Two letter prefix (`do`).
- Scripts do not hide the manual steps and can optionally print them (e.g. `set -x`).
  (Lang dependent.)
- Supports command grouping (subdirectories).
- Tab-completion "just works".
- `do` scripts can easily call out to other `do` scripts, e.g. `do/test` may
  invoke `do/build` and `do/database/start` before executing tests.
- Encourages simplicity in scripts and ensuring that everything can be done
  without needing the `do` scripts.
- Doesn't require that everything needs a custom task.
- Compatible with "[do-nothing scripting](https://blog.danslimmon.com/2019/07/15/do-nothing-scripting-the-key-to-gradual-automation/)".
- Can create a task which just lists helpful commands.
- Interactive commands work as expected.
- Fast execution.
- Minimal indirection.

### Cons

- Requires all users to have the chosen scripting language(s) installed.
- Need to add the [shebang](https://en.wikipedia.org/wiki/Shebang_\(Unix\))
  line to each file.
- Need to make each file executable.
- No standardised/built-in option handling.  (Lang dependent.)
- No standardised/built-in documentation/command help.  (Lang dependent.)
- Can end up with code duplication between scripts.  (Solvable by placing
  shared code in a hidden directory within `do`.)
- Duplicated code across many repositories if they have similar/identical
  tasks.  (See [do-tasks](../do-tasks) for my conceptual solution to this and
  more.)

---

_Public domain.  No rights reserved._
