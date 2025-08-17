# `do` tasks

_2022-11-16_

A design for a future task runner.

What follows is a bunch of notes from various points in time while designing
a possible task runner due to my dissatisfaction with existing ones when my
[`do` scripts](../do-scripts) were not capable enough in larger cross-repo
projects.

---

## 2022-11-16: Explaining the initial idea

The main problems with my [`do` scripts](../do-scripts) the possible solution I explained before are:

1. In big projects, there could be too many files making them a pain to edit.
2. Code duplication across many repositories that use similar or identical
   scripts.
3. Needed to add a shebang to each script and make them executable which was
   a bit annoying.
4. No standardised/built-in documentation/command help.
5. No standardised/built-in option handling.

To solve the above problems we need a task runner that:

1. Uses fewer files (e.g. combine related scripts into single files that make
   sense, such as a "database" file that contains all the database scripts.)
2. Enables easy sharing of code between projects.

As far as I'm aware, there is nothing out there that can do this.


### What to ***do***?

The closest thing I know of to this is [Mozilla's Mach][mach], a "generic
command dispatcher".  However while I do like Mach, it is a little complex and
not really ideal as it is not dynamic enough.

[mach]: https://firefox-source-docs.mozilla.org/mach/index.html

How could this thing be used?

```
$ do
A simple, generic command dispatch tool.
Docs: https://github.com/axvr/do

Subcommands:
  database    | Start/stop/remove local and test databases.
  server      | ...
  ...         | ...

Options:
  --help      | Get help on a command.
  --cmds      | Get a list of available commands.
  --prune     | Remove cached Git dependencies.
  --version   | Print current version of "do".


$ do database
Start/stop/remove local and test databases.

Subcommands:
  local       | Control the local development DB.
  test        | Control the local DB used in tests.

Options:
  --help      | Get help on a command.
  --cmds      | Get a list of available commands.


$ do database local --help
Control the local development DB.

Subcommands:
  start       | Start the local dev database.
  stop        | Stop the local dev database.
  remove      | Purge the local dev database.

Options:
  --help      | Get help on a command.
  --cmds      | Get a list of available commands.


$ do database local start --help
[ACTION] Start the local dev database.

Options:
  --help      | Get help on a command.
  --cmds      | Get a list of available commands.
  --plan      | Print the script that would be executed.
  --          | Subsequent args will be passed to the script.


$ do database local start
[do] Fetching Git dep: github.com:axvr/doscripts:master:docker.sh
[do] Built script: /tmp/do/sh/database-local-start.sh
[do] Log file:     /tmp/do/log/database-local-start.log
[do] Executing script...
...
```

All of these available commands are dynamically generated based on the project do is run in.


### How could it work?

The do command uses files in the `do` directory to figure out what it can do.

```
do/
  database.yml
  docker.sh
  ...
```


```yml
# do/database.yml

database:
  help: Database helper commands.
  deps:
    - do/docker.sh


database local:
  help: Control the local development DB.
  vars:
    DB_VOLUME: 'my-service-local-database-data'
    DB_CONTAINER: 'my-service-local-database'
    DB_PORT: 5683

database local start:
  help: Start an instance of the local development DB.
  do: |
    docker_volume_create "$DB_VOLUME"

    if docker_container_exists "$DB_CONTAINER"; then
        docker start "$DB_CONTAINER"
    fi

    if ! docker_process_exists "$DB_CONTAINER"; then
        # TODO: how to get the DB user, password, port and name?  direnv?
        docker run -d \
            --name "$DB_CONTAINER" \
            -p "${DB_PORT}:5432" \
            -e POSTGRES_USER="$POSTGRES_USER" \
            -e POSTGRES_PASSWORD="$POSTGRES_PASSWORD" \
            -e POSTGRES_DB="$POSTGRES_DATABASE" \
            -v "${DB_VOLUME}:/var/lib/postgresql/data" \
            postgres:12.7-alpine
    fi

database local stop:
  help: Stop the local development DB.
  do: |
    if docker_process_exists "$DB_CONTAINER"; then
        docker stop "$DB_CONTAINER"
    fi

database local remove:
  help: Completely remove the local DB.
  do: |
    do database local stop

    if docker_container_exists "$DB_CONTAINER"; then
        docker rm "$DB_CONTAINER"
    fi

    if docker_volume_exists "$DB_VOLUME"; then
        docker volume rm "$DB_VOLUME"
    fi


database test:
  help: Control the test development DB.
  vars:
    DB_VOLUME: 'my-service-test-database-data'
    DB_CONTAINER: 'my-service-test-database'

# database test start:
#   help: Start a local DB for tests.
#   do: |
# TODO: CI server should get postgres image from AWS ECR.
```

Due to YAML complexity, I propose that this be a strict YAML subset.


## 2022-12-31: Extra thoughts

- Build shell scripts with `set -x` and `execve` them.
- Run commands with [Oil shell](https://oils.pub/)? Option to choose between osh and oil.
- `set -x` in oil?
  - `set -x` hide secrets!
- Should `do` download its own version of Oil if not found in PATH?

### Similar projects

- Mach
- https://github.com/casey/just
- https://taskfile.dev/
- https://github.com/adriancooney/Taskfile
- https://gulpjs.com/
- https://gruntjs.com/
- https://github.com/ianthehenry/sd

#### Differences

- Structure and group your tasks into separate files, or don't. Your choice.
- Uses Oil instead of the default system Shell for consistency and ease of use.
- Use external scripts to avoid duplicate code and easy task updates across many
projects.
- Simplest syntax.
- Simple implementation.
- Transparent execution. (--plan)
- Tab completion of tasks.
- Short command name.
- Simple implementation.
- Just a script builder and executor.
- Safer defaults by using Oil.
- Prints what it is running as it does it.
- Generated script is available to view, so it can be checked in case of error.
- Better error messages (with Oil).
- Language agnostic.
- Fast.
- Simple.
- Simple.
- Simple.
- Dotenv integration?

## UNKNOWN: Later notes

- Interactive command output
- Easy colour output
- `--time` - print the time it took to run each part and the whole script?
- `--bump` - update all deps to latest Git hash
- Add prompts for destructive actions.
- Write Python code inline?
  - Chose arbitrary scripting language?
- How to log execution times?
- `shellcheck <(do database local start --plan)`
- Generate 4 helper shell functions for scripts: log, err, warn, do

```
do --shell ???
do> ...
```

```yml
# do/database.yml
database:
  help: Database commands
  deps:
    - github.com:axvr/doscripts/...

database local start:
  help: |
    Start an instance of the local DB.
  do: |
    do ensure docker
    docker run -it ...
    log -g "DB started"
    # TODO: custom functions? do fn ...
```

```
do database local start --
do fn ...
do --cmds database
do database --help
```

- Everything automatically logged. On error, prints log file, which includes timestamps.
- Load shell scripts from GH or do dir.

Language?
- Go?
- Python? - Has StrictYAML
- Scheme? - Which one? YAML parser?
- Babaska?
- JavaScript?
- TypeScript?

### Preamble

```sh
#!/bin/sh

DO_LOG="/tmp/myLog.log"

exec > "$DO_LOG" 2>&1

(
  DO_ROOT=
  DO_CWD=
  DO_DIR=
  DO_CMD=
  DO_ARGS=

  cd "$DO_ROOT"

  do () { "$DO_CMD" "$@" }

  log () {
    # print to stderr
  }

  err () { log -r "$@" exit 1; }

  warn () { log -o "$@" }

  # deps are inserted here?

  set -e
  set -x

  # the script is inserted here
)

exit 0
```

- Python execve https://stackoverflow.com/a/46548514
- Go execve https://pkg.go.dev/syscall#Exec

Why `execve`?
- Slightly improved security
- Shell used can be changed more easily - I may not allow this though
- Endless loop can be killed more easily as the PID doesn't change
- "do" no longer needs to run, so there is no reason to keep it around.
- Simpler implementation.

## 2025-08-17: Reflective notes

If I were to implement this project today, I would do it in
[CHICKEN Scheme](https://www.call-cc.org) and the default scripting language
would be [execline](https://skarnet.org/software/execline/) instead of Oils.

I believe execline would be the best choice (with some helpers of my own) since
it effectively does much of what I wanted to achieve security-wise with the
Bernstein chaining.  Execline also solves shell expansion issues and provides
the `envfile` utility which provides `dotenv` functionality but better.

In many of my old-style [`do` scripts](../do-scripts) I now use `execline` over
POSIX shell.

However I would also likely still provide the option of switching scripting
languages.

I was never too happy with the use of YAML, but it does read well and avoids
the need for a custom file format.
