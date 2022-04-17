# Enqueue API

Source code of the Enqueue API.


## Setup


### Dependencies

Install the following dependencies.

- [OpenJDK](https://openjdk.java.net/)
- [Clojure CLI tools](https://clojure.org/guides/getting_started#_clojure_installer_and_cli_tools)
- [Docker](https://www.docker.com/)
- [Libsodium](https://libsodium.gitbook.io/doc/installation)


## Usage

Start the server in development mode:

```sh
do/server/dev/start
```

The server will be started at <http://localhost:7881/>.

(Note: you will need to use a REPL server (such as a socket REPL) to connect to
the API.)


### Run tests

```sh
# Run unit tests.
do/test/unit

# Run unit, integration and system tests.
do/test/all
```


## Legal

Copyright © 2020–2022, [Alex Vear](https://www.alexvear.com).
