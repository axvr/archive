# Enqueue API

Source code of the Enqueue API.


## Setup


### Dependencies

Install the following dependencies.

- [Clojure CLI tools](https://clojure.org/guides/getting_started#_clojure_installer_and_cli_tools)
- [Docker](https://www.docker.com/)
- [libsodium](https://libsodium.gitbook.io/doc/installation)


## Usage

Start the server in development mode:

```sh
./start_db/dev.sh
clojure -X:dev:run
```

The server will be started at <http://localhost:7881/>.

(Note: you will need to use a REPL server (such as a socket REPL) to connect to
the API.)


### Run tests

```sh
# Start test DB
./start_db/test.sh

# Run unit tests.
clojure -X:test

# Run unit, component and system tests.
clojure -X:test :types '[:unit :component :system]'
```


## Legal

Copyright © 2020–2021, [Alex Vear](https://www.alexvear.com).
