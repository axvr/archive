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
# Start development DB.
do/database/dev/start

# Migrate DB.
clojure -X:dev:migrate

# Run server.
clojure -X:dev:run

# Stop development DB.
docker stop enqueue-db-dev
```

The server will be started at <http://localhost:7881/>.

(Note: you will need to use a REPL server (such as a socket REPL) to connect to
the API.)


### Run tests

```sh
# Start test DB.
do/database/test/start

# Run unit tests.
clojure -X:test

# Run unit, integration and system tests.
clojure -X:test :types '[:unit :integration :system]'

# Stop test DB.
docker stop enqueue-db-test
```


## Legal

Copyright © 2020–2022, [Alex Vear](https://www.alexvear.com).
