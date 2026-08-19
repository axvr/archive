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

Start the server in local development mode:

```sh
# Start local server + DB + nREPL server.
do/server/local/start

# Start local DB.
do/database/local/start

# Migrate DB.
clojure -X:local:migrate

# Run server.
clojure -X:local:run

# Run with nREPL server.
clojure -X:nrepl:local:run

# Start REPL without starting server.
clj -X:local:run :server? false

# Stop local development DB.
docker stop enqueue-db-local
```

By default, the server will be started at <http://localhost:7881/>.


### Run tests

```sh
# Start test DB.
do/database/test/start

# Run all tests.
clojure -X:test

# Run specific test types.
clojure -X:test:unit
clojure -X:test:integration
clojure -X:test:system

# Run multiple types at once.
clojure -X:test :types '[:unit :integration :system]'

# Run linter.
clojure -M:lint

# Stop test DB.
docker stop enqueue-db-test
```


## Legal

Copyright © 2020–2022, [Alex Vear](https://www.alexvear.com).
