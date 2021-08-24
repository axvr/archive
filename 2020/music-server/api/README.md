# Enqueue API

Source code of the Enqueue API.


## Setup


### Dependencies

Install the following dependencies.

- [Clojure CLI tools](https://clojure.org/guides/getting_started#_clojure_installer_and_cli_tools)
- [PostgreSQL](https://www.postgresql.org/)
  - [Install on Fedora](https://fedoraproject.org/wiki/PostgreSQL)
- [libsodium](https://libsodium.gitbook.io/doc/installation)

Then complete the set-up of the local PostgreSQL server and set the password of
the postgres account in the `config/*/config.edn` files.

On Fedora:

```sh
# Install dependencies.
sudo dnf upgrade --refresh
sudo dnf install libsodium postgresql-server

# Setup PostgreSQL server.
sudo postgresql-setup --init-db --unit postgresql
sudo systemctl enable postgresql.service

# Set password for "postgres" account.
sudo -u postgres psql
\password postgres
\quit

# Set all values in the "method" column to "md5".
sudo -e /var/lib/pgsql/data/pg_hba.conf

# Restart DB server.
sudo systemctl restart postgresql.service
```


### Create databases

After installing the dependencies you need to create the databases used by
Enqueue.

```sql
sudo -u postgres psql
create database enqueue ENCODING = 'UTF-8';
create database enqueue_test ENCODING = 'UTF-8';
\quit
```


## Usage

Start the server in development mode:

```sh
clojure -X:dev:run
```

The server will be started at <http://localhost:7881/>.

(Note: you will need to use a REPL server (such as a socket REPL) to connect to
the API.)


### Run tests

```sh
# Run unit and integration tests.
clojure -X:test

# Run unit, integration and E2E tests.
clojure -X:test :types '[:unit :integration :e2e]'
```

(Note: the CI pipeline doesn't have database access, so E2E tests aren't run
there.)


## Legal

Copyright © 2020–2021, [Alex Vear](https://www.alexvear.com).
