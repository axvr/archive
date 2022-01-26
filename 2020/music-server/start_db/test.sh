#!/bin/sh

# Script to start the test DB for Enqueue.

# TODO: run this automatically when running tests?
# TODO: stop and remove container once tests finish?

process="$(docker ps -qf name=enqueue-db-test)"

if [ -n "$process" ]; then
    echo "Container already running.  ID: $process"
else
    docker pull postgres

    docker run -d \
        --name enqueue-db-test \
        -p 5454:5432 \
        -e POSTGRES_USER=postgres \
        -e POSTGRES_PASSWORD=postgres \
        -e POSTGRES_DB=enqueue \
        -e POSTGRES_INITDB_ARGS='--encoding=UTF-8' \
        postgres
fi
