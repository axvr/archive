#!/bin/sh

# Script to start the development DB for Enqueue.

volume="$(docker volume ls -qf name=enqueue-db-dev-data)"

if [ -n "$volume" ]; then
    echo "Volume already exists.  Name: $volume"
else
    echo "Creating Docker volume."
    docker volume create enqueue-db-dev-data
fi

process="$(docker ps -qf name=enqueue-db-dev)"

if [ -n "$process" ]; then
    echo "Container already running.  ID: $process"
else
    docker pull postgres

    docker run -d \
        --name enqueue-db-dev \
        -p 5455:5432 \
        -e POSTGRES_USER=postgres \
        -e POSTGRES_PASSWORD=postgres \
        -e POSTGRES_DB=enqueue \
        -e POSTGRES_INITDB_ARGS='--encoding=UTF-8' \
        -v enqueue-db-dev-data:/home/postgres/data \
        postgres
fi
