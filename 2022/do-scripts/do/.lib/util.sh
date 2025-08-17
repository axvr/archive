#!/bin/sh

# Helper functions for `do` scripts.  Import with `. do/.lib/util.sh`

abort() { echo >&2 "$*"; exit 1; }

# Docker functions for local and test DBs.
is_db_running() { test -n "$(docker ps -qf "name=$1")"; }
has_db_container() { test -n "$(docker container ls -aqf "name=$1")"; }
has_db_volume() { test -n "$(docker volume ls -qf "name=$1")"; }
