docker_volume_exists () {
    test -n docker volume ls -qf name="$1"
}

docker_volume_create () {
    if ! docker_volume_exists "$1"; then
        docker volume create "$1"
    fi
}

docker_container_running () {
    test -n docker container ls -aqf name="$1"
}

docker_process_running () {
    test -n docker ps -qf name="$1"
}
