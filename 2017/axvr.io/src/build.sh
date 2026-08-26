#!/bin/sh

# No rights reserved.  Public domain.

# TODO: atomic builds.
# TODO: use GNU parallel?
# TODO: remove duplicate code.

if [ -d dist ]; then
    rm -rf dist/*
    cp -r static/* dist
else
    git worktree remove -f dist
    git worktree add dist master
    exec "$0"
fi

(
    cd content
    # TODO: don't require assets dir hack.
    for i in $(find . -name 'assets' -type d); do
        rsync -rR "$i" ../dist/
    done
)

dest () { echo "$1" | sed 's/^content\/\(.*\)\..*\?$/dist\/\1.html/'; }

for page in $(find content/ -type f -name '*.html'); do
    {
        dest="$(dest "$page")"
        mkdir -p "$(dirname "$dest")"
        ./src/2sp < "$page" | ./src/gen 'template.html' "$dest"
        echo "$page --> $dest"
    } &
done

for page in $(find content/ -type f -name '*.md'); do
    {
        dest="$(dest "$page")"
        mkdir -p "$(dirname "$dest")"
        ./src/2sp < "$page" | pandoc -f gfm -t html | ./src/gen 'template.html' "$dest"
        echo "$page -> $dest"
    } &
done

wait
