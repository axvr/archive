#!/bin/sh

for file in `find . -name '*.md' -type f | grep -v '^./posts' | sed 's/\.md$/\.html/'`
do
    redo-ifchange "$file" &
done

wait

redo-ifchange posts/index.html
