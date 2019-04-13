#!/bin/bash

redo-ifchange "$(echo "$1" | sed 's/\.html$/.md/')" website.template.1 website.template.2

file="$(pandoc -f gfm -t html "$(echo "$1" | sed 's/\.html$/.md/')")"

title="$(echo "$file" | sed -n 's/<title>\(.*\?\)<\/title>/\1/p')"
[ "$title" = "" ] && title="Alex Vear" || title="Alex Vear | $title"

# FIXME: replace this process substitution with a POSIX compatible alternative
cat website.template.1 <(echo "$file") website.template.2 | sed "s/###TITLE###/$title/"
