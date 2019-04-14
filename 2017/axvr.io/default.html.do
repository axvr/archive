#!/bin/sh

redo-ifchange "$(echo "$1" | sed 's/\.html$/.md/')" website.template.1 website.template.2

file="$(pandoc -f gfm -t html "$(echo "$1" | sed 's/\.html$/.md/')")"

title="$(echo "$file" | sed -n 's/<title>\(.*\?\)<\/title>/\1/p')"
[ "$title" = "" ] && title="Alex Vear" || title="Alex Vear | $title"

cat website.template.1 | sed "s/###TITLE###/$title/" >> "$3"
echo "$file" >> "$3"
cat website.template.2 >> "$3"
