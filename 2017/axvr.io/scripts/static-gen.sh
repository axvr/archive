#!/bin/sh

# Script to generate the static HTML pages for my website.

for file in $(find . -name '*.html.tmp'); do

    title=$(sed -n 's/<title>\(.*\?\)<\/title>/\1/p' "$file")
    [ "$title" = "" ] && title="Alex Vear" || title="Alex Vear | $title"

    out_file=$(echo "$file" | sed -n 's/\.html\.tmp$/\.html/p')
    cp "template.html" "$out_file"

    sed -i "s/###TITLE###/$title/" "$out_file"

    sed -i -e "/###CONTENT###/r $file" "$out_file"

    # FIXME remove `<title>` tags in body
    # sed -i '/^<title>.*\?<\/title>/d' "$out_file"

    printf "injected '%s' --> '%s'\\n" "$file" "$out_file"

done
