#!/bin/sh

# Simple script to generate the static HTML pages for my website

for file in $(find . -name '*.content.html'); do

    title=$(sed -n 's/<h1>\(.*\)<\/h1>/\1/p' "$file")
    author=$(sed -n 's/^###AUTHOR###:\(.*\)$/\1/p' "$file")

    out_file=$(echo "$file" | sed -n 's/\(\.md\|\.content\.html\)$/\.html/p')
    cp "template.html" "$out_file"

    sed -i "s/###TITLE###/$title/" "$out_file"
    sed -i "s/###AUTHOR###/$author/" "$out_file"

    sed -i -e "/###CONTENT###/r $file" "$out_file"
    # sed -i '/\(^<h1>\(.*\)<\/h1>\|###\(CONTENT\|AUTHOR\)###\)/d' "$out_file"
    sed -i '/^<h1>\(.*\)<\/h1>/d' "$out_file"

    printf "%s --> %s\\n" "$file" "$out_file"

done
