#!/bin/sh

# Simple script to generate the static HTML pages for my website

for FILE in $(find . -name '*-content.html'); do

    TITLE=$(sed -n 's/^###TITLE###:\(.*\)$/\1/p' "$FILE")
    AUTHOR=$(sed -n 's/^###AUTHOR###:\(.*\)$/\1/p' "$FILE")

    OUT_FILE=$(echo "$FILE" | sed -n 's/\(\.md\|-content\.html\)$/\.html/p')
    cp "template.html" "$OUT_FILE"

    sed -i "s/###TITLE###/$TITLE/" "$OUT_FILE"
    sed -i "s/###AUTHOR###/$AUTHOR/" "$OUT_FILE"

    sed -i -e "/###CONTENT###/r $FILE" "$OUT_FILE"
    sed -i '/###\(TITLE\|CONTENT\|AUTHOR\)###/d' "$OUT_FILE"

    printf "%s --> %s\\n" "$FILE" "$OUT_FILE"

done
