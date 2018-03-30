#!/bin/sh

# Simple script to generate the static HTML pages for my website


# TODO RSS feed construction (and where to put the link)

for FILE in $(find . -name '*-content.html' -o -name '*.md'); do

    if [ $FILE == "./README.md" ]; then
        continue
    fi

    # Tags could be comments, and the title could be a <h1> tag
    TITLE=$(sed -n 's/^###TITLE###:\(.*\)$/\1/p' "$FILE")
    AUTHOR=$(sed -n 's/^###AUTHOR###:\(.*\)$/\1/p' "$FILE")

    OUT_FILE=$(echo "$FILE" | sed -n 's/\(\.md\|-content\.html\)$/\.html/p')
    cp "template.html" "$OUT_FILE"

    sed -i "s/###TITLE###/$TITLE/" "$OUT_FILE"
    sed -i "s/###AUTHOR###/$AUTHOR/" "$OUT_FILE"

    # TODO If a markdown file, convert it first (after removing "###TITLE###")

    sed -i -e "/###CONTENT###/r $FILE" "$OUT_FILE"
    sed -i '/###\(TITLE\|CONTENT\|AUTHOR\)###/d' "$OUT_FILE"

    echo "$FILE --> $OUT_FILE"

done

