#!/bin/sh

# Simple script to convert Markdown pages to HTML-content pages

for file in $(find . -name '*.md'); do
    [ "$file" = "./README.md" ] && continue

    if [ ! "$(command -v pandoc)" ]; then
        printf "Error: Pandoc is not installed, cannot compile MD to HTML\n"
        exit 1
    fi

    content_file="$(printf "$file" | sed -n 's/\.md$/.html.tmp/p')"
    pandoc -f gfm-gfm_auto_identifiers -t html "$file" > "$content_file"

    printf "converted '%s' --> '%s'\\n" "$file" "$content_file"
done
