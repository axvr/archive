#!/bin/sh

# Script to compile all Markdown files to HTML files.

for md_file in $(find . -name '*.md' -type f); do
    [ "$md_file" = "./README.md" ] && continue

    if [ ! "$(command -v pandoc)" ]; then
        printf "Error: Pandoc is not installed, cannot compile MD to HTML\n"
        exit 1
    fi

    html_file="$(printf "$md_file" | sed -n 's/\.md$/.html.tmp/p')"
    pandoc -f gfm-gfm_auto_identifiers -t html "$md_file" > "$html_file"

    printf "converted '%s' --> '%s'\\n" "$md_file" "$html_file"
done
