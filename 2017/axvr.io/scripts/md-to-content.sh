#!/bin/sh

# Simple script to convert Markdown pages to HTML-content pages

for file in $(find . -name '*.md'); do
    if [ $file == "./README.md" ]; then
        continue
    elif [ "$(printf "$file" | grep "\.md$" )" != "" ]; then
        if [ "$(command -v pandoc)" == "" ]; then
            printf "Error: Pandoc is not installed, cannot compile MD to HTML\n"
            exit 1
        fi
        content_file="$(printf "$file" | sed -n 's/\.md$/.content.html/p')"
        pandoc -f gfm-gfm_auto_identifiers -t html "$file" > "$content_file"

        printf "%s --> %s\\n" "$file" "$content_file"
    fi
done
