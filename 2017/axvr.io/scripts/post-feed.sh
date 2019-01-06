#!/bin/sh

# Simple script to create the blog content feed.

feed_page="posts/index.html.tmp"

# Reset the feed page
printf "<title>Posts</title>\\n" > "$feed_page"

# Itterate over all articles in chronological order and take the top 15.
for article in $(find ./posts/*/ -name '*.html.tmp' | sort -r | sed 15q); do

    title=$(sed -n 's/<h1>\(.*\)<\/h1>/\1/p' "$article")
    url=$(printf "%s" "$article" | sed -n 's/\/[^/]\+$//p' | sed -n 's/^\.//p')

    # Find the first paragraph, and grab the line numbers. This paragraph will
    # be used as the post description.
    desc_start=$(grep -nm 1 "<p>" "$article" | sed -n 's/^\([0-9]\+\):.*$/\1/p')
    desc_end=$(grep -nm 1 "</p>"  "$article" | sed -n 's/^\([0-9]\+\):.*$/\1/p')

    {
        printf "<h2><a href=\"%s\">%s</a></h2>\\n" "$url" "$title"
        printf "%s\\n" "$(sed -n "${desc_start},${desc_end}p" "$article")"
        printf "<br>\\n"
    } >> "$feed_page"

    printf "listed '%s' --> '%s'\\n" "$article" "$feed_page"

done
