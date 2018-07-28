#!/bin/sh

# Simple script to create the blog content feed for a HTML-content page.

# TODO RSS feed construction (and where to put the link)

feed_page="index.content.html"

# Reset the feed page
printf "<h1>Home</h1>\\n###AUTHOR###:Alex Vear\\n" > "$feed_page"

for article in $(find ./blog/ -name '*.content.html' | sort -r | sed 10q); do

    title=$(sed -n 's/<h1>\(.*\)<\/h1>/\1/p' "$article")
    url=$(printf "%s" "$article" | sed -n 's/\/[^/]\+$//p' | sed -n 's/^\.//p')
    desc_start=$(grep -nm 1 "<p>" "$article" | sed -n 's/^\([0-9]\+\):.*$/\1/p')
    desc_end=$(grep -nm 1 "</p>"  "$article" | sed -n 's/^\([0-9]\+\):.*$/\1/p')

    {
        printf "<h2><a href=\"%s\">%s</a></h2>\\n" "$url" "$title"
        printf "%s\\n" "$(sed -n "${desc_start},${desc_end}p" "$article")"
        printf "<br>\\n"
    } >> "$feed_page"

    printf "%s --> %s\\n" "$article" "$feed_page"

done
