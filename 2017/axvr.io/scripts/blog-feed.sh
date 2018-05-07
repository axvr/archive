#!/bin/sh

# Simple script to create the blog content feed for a HTML-content page.

FEED_PAGE="index-content.html"

# Reset the feed page
printf "###TITLE###:Home\\n###AUTHOR###:Alex Vear\\n" > "$FEED_PAGE"

for ARTICLE in $(find ./blog/ -name '*-content.html' | sort -r | sed 10q); do

    TITLE=$(sed -n 's/^###TITLE###:\(.*\)$/\1/p' "$ARTICLE")
    URL=$(printf "%s" "$ARTICLE" | sed -n 's/\/[^/]\+$//p' | sed -n 's/^\.//p')
    DESC_START=$(grep -nm 1 "<p>" "$ARTICLE" | sed -n 's/^\([0-9]\+\):.*$/\1/p')
    DESC_END=$(grep -nm 1 "</p>"  "$ARTICLE" | sed -n 's/^\([0-9]\+\):.*$/\1/p')

    {
        printf "\\n<div class=\"item\">\\n"
        printf "<h2><a href=\"%s\">%s</a></h2>\\n" "$URL" "$TITLE"
        printf "%s\\n" "$(sed -n "${DESC_START},${DESC_END}p" "$ARTICLE")"
        printf "</div>\\n"
    } >> "$FEED_PAGE"

    printf "%s --> %s\\n" "$ARTICLE" "$FEED_PAGE"

done
