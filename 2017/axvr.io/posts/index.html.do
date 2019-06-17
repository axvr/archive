#!/bin/sh

redo-ifchange $(find . -type f -name '*.md' | sed 's/\.md$/.html/') \
    "../website.template.1" "../website.template.2"

cat ../website.template.1 | sed 's/###TITLE###/Alex Vear | Posts/' >> "$3"

cat << EOF >> "$3"
<title>Posts</title>
<h1>Hopefully coherent</h1>
<p>This is where I publish opinion pieces I write.  RSS feed coming soon.</p>
EOF

# Iterate over all posts and sort in chronological order.
for article in $(find */ -name '*.html' -type f | sort -r)
do

    title=$(sed -n 's/<h1.*\?>\(.*\?\)<\/h1>/\1/p' "$article" | sed 's/\(^\s\+\|\s\+$\)//g')
    url=$(echo "/posts/$article" | sed 's/\/index.html$//')

    # Find the first paragraph, and grab the line numbers. This paragraph will
    # be used as the post description.
    desc_start=$(grep -nm 1 "<p>" "$article" | sed 's/^\([0-9]\+\):.*$/\1/')
    desc_end=$(grep -nm 1 "</p>"  "$article" | sed 's/^\([0-9]\+\):.*$/\1/')

    {
        printf "<h2><a href=\"%s\">%s</a></h2>\\n" "$url" "$title"
        printf "%s\n" "$(sed -n "${desc_start},${desc_end}p" "$article")"
    } >> "$3"

done

cat ../website.template.2 >> "$3"

# TODO: make this smarter. Add metadata stuff to the top of the post documents.
