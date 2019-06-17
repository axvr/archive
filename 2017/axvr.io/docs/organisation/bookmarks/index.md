<title>Bookmarking</title>

[\<-- back](../)

# My bookmarking system

These pages detail the bookmarking system I use while web browsing.

It makes heavy use of browser bookmarks (it can also be done with regular text
files, but requires extra tools to sync between devices).

- [Version 3](v3) - Introduced what I call "research sessions".
- [Version 2](v2) - Added missing categories and introduced sub-categories.
- [Version 1](v1) - Initial version, extremely simple.

## Design decisions

- Low maintenance, and easy to manage (mostly flat).
- Contain a miscellaneous category.
- Easy to implement using simple text files.
- Simple to understand, and easy to use.
- Extensible, easily parsed by simple tools.

## Auto-archiver

By using this simple system, it is possible to create a script which accepts
the bookmark list, and sends each of the links to the [Wayback
Machine](https://web.archive.org/), and creates local archives of the bookmarks
using tools such as [wget](https://www.gnu.org/software/wget/), and
[youtube-dl](https://rg3.github.io/youtube-dl/).
