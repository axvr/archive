<title>Bookmarking</title>

[\<-- back](../)

# My bookmarking system

I believe I have managed to come up with a functional and efficient bookmarking
solution. It makes heavy use of browser bookmarks (it can also be done with
regular text files, e.g. Org-mode and Markdown, however the ability to
automatically sync between devices (without the use of additional tools) is
lost).

Some design considerations that I made were:

- Low maintenance, and easy to manage.
- Mostly flat, rely on search (because there will always be edge cases when
  trying to categorise everything).
- Easy to implement as simple text files.
- Simple to understand, and easy to use.
- Extensible, and easily parsed by simple tools.

## The system

I have designed the system to be as low maintainance as possible, while still
providing basic organisation.

The system operates using a small number of bookmark folders (i.e.
_categories_).

- [Version 1](v1)
- [Version 2](v2)
- [Version 3](v3)

## Creating an auto-archiver

By using this simple system, I should be able to create a simple script which
accepts the exported bookmarks file, and sends each of the links to the
[Wayback Machine](https://web.archive.org/), and creates local archives of the
bookmarks using tools such as [wget](https://www.gnu.org/software/wget/), and
[youtube-dl](https://rg3.github.io/youtube-dl/).

### Considerations still to make

A convention may need to be created to allow me to manually specify
the link depth the archiver should attempt to archive, for example
this would ensure that all of the pages of a multi-page article get
archived.

