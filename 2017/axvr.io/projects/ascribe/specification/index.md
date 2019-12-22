<title>Ascribe specification</title>

[← back](../)

# Ascribe specification

**Version**: `v1.0`<br>
**Last modified**: `2019-05-19`

This is the Ascribe specification. If you are unsure how `.gitattributes` files
work, you can read my document on [using `.gitattributes` files](../usage).
That document also explains the various terms used in this page, e.g.
"explicitly set/unset".

Most of the time if a specific option is "unspecified", Ascribe should use the
default settings of the editor or tool, i.e. do nothing. Any exceptions to this
are mentioned below.

To reduce risks posed by arbitrary code execution, only specific characters are
allowed to be used in the value for an option. The value has to match this
([POSIX extended](https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap09.html#tag_09_04))
regular expression: `/[a-zA-Z0-9_.-]*/`.

- [`expand-tab`](#expand-tab)
- [`tab-stop`](#tab-stop)
- [`eol`](#eol)
- [`trim-trailing-whitespace`](#trim-trailing-whitespace)
- [`final-newline`](#final-newline)
- [`line-length`](#line-length)
- [`binary`](#binary)
- [`working-tree-encoding`](#working-tree-encoding)

<!-- Possible additional attributes
- File type detection.
- Trim trailing newlines from end of file.
- Spell check.
- Spell check language.
-->

## `expand-tab`

**Type**: boolean.

Controls whether presses of the tab key will be expanded into spaces. The
number of spaces a tab is expanded to is controlled by [`tab-stop`](#tab-stop).

## `tab-stop`

**Type**: integer.

The number of spaces which represent a tab character. The most common values
for this attribute are: `2`, `4` and `8`.

## `eol`

**Type**: string.

Sets the character(s) used at the end of a line.

Setting this option enables end-of-line normalisation by Git (without having to
set `text`).

**Values**: (case insensitive)

- `lf` - Unix
- `crlf` - DOS

There is no support for Mac line-endings (`cr`), this is because they are now
extremely rare and not supported by Git.

## `trim-trailing-whitespace`

**Type**: boolean.

If explicitly set, this option will automatically trim trailing whitespace.
Typically this would occur just before saving the file.

## `final-newline`

**Type**: boolean.

The [POSIX specification](https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap03.html#tag_03_206)
states that a line should always end with a newline character, including the
last line of the file. Many tools in the Unix world expect this final newline
character to be there, otherwise they may consider the file to be corrupted or
at the very least, truncated.

By explicitly setting this option, an Ascribe tool will automatically insert
the final newline character if it is missing.

This option is not enforced by Git, however running `git diff --check` will
inform you if it is missing.

## `line-length`

**Type**: integer.

This option is primarily used for line length guides. Ascribe intentionally
doesn't try to enforce it.

## `binary`

**Type**: boolean.

If explicitly set, Git will not attempt to diff changes, and Ascribe will
inform the editor that the file contains binary content.

## `working-tree-encoding`

**Type**: string.

Whenever a file is checked into Git, it will be stored using the UTF-8 file
encoding. When files are checked out of Git, they will also be encoded in UTF-8
(regardless of the encoding the file was originally using). The
`working-tree-encoding` option allows you to override the encoding used when
checking out the file.

This option is enforced by Git, however there are some limitations. According
to the [`gitattributes(5)`](https://www.git-scm.com/docs/gitattributes) manual
page, there can be pretty major issues when using Git clients which don't
support the `working-tree-encoding` option.

Because of these issues, this option hasn't yet been implemented in the
"official" Ascribe extensions. If you must use it, please read the entirety of
the appropriate section of the `gitattributes(5)` manual page, do it
manually and inform all contributors about potential issues.
