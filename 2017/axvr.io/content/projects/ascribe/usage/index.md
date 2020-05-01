<!-- METADATA
title: Using .gitattributes files
-->

[← back](../)

# Using `.gitattributes` files

This document will teach you how to use read and use `.gitattributes` files.
To learn about the attributes Ascribe uses, read the
[specification](../specification), however it is greatly recommended that you
read this page first.

## Example

This is the `.gitattributes` file for the Ascribe Vim extension.

```
*        text=auto final-newline eol=lf
*.vim    expand-tab tab-stop=4 trim-trailing-whitespace
```

This file defines 2 [rules](#rules), one for all files, and the other for files
with the `.vim` extension.

<details>
<summary>Click to view the equivalent <code>.editorconfig</code> file.</summary>

```
root = true

[*]
end_of_line = lf
insert_final_newline = true

[*.vim]
indent_style = space
indent_size = 4
trim_trailing_whitespace = true
```

</details>

## Rules

Each line of a `.gitattributes` file is a separate rule.  It begins with
a [file pattern](#file-pattern), and is followed by a [list of
attributes](#attribute-list) (whitespace is insignificant).

```
 *.vim    expand-tab tab-stop=4 trim-trailing-whitespace
|_____|  |______________________________________________|
   |                           |
File pattern             Attribute list
```

A file can be matched by multiple rules.  In this example a file named
`something.vim` will match both the `*` and `*.vim` rules, the resulting
attribute list will be a combination of the two rules with the lower lines
taking precedence over the upper lines.

## File pattern

The file patterns are the same as those used in
[`.gitignore`](https://www.git-scm.com/docs/gitignore#_pattern_format) files,
but without the negation pattern (denoted by a literal `!` at the start of the
line).

## Attribute list

When an attribute is not in the resulting attribute list, it is "unspecified".

If an attribute is mentioned in the `.gitattributes` file, it has been
"explicitly specified".

Attributes which appear in the resulting attribute list but are not in the
`.gitattibutes` file are "implicitly specified".  Sometimes explicitly
specifying an attribute will cause another to be implicitly specified.  For
example explicitly setting the `binary` attribute will implicitly unset the
`diff` attribute.

Specified attributes can have 3 states.

- Set (boolean true)
- Unset (boolean false)
- Set to value

These states are represented in `.gitattributes` files as the following.

```
*.py   expand-tab   tab-stop=4   -binary
      |__________| |__________| |_______|
            |            |          |
     Explicitly set      |   Explicitly unset
             Explicitly set  to value
```

To explicitly set an attribute, just specify its name.  To unset it, prepend it
with a hyphen (`-`).  To set to a value append an equals (`=`) and the value.

## More information

- [`gitattributes(5)`](https://www.git-scm.com/docs/gitattributes)
- [`git-check-attr(1)`](https://www.git-scm.com/docs/git-check-attr) — The official `.gitattributes` parser.
