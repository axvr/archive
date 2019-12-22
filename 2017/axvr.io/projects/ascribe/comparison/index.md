<title>Ascribe vs EditorConfig</title>

[← back](../)

# Comparison between EditorConfig and Ascribe

Both Ascribe and EditorConfig have clear advantages and disadvantages. I have
created this page to provide an overview and basic comparison between the two
tools, to enable you to make an informed decision about which one to use.

## A quick overview of the two tools

### [EditorConfig](https://editorconfig.org/)

EditorConfig is a standard which defines a file (`.editorconfig`), and a format
for that file. The standard also defines several options which that file can
contain, each of these options can change the behaviour of the developer's
editor when editing specific files.

In order for the behaviour of an editor to be altered, an extension needs to be
installed to that specific editor (some editors support the EditorConfig
standard out-of-the-box).

The EditorConfig project develops multiple parsers for their custom file
format, written in a range of languages; intended to be used within the editor
extensions they create.

EditorConfig does not depend on any version control tools.

### [Ascribe](../)

The Ascribe standard reuses the `.gitattributes` file commonly found in many
projects. It defines how to use specific attributes in the `.gitattributes` to
alter the behaviour of editors.

Ascribe makes use of the existing attributes commonly used by Git (e.g. `eol`
and `binary`), and adds some editor specific ones on top. These attributes are
used by Ascribe extensions which can be installed to the editor.

Since Ascribe uses the `.gitattributes` file, and reuses some of the existing
attributes, several important options are enforced at the VCS level by Git.
This reduces problems for developers who don't use a supported editor.

Ascribe depends on the [Git version control system](https://git-scm.com/).

## Head to head comparison

### EditorConfig

<u>Benefits</u>

- Well supported, many extensions exist and it is very popular.
- `.editorconfig` files are easier to understand than `.gitattributes` files.

<u>Problems</u>

- Requires the creation of large, complex and unreliable/bug-prone extensions.
- File encoding rules don't actually work when used with Git (Git will still
  store and checkout the file in UTF-8 unless you use the `.gitattributes` file).
- Some implementations suffer from security issues caused by allowing arbitrary
  code execution (e.g. issues [#31][] & [#33][] in an [unofficial Vim extension][]).

[unofficial Vim extension]: https://github.com/sgur/vim-editorconfig/
[#31]: https://github.com/sgur/vim-editorconfig/issues/31
[#33]: https://github.com/sgur/vim-editorconfig/issues/33

### Ascribe

<u>Benefits</u>

- Ascribe extensions are significantly simpler and easier to write than
  EditorConfig extensions.
- Some options are enforced at the VCS level.
- Keeps all information on project files in a centralised location, many other
  tools use the `.gitattributes` file to get information (for example
  [GitHub](https://github.com/)'s file type detection can be overridden using
  the [`linguist-language` attribute][]), this makes `.gitattributes` parsers
  much more useful than their `.editorconfig` counterparts.

[`linguist-language` attribute]: https://github.com/github/linguist#using-gitattributes

<u>Problems</u>

- The `.gitattributes` file format is difficult to understand.
- Not many editor extensions available at the moment.
- Depends on Git.
