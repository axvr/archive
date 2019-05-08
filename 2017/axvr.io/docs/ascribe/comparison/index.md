<title>Ascribe vs EditorConfig</title>

[\<-- back](../)

# Comparison between EditorConfig and Ascribe

<!--
Many of Ascribe's options are enforced at the VCS level, so there is less to
worry about if someone is not using an Ascribe extension.

`.editorconfig` parsers are not as useful as `.gitattributes` parsers.
-->

| EditorConfig               | Ascribe                    |
|----------------------------|----------------------------|
| `indent_style`             | `expand-tab`               |
| `indent_size`              | `tab-stop`                 |
| `tab_width`                | N/A                        |
| `end_of_line`              | `eol`                      |
| `charset`                  | `working-tree-encoding`    |
| `trim_trailing_whitespace` | `trim-trailing-whitespace` |
| `insert_final_newline`     | `final-newline`            |
| `root`                     | N/A                        |
| `max_line_length`          | `line-length`              |

### Benefits of EditorConfig

- Extensions exist for many editors and other development tools.
- `.editorconfig` files are very readable.
- File pattern rules are similar to those used in `.gitignore` files.

### Problems with EditorConfig

- Another file in your repo and another file format to learn.
- If an extension isn't being used or one doesn't exist for a specific editor,
  the settings are not enforced.
- Difficult to add new options.
- File encoding rules don't actually work when used with Git (Git will still
  store and checkout the file in UTF-8).
- Some implementations of EditorConfig suffer from security issues; arbitrary
  code execution. For example these issues were reported about an
  [unofficial EditorConfig Vim extension](https://github.com/sgur/vim-editorconfig):
  [#31](https://github.com/sgur/vim-editorconfig/issues/33) and
  [#33](https://github.com/sgur/vim-editorconfig/issues/31).

### Benefits of Ascribe

- Some settings are enforced; even if an extension doesn't yet exist (enforced
  through Git, since it uses the `.gitattributes` file).
- Ascribe extensions are significantly simpler, quicker and easier to write
  than EditorConfig extensions.
- You probably already have `.gitattributes` file your repository.
- File pattern rules are the same as those used in `.gitignore` files.
- Extremely easy to add new options (it literally only takes a few seconds).
- Safer than EditorConfig and modelines, by preventing arbitrary code
  execution.
- File encoding rules partially work (issues with old Git versions and
  alternative Git implementations, it can also slow down some Git operations).
- Other tools use the `.gitattributes` file to get their information, so you
  will just be setting the options once and in the same place (e.g. you can
  [override GitHub's file type detection](https://github.com/github/linguist#overrides)).
- If a `.gitattributes` parser needs to be written for a Ascribe extension,
  it'll be much more useful than an EditorConfig parser.

### Problems with Ascribe

- The `.gitattribute` file format is difficult to understand at first.
- Only works with Git repositories (however a hacky solution is possible to
  make it work on non-git repos without writing a `.gitattributes` parser).
- Not many editor extensions for it (there is only the sample Git extension at
  the time of writing).

---

## The biggest problem with EditorConfig

Possibly the biggest problem with EditorConfig is the size and complexity of
the extensions which allow it to actually work. As an example I'll be comparing
the [official Vim EditorConfig extension](https://github.com/editorconfig/editorconfig-vim)
against the [Ascribe equivalent](https://git.sr.ht/~axvr/constconf.vim/).

The EditorConfig extension contains ~2500 LOC (including the
[parser](https://github.com/editorconfig/editorconfig-core-py)), and the
Ascribe extension contains ~100 LOC (at the time of writing), both have the
exact same functionality. To reitterate upon that; the **exact same**
functionality but requiring just 4% of the code.
