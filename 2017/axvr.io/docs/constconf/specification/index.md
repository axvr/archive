<title>ConstConf spec</title>

[\<-- back](../)

# ConstConf specification

## Options

| EditorConfig               | ConstConf                  | Implemented |
|----------------------------|----------------------------|-------------|
| `indent_style`             | `expand-tab`               | Yes         |
| `indent_size`              | `tab-stop`                 | Yes         |
| `tab_width`                | N/A                        | N/A         |
| `end_of_line`              | `eol`                      | Yes         |
| `charset`                  | `working-tree-encoding`    | No          |
| `trim_trailing_whitespace` | `trim-trailing-whitespace` | Yes         |
| `insert_final_newline`     | `final-newline`            | Yes         |
| `root`                     | N/A                        | N/A         |
| `max_line_length`          | `line-length`              | Yes         |

### Boolean

- explicit unset
- explicit set
- unspecified

### Value

- explicit set to value
- unspecified

## Possible extensions

- File type detection.
- Trim trailing newlines from end of file.
- Spell check.
- Spell check language.

## More information

- [`gitattributes(5)`](https://www.git-scm.com/docs/gitattributes)
- [`git-check-attr(1)`](https://www.git-scm.com/docs/git-check-attr) - The official `.gitattributes` parser.
