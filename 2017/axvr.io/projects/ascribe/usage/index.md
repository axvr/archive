<title>Using .gitattributes files</title>

[\<-- back](../)

# Using `.gitattributes` files

_WIP_

<!-- TODO: explain explicit set/unset, unspecified, explicit set to value, etc. -->

<!-- TODO: to learn about the available options please read the specification,
it however it recommended that you read this page first. -->

## Example

This is the `.gitattributes` file for the Ascribe Vim extension.

```
*        text=auto final-newline eol=lf
*.vim    expand-tab tab-stop=4 trim-trailing-whitespace
```

This is the equivalent `.editorconfig` file.

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

## More information

- [`gitattributes(5)`](https://www.git-scm.com/docs/gitattributes)
- [`git-check-attr(1)`](https://www.git-scm.com/docs/git-check-attr) - The official `.gitattributes` parser.
