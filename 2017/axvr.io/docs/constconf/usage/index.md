<title>Using .gitattributes</title>

[\<-- back](../)

# Using `.gitattributes` files

_WIP_

## Example

This is the `.gitattributes` file for the ConstConf Vim extension.

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
