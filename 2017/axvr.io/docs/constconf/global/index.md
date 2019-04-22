<title>ConstConf global config</title>

[\<-- back](../)

# ConstConf global configuration

Since ConstConf uses the Git CLI tool (the example implementation does anyway),
it is technically possible to have a global file containing ConstConf
configuration options for many file types which will (eventually) be portable
between editors.

I however, recommend against doing this for several reasons, the first of which
is that the options will only work for files in a Git repository. The second
reason is that you will probably forget to copy those settings into a
project-local `.gitattributes` file. The third reason is that the ConstConf is
not well supported (well, it isn't yet anyway).

## Example

```
# Cross-editor file type configuration

*           text=auto final-newline trim-trailing-whitespace

*.py        expand-tab tab-stop=4 line-length=79
*.rb        expand-tab tab-stop=2
*.pl        expand-tab tab-stop=4 line-length=79
*.pm        expand-tab tab-stop=4 line-length=79

*.hs        expand-tab tab-stop=4 line-length=79

# Markup
*.md        expand-tab tab-stop=2
*.mkd       expand-tab tab-stop=2
*.markdown  expand-tab tab-stop=2
*.tex       expand-tab tab-stop=2

# Web
*.html      expand-tab tab-stop=2
*.css       expand-tab tab-stop=2
*.js        expand-tab tab-stop=4 line-length=79
*.ts        expand-tab tab-stop=4 line-length=79

# C & C++
*.c         -expand-tab
*.h         -expand-tab
*.cc        -expand-tab
*.cpp       -expand-tab
Makefile    -expand-tab

# .NET Core
*.cs        expand-tab tab-stop=4
*.csproj    expand-tab tab-stop=2 eol=crlf
*.sln       -expand-tab eol=crlf -final-newline -trim-tailing-whitespace
```
