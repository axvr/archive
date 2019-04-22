<title>ConstConf goals</title>

[\<-- back](../)

# ConstConf project goals & scope

## Goals

One major goal of ConstConf is to encourage the improvement of existing tools
and infrastructure rather than creating something completely new (i.e.
EditorConfig) which could have just as easily been built on top of existing
tools (please try to ignore the irony of that last statement).

ConstConf is achieving this goal by promoting the use of the `.gitattributes`
file and improving parsers for it. One example of this is the possible future
feature for file-type detection which should use the same attribute and value
names as [GitHub's linguist](https://github.com/github/linguist#overrides)
tool.

I also want to promote good design of software so all "official" ConstConf
extensions must meet extremely high code quality standards and aim to be
virtually bug free.

## Scope

Often I see many people complaining about EditorConfig not including "this" or
"that" obscure feature; mainly whether `{` (curly braces) should be on a new
line. To those people, I say no. Such features are out side the scope of the
EditorConfig and ConstConf projects, most editors don't have configuration
options for such style rules. If you want the `{` to be on a new line, press
the return key before typing the curly brace.
