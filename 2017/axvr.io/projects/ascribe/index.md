<title>Ascribe</title>

# Ascribe

### An alternative to [EditorConfig](https://editorconfig.org/).

The idea behind EditorConfig is an excellent one. It provides a mechanism to
automate the setting of core editor configuration options, for a huge number of
editors and other development tools on a per-project basis.

EditorConfig is undoubtedly a very useful tool, but I think I can do one
better.

## What actually is Ascribe?

Ascribe is a standard for software development tools (specifically editors) to
retrieve and handle information on the various files within a project through
the `.gitattributes` file already found in many software projects.

While EditorConfig does mostly achieve the same goal, it is solely focused on
editors, therefore it doesn't (_easily_) allow anyone to add additional options for
other types of development tools.

Ascribe is designed to allow software developers to keep all information about
the files in their projects in one place, and encourages the improvement of the
existing (and arguably, much more useful) `.gitattributes` parsers.

## Want to learn more?

**Note**: _These pages are still a work in progress_.

- [Comparison between EditorConfig and Ascribe](comparison).
- [Ascribe specification](specification).
- [Using `.gitattributes` files](usage).
- [Project goals & scope](goals).

## Official extensions

- [Vim](https://git.sr.ht/~axvr/ascribe.vim) (_WIP_)
- GNU Emacs (_WIP_)
- Git CLI (_WIP_ - uses Git hooks)
