<title>Ascribe other VCS</title>

[\<-- back](../)

# Ascribe support for other VCS

Eventually it would be nice to add support for additional version control tools
(especially Mercurial). Unfortunately Ascribe will only focus on Git support at
this point in time.

### Make it work on any file (hacky solution)

We can temporarily emulate a git repo by creating the base dir and file
structure. In there we can place a `.git/info/attributes` symbolic link to the
located attributes file.
