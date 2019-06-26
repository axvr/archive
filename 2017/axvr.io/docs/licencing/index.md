<title>Licencing</title>

# Licencing
 
This page details how I licence my projects and creative works (this only
applies to new projects, forks should maintain the original licence).

## Selecting a licence

### <u>Software</u>

For small software projects any permissive licence is fine. The preferred ones
are:

- The X11/MIT licence,
- The ISC licence (or the OpenBSD variant),
- Any of the BSD licences,
- The Apache 2.0 licence (_cannot be included in OpenBSD_).

Alternatively, you can dedicate the source code to the public domain using the
Creative Commons Zero 1.0 (CC0) public domain dedication.

For medium to large sized software projects, use the Mozilla Public Licence 2.0
(MPL).

Any configuration files (especially default ones which get copied to the user's
home directories) must be dedicated to the public domain.

Trivial code, test cases (e.g. unit and integration tests), build steps and
code snippets for documentation should be dedicated to the public domain.

The only exception is when the choice of licence would enable better
integration with the rest of the ecosystem. For example, using the
<abbr title="GNU General Public Licence v3.0">GNU GPLv3</abbr> and the Artistic
Licence 2.0 for GNU Emacs and Perl packages respectively.

### <u>Documentation and creative works</u>

Documentation should be dedicated to the public domain using the CC0.

### <u>Fonts</u>

Custom fonts should use the SIL Open Font License (OFL).

## Licence headers and copyright notices

### <u>CC0 1.0 Universal public domain dedication</u>

For more information on the CC0 visit: <https://creativecommons.org/publicdomain/zero/1.0/>

A full copy of the CC0 text should be included in the project source under the
file name `COPYING`.

For full source files use this file header:

```
/*
 * Written in <year> by <authors>
 *
 * To the extent possible under law, the author(s) have dedicated all
 * copyright and related and neighboring rights to this software to
 * the public domain worldwide. This software is distributed without
 * any warranty.
 *
 * You should have received a copy of the CC0 Public Domain Dedication
 * along with this software. If not, see
 * <http://creativecommons.org/publicdomain/zero/1.0/>.
 */
```

For small code snippets and <abbr title="Hypertext Markup Language">HTML</abbr>
and <abbr title="Cascading Style Sheets">CSS</abbr> files, this header will
suffice:

```
/* Any copyright is dedicated to the Public Domain.
 * https://creativecommons.org/publicdomain/zero/1.0/ */
```

Documentation files may alternatively use this header text in the footer of the
file to make the file more readable.

View the [full CC0 text](assets/CC0.txt).


### <u>Mozilla Public Licence 2.0</u>

For more information on the MPL visit: <https://www.mozilla.org/MPL/>

A full copy of the MPL 2.0 text should be included in your project source under
the file name `LICENCE`.

When using the MPL copyright headers in files are important because the MPL
operates on a per-file basis. If you are unable to place a copyright header on
a particular file you must place a copy of the `LICENCE` file in the directory
containing that source file.

```
/*
 * Copyright (c) <year> <copyright holders>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
```

View the [full MPL text](assets/MPL.txt).

## Patents

Most permissive software licences don't make any mention of patents, and the
CC0 public domain dedication, explicitly states that it doesn't affect any
patents that I may have.

> No trademark or patent rights held by Affirmer are waived, abandoned,
> surrendered, licensed or otherwise affected by this document.

My stance on patents is the following: I dislike patents and don't believe in
intellectual property, as such I will never hold a patent on any of my works.
