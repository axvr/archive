<title>Project timeline</title>

[← back](../)

# Project timeline

The following is a mostly complete list of all my projects (both big and small)
in reverse chronological order.  This page explains the motivation behind my
projects and provides a permanent archive for the really small ones.

The italic lines give context on hardware and tools I was using at the time.

## The age of ideas

We're just getting started.

<dl>

  <dt><code>2019</code> <a href="/projects/ascribe">Ascribe</a></dt>
  <dd><p>
  Ascribe is a standard for software development tools (specifically editors)
  to retrieve and handle information on the various files within a project
  through the <code>.gitattributes</code> file already found in many software
  projects.

  It is essentially an alternative to <a href="https://editorconfig.org/">EditorConfig</a>.
  </p></dd>


  <dt><code>2019</code> <a href="https://github.com/axvr/photon.vim">Photon.vim</a></dt>
  <dd><p>
  An elegant, dark colour scheme with minimal syntax highlighting for Vim.

  Photon is a low-contrast colour scheme, which cares about the little details.
  It is based on an old version of paramount and is designed to look exactly
  the same on 256 colour terminals as it does on true colour terminals.
  </p></dd>

</dl>

## The early days

These were the early days; I was just beginning to learn how to write code, how
stuff works and electrical engineering basics.

<dl>

  <dt><code>2019-02</code> <i>Finished apprenticeship, stayed on.</i></dt>
  <dd><p></p></dd>


  <dt><code>2018</code> <a href="https://github.com/axvr/org.vim">Org.vim</a></dt>
  <dd><p>
  Org.vim is a minimal <a href="https://orgmode.org/">Org mode</a> plugin for
  Vim. It provides only the absolute necessities (e.g. syntax highlighting and
  folding) at a high standard.

  The main goal of this plugin is to replicate
  <a href="https://github.com/tpope/vim-markdown/">Vim's default Markdown</a>
  editing experience for Org mode files.
  </p></dd>


  <dt><code>2018</code> <a href="https://github.com/axvr/txtdb">txtdb</a></dt>
  <dd><p>
  An attempt to create a simple database engine and RDBMS which stores the
  table data in text files, using the CSV format.

  This was started as a learning project to learn how databases, SQL and
  compilers.
  </p></dd>


  <dt><code>2018-01-24</code> <i>Fourth computer (Dell XPS 13 9370).</i></dt>
  <dd><p></p></dd>


  <dt><code>2017-11-13</code> <i>First job (apprentice software developer).</i></dt>
  <dd><p></p></dd>


  <dt><code>2017</code> <a href="assets/gnome-theme.sh">GNOME Theme Switcher</a></dt>
  <dd><p>
  In an attempt to automate the set up of my Fedora Linux desktop installation,
  I had wanted a way to quickly switch between different themes, toggle
  extensions, and change system settings quickly. These options were grouped
  into what I called "complete themes". The result was <a href="assets/gnome-theme.sh">extracted
  into a Bash script</a>, and has had a few minor tweaks since.

  <a href="https://www.reddit.com/r/unixporn/comments/73l9qg/gnome_script_to_switch_gnome_themes/">I
  announced it on Reddit</a>, and the response was completly unexpected (I
  honestly expected no one to care). Shortly after the announcement, I stopped
  using it as I was mostly happy with stock GNOME, and it was too much of a
  hassle to install GNOME extensions at the time.
  </p></dd>


  <dt><code>2017</code> <a href="https://github.com/axvr/vivid.vim">Vivid.vim</a></dt>
  <dd><p>
  This version of Vivid was a full rewrite of was had become "Vivid-Legacy".
  This new version was a fraction of the size and significantly faster.

  The goal of this new implementation of Vivid changed a lot during development
  time. Originally it was supposed to be a cleaner and faster version of Vundle
  and make use of the Vim's new packages feature. Later it evolved into a
  "meta-plugin manager" where it acted as a plugin manager for plugins (to
  manage dependencies). Eventually I removed that cruft (after realising it was
  a terrible idea), and finalised the last version in late 2018.

  Shortly after creating the final version, I stopped using Vivid and switched
  to using Vim's package feature in combination with Git submodules.
  </p></dd>


  <dt><code>2017</code> ParamParse</dt>
  <dd><p>
  This was an overly complex and experimental python command-line argument
  parser library. What made is particularly complex is that it returned a
  matrix, containing a lage number of items, which should have been stored in
  dictionary form at the very least. Usage of this library would have be a
  source of many bugs and pain.

  On the plus side, the concept was quite neat.

  <ul>
  <li><a href="assets/paramparse/pp.py">ParamParse library</a></li>
  <li><a href="assets/paramparse/ppexample.py">Usage example</a></li>
  </ul>
  </p></dd>


  <dt><code>2017</code> <a href="https://github.com/axvr/Vivid-Legacy.vim">Vivid-Legacy.vim</a></dt>
  <dd><p>
  This was my second programming project; I had been wanting to learn more
  about Vim and was interested in how the plugin management aspect of Vim
  worked, so I decided I wanted to try and create my own plugin manager, after
  a couple of failed attempts, I forked <a href="https://github.com/VundleVim/Vundle.vim">Vundle</a>
  (which had been stagnant since 2014) and began making minor tweaks.

  Eventually I realised that I would have to rewrite it from scratch to get it
  to become what I wanted. That rewrite inherited the name "Vivid", and this
  version was renamed to "Vivid-Legacy".
  </p></dd>


  <dt><code>2017</code> <a href="https://github.com/axvr/alis">ALIS</a> (Arch Linux Installation Script)</dt> 
  <dd><p>
  ALIS was an installer for <a href="https://www.archlinux.org/">Arch
  Linux</a>.  This was my first proper programming project. It was started
  because the installer I had previously been using
  (<a href="https://sourceforge.net/projects/architect-linux/">Architect Linux</a>)
  had recently ceased development. I took this as an opportunity to learn more
  about Linux systems and how to write code by creating my own version of the
  installer from scratch.

  The project was eventually killed by the end of the year as it had reached an
  insane level of complexity (typical of first projects), and I had already
  learnt a lot, in the process.
  </p></dd>


  <dt><code>2016-09</code> <i>Began learning Vim.</i></dt>
  <dd><p></p></dd>


  <dt><code>2016</code> <a href="assets/sqrandtrinums.py">Matching square and triangle numbers</a></dt>
  <dd><p>
  On the 13th of July 2016, Matt Parker released a video to his YouTube channel
  titled "<a href="https://www.youtube.com/watch?v=Gh8h8MJFFdI">Puzzle: Is 36
  the only triangle-square number?</a>". This video inspired me to try and
  write a small program to solve the question he posed by calculating which
  numbers are both triangle and square numbers.

  Upon reflection 3 years later, it is clear that this program was extremely
  inefficient and has made me consider writing a blog post on algorithm
  optimisation featuring this type of calculation.
  </p></dd>


  <dt><code>2015</code> <a href="/projects/teg">Thermoelectric generator</a></dt>
  <dd><p>
  While taking part in a mini survival-like competition as a teenager, I
  thought it would be interesting to see if it was possible to efficiently
  generate electricity on a tiny budget. The end result was capable of charging
  a mobile phone.

  This was my first proper hardware/electrical engineering project. It spanned
  from November 2015 to February 2016.
  </p></dd>


  <dt><code>2015</code> Rock, paper scissors</dt>
  <dd><p>
  During my first year of computer science classes we were given a series of
  tasks to complete in Python; well, lets just say that I may have got a little
  <i>too</i> competitive.

  One of these tasks was to create the classic children's game "Rock, paper,
  scissors". I ended up creating what may very well have been the smallest
  Python implementation of the game at the time: totaling just 9 lines of code
  (all of the others were in the 20 LOC range). It is left as an exercise to
  the reader to work out what alowed it to become so small (it is very hacky).

  After that I created a slightly more advanced version which handled stuff
  such as best of 3, printing the rules, etc. which was somehow still smaller
  than all of the others in the class. I had also wrote an implementation of
  "<a href="http://www.samkass.com/theories/RPSSL.html">Rock, paper, scissors, lizard, Spock</a>".

  The source code for each is linked below (they have all since been placed
  into the public domain):

  <ul>
  <li>Rock, paper, scissors (<a href="assets/rps/rps_core.py">core</a>,
  <a href="assets/rps/rps_adv.py">advanced</a>)</li>
  <li>Rock, paper, scissors, lizard, Spock (<a href="assets/rps/rpsls_core.py">core</a>,
  <a href="assets/rps/rpsls_adv.py">advanced</a>)</li>
  </ul>
  </p></dd>


  <dt><code>2015-09</code> <i>First computer science class.</i></dt>
  <dd><p></p></dd>


  <dt><code>2015-06</code> <i>Third computer (Dell Latitude D630).</i></dt>
  <dd><p></p></dd>


  <dt><code>2015-05-25</code> <i>Second computer (Raspberry Pi 2 Model B).</i></dt>
  <dd><p></p></dd>


  <dt><code>2014-05-25</code> <i>First computer (Raspberry Pi 1 Model B rev. 1.2).</i></dt>
  <dd><p></p></dd>

</dl>
