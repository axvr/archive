% Variables begin with uppercase letters and rebinding is not
% permitted.
%
% Lower case names are atoms, i.e. symbolic constants (similar
% to Lisp symbols).

X = 123.  % Correct.
x = 123.  % Incorrect.


% Compile a module from the Erlang shell with:
c(module_name).

% e.g.
c(hello).
hello:start().
halt().

% Or from the OS CLI:
%   erlc hello.erl  # Produces a `.beam` file.
%   erl -noshell -s hello start -s init stop
%
% The `-s` args are equivalent to `hello:start().` and
% `init:stop().`

% Compilation of a module creates a `module_name.beam` file.

% Erlang has no built-in build tool?  Use Makefiles, Redo, etc.

% gitignore the `*.beam` files.

% A process is a lightweight virtual machine that communicates
% via message passing.

% Can list/interrupt/kill/etc. running jobs by typing Ctrl-G
% and `h` in Eshell.
