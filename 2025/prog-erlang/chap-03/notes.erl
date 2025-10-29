% From the Erlang shell, Ctrl-C + `a` will abort Erlang.
% However this could result in data loss, so prefer this
% instead:
q().  % or `init:stop().`

% Controlled shutdown.  Flushes and closes files, databases are
% stopped, and all applications are stopped in an ordered
% manner.

% This is similar to calling `shutdown-agents` in Clojure before
% exiting.

% Atoms prefixed with a hyphen are "annotations".  They cannot
% be entered into the shell.

% The Tab key in the Erlang shell will do tab completion of
% module and function names as well as displaying some function
% signature information.  It can even be used to list the
% exported functions of a module.

help().  % List internal shell commands.
b().     % List bound vairables.
f().     % Forget all bindings.
f(X).    % Forget specific binding.
% and more...


% == Arithmetic ==
%
% Erlang follows normal arithmetic rules.  Brackets force
% precedence.

2 + 3 * 4.    % 14
(2 + 3) * 4.  % 20

% Integer arithmetic is exact.  Floating point is not.  (64-bit
% IEEE-754 strikes again).

% Integer bases are selected with the radix followed by a `#` then the number.  E.g.

16#FF.  % 255

% No fractional types.

1/2.
1/3.

% Division to integer and remainder (modulo).
5 div 3.  % 1
5 rem 3.  % 2


% == Variables/bindings ==
%
% Single assignment/binding.

X = 123.
X.        % 123
X = 456.  % Error

% Must begin with upper case character.

% Lexically scoped.

% `=` is not assignment, it is unification through pattern matching like in Prolog.
% Similar to logic programming.  Setting a constraint on the
% value of `X`.  Makes sense considering Erlang's Prolog
% history.

X = 123.
123 = X.
X = 123.

% This makes destructuring possible.


% == Atoms ==
%
% Atoms are "symbolic constants".  Like keywords in Clojure or
% Common Lisp, or quoted symbols in all Lisps.
%
% Atoms begin with lowercase letters and can contain
% alphanumeric characters, underscore and @.
%
% Atoms can be quoted with ' characters.  E.g.

'Hello'.
'+'.
'Wow, spaces in an atom.'.
'This is not a string.'.

% == Tuples ==

% Tuples are inside of `{` and `}`.

P = {10, 45}.

% Tuples are not typed, it is recommended to include an atom in
% the first slot to say what it represents.
P = {point, 10, 45}.

% Arbitrarily nested.

Person = {person,
          {name, joe},
          {height, 1.82},
          {footsize, 42},
          {eyecolour, brown}}.

% Destructuring.

Point = {point, 10, 45}.
{point, X, Y} = Point.

{not_point, X, Y} = Point.  % Error, `not_point` doesn't match.

% Use `_` for destructuring when you don't care about the value.
% The "anonymous variable".

{point, _, Y} = Point.

% Destructuring can be nested.


% == Lists ==

% Lists are like tuples except use `[` and `]`.

Drawing = [{square, {10, 10}, 10},
           {triangle, {15, 10}, {25, 10}, {30, 40}}].

% Tuples are fixed size collections.  Lists are arbitrary size
% collections.

% Lists are constructed from cons cells and therefore have
% a head and a tail which can be destructured on like in Lisp
% `car` and `cdr`.

[H | T] = Drawing.
H = {square, {10, 10}, 10}.
T = [{triangle, {15, 10}, {25, 10}, {30, 40}}].
[].  % the empty list.  Tail on the empty list is the empty list.

% Cons-ing lists is possible with the same [H|T] syntax.

ThingsToBuy = [{apples, 10}, {pears, 6}, {milk, 3}].
ThingsToBuy2 = [{oranges, 4}, {newspaper, 1} | ThingsToBuy].

[A, B, C | T] = [1, 2, 3].


% == Strings ==
%
% Technically no strings in Erlang.  List of integers or as
% a binary.  When a string is a list of integers, each element
% is a Unicode code point.

"Hello".

% If a list of integers is only printable characters, it will
% display it as a string.

[83, 117, 114, 112, 114, 105, 115, 101].  % "Surprise"

% Use `$` to get the integer for a specific character.

$a.  % 97

I = $s.
[I - 32, $u, $r, $p, $r, $i, I, $e].

% In the above shift `I` by 32 to convert to upper case.

% Use `\x{hex}` to place Unicode characters into strings using
% their hex codes.

G = "a\x{221e}b".  % [97, 8734, 98]
io:format("~ts~n", [G]).  % prints "a∞b"

[97, 98, 99].  % "abc"
io:format("~w~n", ["abc"]).  % prints "[97, 98, 99]"
