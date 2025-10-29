-module(geometry).
-export([area/1, test/0]).

test() ->
    12  = area({rectangle, 3, 4}),
    144 = area({square, 12}),
    tests_passed.

area({rectangle, Width, Height}) -> Width * Height;
area({circle, Radius})           -> 3.14159 * Radius * Radius;
area({square, Side})             -> Side * Side.
% ^ Intentionally doesn't handle no clause match.

% - `,` separate arguments in function calls, data constructors
%       and patterns.  (Short range.)
% - `;` separate clauses.  E.g. function definitions, case, if,
%       try...catch and receive expressions.  (Medium range.)
% - `.` (followed by whitespace) separates entire functions and
%       expressions.  (Long range.)
