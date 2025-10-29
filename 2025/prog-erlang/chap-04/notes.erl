-module(notes).

Rectangle = {rectangle, 10, 5}.
{rectangle, Width, Height} = Rectangle.
Width.
Height.

Square = {square, 3}.
{square, Side} = Square.
Side.

geometry:area(Rectangle).
geometry:area(Square).


% If accidentally created a module with the same name as
% a system module, rename the module and delete any `.beam`
% files for the old name.

% Can use these in the Erlang shell:
pwd().
ls().
cd("..").

% Anonymous functions.

Double = fun(X) -> X * 2 end.
10 = Double(5).

Hypot = fun(X, Y) -> math:sqrt(X * X + Y * Y) end.
5.0 = Hypot(3, 4).

TempConvert = fun({c, C}) -> {f, 32 + C * 9 / 5};
                 ({f, F}) -> {c, (F - 32) * 5 / 9}
              end.

{f, 212.0} = TempConvert({c, 100}).
{c, 100.0} = TempConvert({f, 212}).
{f, 32.0}  = TempConvert({c, 0}).

L = [1, 2, 3, 4, 5].
lists:map(fun(X) -> X * 2 end, L).

Even = fun(X) -> (X rem 2) =:= 0 end.
% Use =:= to test for equivalence.

lists:map(Even, L).
lists:filter(Even, L).

% Collection functions in Erlang are sometimes referred to as
% "list-at-a-time" functions.

lists:member(3, L).

Fruit = [apple, pear, orange].
MakeTest = fun(L) -> (fun(X) -> lists:member(X, L) end) end.
% We wrap the returning function in brackets to include the
% `end` for the `fun`.
IsFruit = MakeTest(Fruit).
lists:filter(IsFruit, [dog, orange, cat, apple, bear]).

% Implementing a rudimentary for loop in Erlang.
for(Max, Max, F) -> [F(Max)];
for(I, Max, F)   -> [F(I) | for(I + 1, Max, F)].

for(1, 10, fun(I) -> I end).
for(1, 10, fun(I) -> I * I end).
