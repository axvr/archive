-module(mylists).
-export([sum/1, map/2]).

sum([])      -> 0;
sum([H | T]) -> H + sum(T).

% Pretty straightforward, like writing the same in a Scheme.
map(_, [])      -> [];
map(F, [H | T]) -> [F(H) | map(F, T)].
% Use `_` if don't care about variable value.
