-module(shop).
-export([cost/1, total/1]).

cost(oranges)   -> 5;
cost(newspaper) -> 8;
cost(apples)    -> 2;
cost(pears)     -> 9;
cost(milk)      -> 7.

total([{What, N} | T]) -> cost(What) * N + total(T);
total([])              -> 0.

% Equivalent Clojure code:
%
%   (defn total [[[what n] & t]]
%     (if what
%       (+ (* (cost what) n) (total t))
%       0))
%
% Hypothetical pattern matching version of defn:
%
%   (defn total
%     ([[[what n] & t]] (+ (* (cost what) n) (total t)))
%     ([] 0))
