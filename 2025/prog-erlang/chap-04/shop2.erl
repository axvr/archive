-module(shop2).
-export([cost/1, total/1]).
-import(mylists, [map/2, sum/1]).

cost(oranges)   -> 5;
cost(newspaper) -> 8;
cost(apples)    -> 2;
cost(pears)     -> 9;
cost(milk)      -> 7.

total(Items) ->
    sum(map(fun ({What, N}) -> cost(What) * N end, Items)).

% How a s-expression version of the above may look:
%
%   (mod shop2
%     (:import [mylists :refer [map sum]]))
%
%   (defmatch cost
%     ([:oranges]   5)
%     ([:newspaper] 8)
%     ([:apples]    2)
%     ([:pears]     9)
%     ([:milk]      7))
%
%   (defn total [items]
%     (sum (map (fn [[what n]] (* n (cost what))) items)))
