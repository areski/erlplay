-module(qsort).
-export([qsort1/1, qsort2/1, qsort3/1]).

%Using list comprehensions
qsort1([]) ->
    [];
qsort1([H | T]) ->
    qsort1([ X || X <- T, X < H ]) ++ [H] ++ qsort1([ X || X <- T, X >= H ]).


%Using a partitioning function
qsort2([]) ->
    [];
qsort2([H | T]) ->
    {Less, Equal, Greater} = part(H, T, {[], [H], []}),
    qsort2(Less) ++ Equal ++ qsort2(Greater).

part(_, [], {L, E, G}) ->
    {L, E, G};
part(X, [H | T], {L, E, G}) ->
    if
        H < X ->
            part(X, T, {[H | L], E, G});
        H > X ->
            part(X, T, {L, E, [H | G]});
        true ->
            part(X, T, {L, [H | E], G})
    end.

qsort3([]) -> [];
qsort3([H | T]) -> qsort3_acc([H | T], []).
qsort3_acc([], Acc) -> Acc;
qsort3_acc([H | T], Acc) -> part_acc(H, T, {[], [H], []}, Acc).

part_acc(_, [], {L, E, G}, Acc) ->
    qsort3_acc(L, (E ++ qsort3_acc(G, Acc)));
part_acc(X, [H | T], {L, E, G}, Acc) ->
    if
        H < X -> part_acc(X, T, {[H | L], E, G}, Acc);
        H > X -> part_acc(X, T, {L, E, [H | G]}, Acc);
        true -> part_acc(X, T, {L, [H | E], G}, Acc)
    end.

%%only keep even number
even(L) -> lists:reverse(even(L, [])).

even([], Acc) -> Acc;
even([H|T], Acc) when H rem 2 == 0 ->
    even(T, [H|Acc]);
even([_|T], Acc) ->
    even(T, [Acc]).

%% only keep men older than 60
old_men(L) -> lists:reverse(old_men(L,[])).

old_men([], Acc) -> Acc.
old_men([Person = {male, Age}|People], Acc) when Age > 60 ->
    old_men(People, [Person, Acc]);
old_men([_|People], Acc) ->
    old_men(People, Acc).

