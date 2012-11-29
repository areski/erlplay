-module(functions).
-compile(export_all). %% replace with -export()


head([H|_]) -> H.
second([_,X|_]) -> X.
same(X,X) ->
true;
same(_,_) ->
false.

valid_time({Date = {Y,M,D}, Time = {H,Min,S}}) ->
    io:format("The Date tuple (~p) says today is: ~p/~p/~p,~n",[Date,Y,M,D]),
    io:format("The time tuple (~p) indicates: ~p:~p:~p.~n", [Time,H,Min,S]);
valid_time(_) ->
    io:format("Stop feeding me wrong data!~n").
%%functions:valid_time({{2011,09,06},{09,04,43}}).

help_me(Animal) ->
    Talk = if Animal == cat -> "meow";
              Animal == dog -> "bark";
              true -> "roughhh"
           end,
    {Animal, "says " ++ Talk ++ "!"}.


wrong_age(X) when X < 16; X > 104 ->
    true.

%Factoriel
fac(N) when N == 0 -> 1;
fac(N) when N > 0 -> N*fac(N-1).
% fac(4) 4 * fac(3)

%Tail recursion is a way to transform the above linear process
%(it grows as much as there are elements) to an iterative one
%(there is not really any growth)
tail_fac(N) -> tail_fac(N, 1).

tail_fac(0, Acc) -> Acc;
tail_fac(N, Acc) when N > 0 -> tail_fac(N-1, N*Acc).


tail_len(L) -> tail_len(L, 1).
%
tail_len([], Acc) -> Acc;
tail_len([_|T], Acc) -> tail_len(T, Acc + 1).

%Another function to implement could be sublist/2, which takes
%a list L and an integer N, and returns the N first
sublist(_,0) -> [];
sublist([],_) -> [];
sublist([H|T], N) when N > 0 -> [H, sublist(T, N-1)].


reverse([]) -> [];
reverse([H|T]) -> reverse(T)++[H].

tail_reverse(L) -> tail_reverse(L,[]).
tail_reverse([],Acc) -> Acc;
tail_reverse([H|T],Acc) -> tail_reverse(T, [H|Acc]).

tail_sublist(L, N) -> tail_sublist(L, N, []).
%
tail_sublist(_, 0, Sublist) -> Sublist;
tail_sublist([], _, Sublist) -> Sublist;
tail_sublist([H|T], N, Sublist) when N>0 ->
    reverse(tail_sublist(T, N-1, [H|Sublist])).

%1> recursive:zip([a,b,c],[1,2,3]).
%[{a,1},{b,2},{c,3}]

zip([], []) -> [];
zip([X|Xs], [Y|Ys]) -> [{X,Y}| zip(Xs, Ys)].


tail_zip(X, Y) -> reverse(tail_zip(X, Y, [])).
%
tail_zip([],[],Acc) -> Acc;
tail_zip([X|Xs],[Y|Ys], Acc) ->
    tail_zip(Xs,Ys, [{X,Y}|Acc]).

quicksort([]) -> [];
quicksort([Pivot|Rest]) ->
    {Smaller, Larger} = partition(Pivot,Rest,[],[]),
    quicksort(Smaller) ++ [Pivot] ++ quicksort(Larger).

partition(_, [], Smaller, Larger) -> {Smaller, Larger};
partition(Pivot, [H|T], Smaller, Larger) ->
    if H =< Pivot -> partition(Pivot, T, [H|Smaller], Larger);
        H > Pivot -> partition(Pivot, T, Smaller, [H|Larger])
    end.

lc_quicksort([]) -> [];
lc_quicksort([Pivot|Rest]) ->
    lc_quicksort([Smaller || Smaller <- Rest, Smaller =< Pivot])
    ++ [Pivot] ++
    lc_quicksort([Larger || Larger <- Rest, Larger > Pivot]).
