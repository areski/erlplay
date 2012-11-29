-module(useless).
-export([add/2]).
-export([hello/0]).
-export([greet_and_add_two/1]).

add(A,B) ->
    A + B.

%% Show Greetings
%% io:format/1 is the standard function used to output text
hello() ->
    io:format("Hello, world!~n").

greet_and_add_two(X) ->
    hello(),
    add(X, 2).

