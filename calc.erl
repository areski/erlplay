-module(calc).
-export([rpn/1, rpn_test/0]).

% string:tokens("10 5 +")
% ["10", "5", "+"]

%% rpn(List()) -> Int() | Float()
%% parses an RPN string and outputs the results.
rpn(L) when is_list(L) ->
    [Res] = lists:foldl(fun rpn/2, [], string:tokens(L, " ")),
    Res.

rpn("-", [N1,N2|S]) -> [N2-N1|S];
rpn("+", [N1,N2|S]) -> [N2+N1|S];
rpn(X, Stack) -> [read(X)|Stack].

%% read(String()) -> Int() | Float()
read(N) ->
    case string:to_float(N) of
        {error,no_float} -> list_to_integer(N);
        {F,_} -> F
    end.


rpn_test() ->
    5 = rpn("2 3 +"),
    4 = rpn("2 3 + 1 -").

