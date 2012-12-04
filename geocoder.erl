-module(geocoder).
-author('Sergiy Dzysyak <dzysyak@gmail.com>').

-compile(export_all).

start()->
    ibrowse:start().

getLocation(Address)->
    ApiUrl = "http://maps.google.com/maps/api/geocode/json?sensor=false&address=",
    UrlAddress = ibrowse_lib:url_encode(Address),
    case ibrowse:send_req(ApiUrl++UrlAddress, [], get) of
    {ok, _, _, ResJson}->
        {_, Res} = mochijson2:decode(ResJson),
        case lists:keyfind(<<"status">>, 1, Res) of
        {<<"status">>, <<"OK">>} ->
            {<<"results">>, [{_, Res1}]} = lists:keyfind(<<"results">>, 1, Res),
            {<<"geometry">>, {_, Res2}} = lists:keyfind(<<"geometry">>, 1, Res1),
            {<<"location">>, {_, Res3}} = lists:keyfind(<<"location">>, 1, Res2),
            %io:format("Res: ~p~n",[Res3]);
            Res3;
        _ ->
            false
        end;
    _ ->
        false
    end.