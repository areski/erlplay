-module(poc).
-compile([export_all]).


event_handler(Node, State) ->
    receive
	Event ->

	    R = timer:tc(erlang, spawn, [?MODULE, handle_event, [Event]]),
	    io:format("~p~n", [R]),
	    event_handler(Node, State)
    end.

handle_event(_Event) ->
    ok.

start() ->
    %% this node should be the freeswitch Node
    Node = 'freeswitch@newbalance',
    State = [],
    freeswitch:event(Node, 'CHANNEL_CREATE'),
    freeswitch:event(Node, 'CHANNEL_HANGUP_COMPLETE'),
    freeswitch:start_event_handler(Node, ?MODULE, event_handler, State).
