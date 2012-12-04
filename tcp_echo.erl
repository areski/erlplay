-module(tcp_echo).
-export([listen/1]).

-define(TCP_OPTIONS, [binary, {packet, 2}, {active, false}, {reuseaddr, true}]).
-define(PORT, 8080).

% Call echo:listen() to start the server.
listen(Port) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    spawn(fun() -> accept(LSocket) end).

% Wait for incoming connections and spawn a process that will process incoming packets.
accept(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    Pid = spawn(fun() ->
        io:format("Connection accepted ~n", []),
        loop(Socket)
    end),
    gen_tcp:controlling_process(Socket, Pid),
    accept(LSocket).

% Echo back whatever data we receive on Socket.
loop(Sock) ->
    inet:setopts(Sock, [{active, once}]),
    receive
    {tcp, Socket, Data} ->
        io:format("Got packet: ~p~n", [Data]),
        gen_tcp:send(Socket, Data),
        loop(Socket);
    {tcp_closed, Socket}->
        io:format("Socket ~p closed~n", [Socket]);
    {tcp_error, Socket, Reason} ->
        io:format("Error on socket ~p reason: ~p~n", [Socket, Reason])
    end.
