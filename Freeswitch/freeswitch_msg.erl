-module(freeswitch_msg).
-compile([export_all]).


%[get_var/2, get_var/3, sendmsg/3]

-define(FS_NODE, 'freeswitch@newbalance').

send_msg(UUID, App, Args) ->
    Headers = [{"call-command", "execute"},
        {"execute-app-name", atom_to_list(App)}, {"execute-app-arg", Args}],
        send_msg(UUID, Headers).

send_lock_msg(UUID, App, Args) ->
    Headers = [{"call-command", "execute"}, {"event-lock", "true"},
        {"execute-app-name", atom_to_list(App)}, {"execute-app-arg", Args}],
        send_msg(UUID, Headers).

send_msg(UUID, Headers) -> {sendmsg, ?FS_NODE} ! {sendmsg, UUID, Headers}.

%sendmsg(UUID, playback, "new_sales/1000.wav"),

sendmsg(UUID, App, Args) ->
    send_msg(UUID, App, Args).


%get_var("Application", Data)
%CallerID = get_var("Channel-Caller-ID-Number", Data, "00000000"),
get_var(Header, Event) ->
    %proplists:get_value(<<Header>>, Event).
    1.

get_var(Header, Event, Default) ->
    proplists:get_value(<<Header>>, Event, Default).
