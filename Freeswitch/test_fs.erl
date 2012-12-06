-module(test_fs).
-compile([export_all]).

% Source http://www.freeswitch.org.cn/2011/11/10/shi-yong-erlang-kong-zhi-hu-jiao-liu-cheng.html


% Configure on FS
% <param name="encoding" value="binary"/>


% <extension name="test_erlang">
%   <condition field="destination_number" expression="^9997$">
%     <action application="erlang" data="ivr:! mynode@newbalance"/>
%   </condition>
% </extension>


-define(LOG(Message, Vars), io:format(Message, Vars)).
-define(FS_NODE, 'freeswitch@newbalance').
-define(WELCOME_SOUND, "/usr/local/freeswitch/sounds/en/us/callie/voicemail/8000/vm-tutorial_change_pin.wav").
-define(INPUT_NUMBER_SOUND, "/usr/local/freeswitch/sounds/en/us/callie/voicemail/8000/vm-press.wav").
%-define(WELCOME_SOUND, "tone_stream://%(100,1000,800);loops=1").
%-define(INPUT_NUMBER_SOUND, "tone_stream://%(100,1000,800);loops=2").


start() ->
    register (ivr, self()),
    loop ().

loop () ->
    ?LOG ("yooop  message~n", []),
    ?LOG ("yooop  message2~n", []),
    receive
        {Get_pid, UUID, Ref, FSPid} ->
            NewPid = spawn (fun () -> serve () end),
            FSPid! {Ref, NewPid},
            io:format ("Main process ~p spawned new child process ~p ~n", [self (), NewPid]),
            loop();
        _X ->
            io:format("oupsss~n"),
            loop()
    end.



send_msg(UUID, App, Args) ->
    Headers = [{"call-command", "execute"},
        {"execute-app-name", atom_to_list(App)}, {"execute-app-arg", Args}],
        send_msg(UUID, Headers).

send_lock_msg(UUID, App, Args) ->
    Headers = [{"call-command", "execute"}, {"event-lock", "true"},
        {"execute-app-name", atom_to_list(App)}, {"execute-app-arg", Args}],
        send_msg(UUID, Headers).

send_msg(UUID, Headers) -> {sendmsg, ?FS_NODE} ! {sendmsg, UUID, Headers}.


serve() ->
    receive
        {call, {event, [UUID | Event]} } ->
            ?LOG("New call ~p~n", [UUID]),
            send_msg(UUID, set, "hangup_after_bridge=true"),
            send_lock_msg(UUID, playback, ?WELCOME_SOUND),
            send_msg(UUID, read, "1 4 " ?INPUT_NUMBER_SOUND " erl_dst_number 5000 #"),
            serve();
        {call_event, {event, [UUID | Event]} } ->
            Name = proplists:get_value(<<"Event-Name">>, Event),
            App = proplists:get_value(<<"Application">>, Event),
            ?LOG("Event: ~p, App: ~p~n", [Name, App]),
            case Name of
                <<"CHANNEL_EXECUTE_COMPLETE">> when App =:= <<"read">> ->
                    case proplists:get_value(<<"variable_erl_dst_number">>, Event) of
                        undefined ->
                            send_msg(UUID, read, "1 4 " ?INPUT_NUMBER_SOUND " erl_dst_number 5000 #");
                        Dest ->
                            send_msg(UUID, bridge, "user/" ++ binary_to_list(Dest))
                    end;
                _ -> ok
            end,
            serve();
        call_hangup ->
            ?LOG("Call hangup~n", []);
        _X ->
            ?LOG("ignoring message ~p~n", [_X]),
            serve()
    end.