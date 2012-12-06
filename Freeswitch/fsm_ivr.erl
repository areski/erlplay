-module(fsm_ivr).
-behaviour(gen_fsfsm).

-export([start/1, init/1, handle_info/3, handle_event/3, terminate/3]).
-export([welcome/2, wait_lang/2, wait_number/2, wait_hangup/2]).

-define(FS_NODE, 'freeswitch@newbalance').

-define(WELCOME_SOUND, "/usr/local/freeswitch/sounds/en/us/callie/voicemail/8000/vm-tutorial_change_pin.wav").
-define(INPUT_NUMBER_SOUND, "/usr/local/freeswitch/sounds/en/us/callie/voicemail/8000/vm-enter_new_pin.wav").
-define(SELECT_LANG_SOUND, "/usr/local/freeswitch/sounds/en/us/callie/voicemail/8000/vm-marked-urgent.wav").
%-define(WELCOME_SOUND, "tone_stream://%(100,1000,800);loops=1").
%-define(INPUT_NUMBER_SOUND, "tone_stream://%(100,1000,800);loops=2").
%-define(SELECT_LANG_SOUND, "tone_stream://%(100,1000,800);loops=3").


-define(LOG(Fmt, Args), io:format("~b " ++ Fmt, [?LINE | Args])).

-record(state, {
    fsnode           :: atom(),                  % freeswitch node name
    uuid             :: undefined | string(),    % channel uuid
    cid_number       :: undefined | string(),    % caller id
    dest_number      :: undefined | string()     % called number
}).

start(Ref) ->
    {ok, NewPid} = gen_fsfsm:start(?MODULE, [], []),
    {Ref, NewPid}.

init([]) ->
    State = #state{fsnode = ?FS_NODE},
    {ok, welcome, State}.

%% The state machine
welcome({call, _Name, UUID, Event}, State) ->
    CidNumber = proplists:get_value(<<"Caller-Caller-ID-Number">>, Event),
    DestNumber = proplists:get_value(<<"Caller-Caller-Destination-Number">>, Event),
    ?LOG("welcome ~p", [CidNumber]),
    send_lock_msg(UUID, playback, ?WELCOME_SOUND),
    case u_utils:get_env(dtmf_type) of
        {ok, inband} ->
            send_msg(UUID, start_dtmf, "");
        _ -> ok
    end,
    send_msg(UUID, read, "1 1 " ?SELECT_LANG_SOUND " erl_lang 5000 #"),
        {next_state, wait_lang, State#state{uuid = UUID, cid_number = CidNumber, dest_number = DestNumber}}.


wait_lang({call_event, <<"CHANNEL_EXECUTE_COMPLETE">>, UUID, Event}, State) ->
    case proplists:get_value(<<"Application">>, Event) of
        <<"read">> ->
            DTMF = proplists:get_value(<<"variable_erl_lang">>, Event),
            LANG = case DTMF of
                <<"1">> -> "cn";
                <<"2">> -> "fr";
                _ -> "en"
            end,
            send_msg(UUID, set, "sound_prefix=/usr/local/freeswitch/sounds/" ++ LANG);
        _ -> ok
    end,
    send_msg(UUID, read, "1 4 " ?INPUT_NUMBER_SOUND " erl_dst_number 5000 #"),
    {next_state, wait_number, State};


wait_lang(_Any, State) -> {next_state, wait_lang, State}. % ignore any other messages


wait_number({call_event, <<"CHANNEL_EXECUTE_COMPLETE">>, UUID, Event}, State) ->

    case proplists:get_value(<<"Application">>, Event) of
        <<"read">> ->
            case proplists:get_value(<<"variable_erl_dst_number">>, Event) of
                undefined ->
                    send_msg(UUID, read, "1 4 " ?INPUT_NUMBER_SOUND " erl_dst_number 5000 #"),
                    {next_state, wait_number, State};
                Dest ->
                    send_msg(UUID, bridge, "user/" ++ binary_to_list(Dest)),
                    {next_state, wait_hangup, State}
            end;
        _ -> {next_state, wait_number, State}
    end;

wait_number(_Any, State) -> {next_state, wait_number, State}. % ignore any other messages

wait_hangup({call_event, Name, _UUID, Event}, State) ->
    ?LOG("Event: ~p~n", [Name]),
    {next_state, wait_hangup, State};

wait_hangup(_Any, State) -> {next_state, wait_hangup, State}. % ignore any other messages

handle_info(call_hangup, StateName, State) ->
    ?LOG("Call hangup on ~p~n", [StateName]),
    {stop, normal, State};

handle_info(_Info, StateName, State) -> {next_state, StateName, State}.


handle_event({channel_hangup_event, UUID, Event}, StateName, State) ->
    %% perhaps do bill here
    HangupCause = proplists:get_value(<<"Channel-Hangup-Cause">>, Event),
    ?LOG("Hangup Cause: ~p~n", [HangupCause]),
    {stop, normal, State}.

terminate(normal, StateName, State) -> ok; terminate(Reason, StateName, State) ->
    % do some clean up here
    send_msg(State#state.uuid, hangup, ""),
    ok.


%% private functions
%%
send_msg(UUID, App, Args) ->
    Headers = [{"call-command", "execute"},
    {"execute-app-name", atom_to_list(App)}, {"execute-app-arg", Args}],
    send_msg(UUID, Headers).

send_lock_msg(UUID, App, Args) ->
    Headers = [{"call-command", "execute"}, {"event-lock", "true"},
    {"execute-app-name", atom_to_list(App)}, {"execute-app-arg", Args}],
    send_msg(UUID, Headers).

send_msg(UUID, Headers) -> {sendmsg, ?FS_NODE} ! {sendmsg, UUID, Headers}.


get_active_channels() ->
    case freeswitch:api(FS_NODE, show, "channels") of
    {ok, Chans} ->
        {ok, R} = re:compile("([\\d+])"),
        {match, Match} = re:run(Chans, R, [{capture, [1], list}]),
        %lists:flatten(Match).
        Match.
    _ ->
        0
    end.