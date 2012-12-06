-module(myhandler).

-export([start/0,run/0,launch/1]).


start()->
  %% start our handler process running
  Pid = spawn(?MODULE,run,[]),
  %% register it with the same name as the module - myhandler
  register(?MODULE,Pid).

run()->
  %% wait for messages from FreeSWITCH
  receive
    {call,Data}->
      %% a new call is starting, find the UUID
      %% _Rest is a list of all the channel variable in the form {"<name>","<value"}
      {event, [UUID | _Rest]} = Data,
      error_logger:info_msg("myhandler ~p: new call received, UUID is ~p~n",[self(), UUID]),
      run();
    {call_event,Data} ->
      %% we've got an event for a call we've been notified of already
      {event, [UUID | Rest]} = Data,
      %% find out the name of the event
      Name = proplists:get_value("Event-Name", Rest),
      error_logger:info_msg("myhandler ~p: UUID ~p, event ~p~n",[self(), UUID,Name]),
      run();
    {get_pid, UUID, Ref, Pid} ->
      %% request from FreeSWITCH for an outbound process to handle call at 'UUID'
      NewPid = spawn(?MODULE, run, []),
      error_logger:info_msg("myhandler ~p: request to spawn new handler process, returning ~p~n", [self(), NewPid]),
      Pid ! {Ref, NewPid},
      run()
  end.

launch(Ref) ->
  %% rpc call to a function to return a new outbound call pid
  NewPid = spawn(?MODULE, run, []),
  error_logger:info_msg("myhandler ~p: launch request, returning ~p~n", [self(), NewPid]),
  {Ref, NewPid}.
