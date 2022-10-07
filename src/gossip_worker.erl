%%%-------------------------------------------------------------------
%%% @author ganesonravichandran
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Sep 2022 12:48 pm
%%%-------------------------------------------------------------------
-module(gossip_worker).
-author("ganesonravichandran").

%% API
-export([get_global_state/1, main/4, handle_rumer/8, listen_and_converge/8]).

get_global_state(SupervisorPid) ->
  % Wait for the global state to be shared by the supervisor.
  receive
    {global_state, SupervisorPid, Mapping} -> Mapping
  end.

handle_rumer(CurrentActor, MaxActors, MessageList, GlobalMapping, Topology, SupervisorPid, Rumer, IsNewMessage) ->
  % Choose the next actor
  NextActor = util:get_next_actor(CurrentActor, MaxActors, Topology),

  % Get PID corresponding to next actor
  NextPid = case dict:find(NextActor, GlobalMapping) of
              error -> io:format("Error finding Pid for NextActor [~p] by Actor [~p]", [CurrentActor, NextActor]);
              {ok, Pid} -> Pid
            end,


  % Send the current Rumer to the next chosen worker
  NextPid ! {rumer_from_worker, Rumer},
  %%  NextPid2 ! {rumer_from_worker, Rumer},

  % Listen for such rumers and try to converge
  if
    IsNewMessage ->
    %%  If this a new message, then append it to the seen messages list
      listen_and_converge(CurrentActor, MaxActors, Rumer, lists:append(MessageList, [Rumer]), GlobalMapping, Topology, SupervisorPid, true);

    true ->
    %% If this is not a new message, pass the same message list
      listen_and_converge(CurrentActor, MaxActors, Rumer, MessageList, GlobalMapping, Topology, SupervisorPid, true)
  end.


listen_and_converge(CurrentActor, MaxActors, CurrentElement, MessageList, GlobalMapping, Topology, SupervisorPid, HasValidMessage) ->
  % Get the length of elements to check in the list before converging
  ConvergeLength = list_to_integer(os:getenv("CONVERGE_LENGTH", "9")),

  % Get the length of current messages seen by this actor.
  ListLength = lists:flatlength(MessageList),

  % Check if Gossip Condition is satisfied - Converge length arrieved and all the elements in the message list is current element
  GossipSatisfied = ListLength == ConvergeLength andalso util:check_if_list_converge_gossip(MessageList, CurrentElement),

  if
    GossipSatisfied ->

      % If Gossip is satisfied - Notify the supervisor of the same and kill the worker
      SupervisorPid ! {success, lists:nth(lists:flatlength(MessageList), MessageList), CurrentActor};

  % If Gossip is not satisfied.
    true ->
      receive
      % Only one choosen in random by the supervisor will receive such a message.
        {rumer_from_supervisor, Rumer} ->
          % Happen only the first time.
          io:format("Rumer [~p] received from the supervisor", [Rumer]),
          handle_rumer(CurrentActor, MaxActors, MessageList, GlobalMapping, Topology, SupervisorPid, Rumer, true);

      % When one sender is sendin a message
        {rumer_from_worker, Rumer} ->
          % When workers are communicating among themselves.
          handle_rumer(CurrentActor, MaxActors, MessageList, GlobalMapping, Topology, SupervisorPid, Rumer, true)
      after 1 ->
        if
          % The current worker has at least received one message either from supervisor or another worker.
          HasValidMessage ->
            handle_rumer(CurrentActor, MaxActors, MessageList, GlobalMapping, Topology, SupervisorPid, CurrentElement, false);

          %
          true ->
            listen_and_converge(CurrentActor, MaxActors, CurrentElement, MessageList, GlobalMapping, Topology, SupervisorPid, HasValidMessage)
        end
      end
  end.

main(CurrentIndex, MaxIndex, Topology, SupervisorPid) ->
  % Wait and get the global state shared by the supervisor
  Mapping = get_global_state(SupervisorPid),

  % Listen and converge (receive same message for 10 times).
  listen_and_converge(CurrentIndex, MaxIndex, "", [], Mapping, Topology, SupervisorPid, false).
