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
-export([get_global_state/1, handle_rumer/7, listen_and_converge/7, main/4]).

get_global_state(SupervisorPid) ->
  receive
    {global_state, SupervisorPid, Mapping} -> Mapping
  end.

handle_rumer(CurrentActor, MaxActors, MessageList, GlobalMapping, Topology, SupervisorPid, Rumer) ->
  % Choose the next actor
  NextActor = util:get_next_actor(CurrentActor, MaxActors, Topology),
  NextActor2 = util:get_next_actor(CurrentActor, MaxActors, Topology),

  io:format("Actor [~p] chose [~p] & [~p] actor~n", [CurrentActor, NextActor, NextActor2]),

  % Get PID corresponding to next actor
  NextPid = case dict:find(NextActor, GlobalMapping) of
              error -> io:format("Error finding Pid for NextActor [~p] by Actor [~p]", [CurrentActor, NextActor]);
              {ok, Pid} -> Pid
            end,
  NextPid2 = case dict:find(NextActor2, GlobalMapping) of
               error -> io:format("Error finding Pid for NextActor [~p] by Actor [~p]", [CurrentActor, NextActor]);
               {ok, APid} -> APid
             end,

  % Send the current Rumer to the next chosen worker
  NextPid ! {rumer_from_worker, Rumer},
  NextPid2 ! {rumer_from_worker, Rumer},

  % Listen for such rumers and try to converge
  listen_and_converge(CurrentActor, MaxActors, Rumer, lists:append(MessageList, [Rumer]), GlobalMapping, Topology, SupervisorPid).

listen_and_converge(CurrentActor, MaxActors, CurrentElement, MessageList, GlobalMapping, Topology, SupervisorPid) ->
  % Get the length of elements to check in the list before converging
  ConvergeLength = list_to_integer(os:getenv("CONVERGE_LENGTH", "10")),

  % Get the length of current messages seen by this actor.
  ListLength = lists:flatlength(MessageList),

  % Check if Gossip Condition is satisfied - Converge length arrieved and all the elements in the message list is current element
  GossipSatisfied = ListLength == ConvergeLength andalso util:check_if_list_converge_gossip(MessageList, CurrentElement),
  if
    GossipSatisfied ->

      % If Gossip is satisfied - Notify the supervisor of the same and kill the worker
      SupervisorPid ! {success, lists:nth(1, MessageList), CurrentActor};

    % If Gossip is not satisfied.
    true ->
      receive
        % Only one choosen in random by the supervisor will receive such a message.
        {rumer_from_supervisor, Rumer} ->
          io:format("Rumer [~p] received from the supervisor", [Rumer]),
          handle_rumer(CurrentActor, MaxActors, MessageList, GlobalMapping, Topology, SupervisorPid, Rumer);

        % When one sender is sendin a message
        {rumer_from_worker, Rumer} ->
          handle_rumer(CurrentActor, MaxActors, MessageList, GlobalMapping, Topology, SupervisorPid, Rumer)
      end
  end.

main(CurrentIndex, MaxIndex, Topology, SupervisorPid) ->
  Mapping = get_global_state(SupervisorPid),
  listen_and_converge(CurrentIndex, MaxIndex, "", [], Mapping, Topology, SupervisorPid).
