%%%-------------------------------------------------------------------
%%% @author ganesonravichandran
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Oct 2022 9:17 am
%%%-------------------------------------------------------------------
-module(push_sum_worker).
-author("ganesonravichandran").

-export([main/5, get_global_state/1, handle_message/8, listen_and_converge/7]).

get_global_state(SupervisorPid) ->
  receive
    {global_state, SupervisorPid, Mapping} -> Mapping
  end.

handle_message(CurrentActor, MaxActors, {CurrentSum, CurrentWeight}, ListOfElements, GlobalMapping, Topology, SupervisorPid, {InputSum, InputWeight}) ->

  %% Chose the next actors from the pool based on the topology.
  NextActor = util:get_next_actor(CurrentActor, MaxActors, Topology),
  NextActor2 = util:get_next_actor(CurrentActor, MaxActors, Topology),

  io:format("Actor [~p] chose [~p] & [~p] actors~n", [CurrentActor, NextActor, NextActor2]),

  %% Get PID of the new actors from the mapping
  NextPid = case dict:find(NextActor, GlobalMapping) of
              error -> io:format("Error getting Pid for NextActor [~p] by Current Actor [~p]", [NextActor, CurrentActor]);
              {ok, Pid} -> Pid
            end,
  NextPid2 = case dict:find(NextActor2, GlobalMapping) of
               error -> io:format("Error getting Pid for Next Actor [~p] by Current Actor [~p]", [NextActor2, CurrentActor]);
               {ok, Pid2} -> Pid2
             end,

  {NewSum, NewWeight} = {(CurrentSum + InputSum) / 2, (CurrentWeight + InputWeight) / 2},

  %% Send New Messages to next Pids
  NextPid ! {message_from_worker, {NewSum, NewWeight}},
  NextPid2 ! {message_from_worker, {NewSum, NewWeight}},

  % Get the length of elements to check in the list before converging
  ConvergeLength = list_to_integer(os:getenv("CONVERGE_LENGTH", "10")),

  %% Listen and Converge with the new sum
  listen_and_converge(CurrentActor, MaxActors, {NewSum, NewWeight}, util:update_fixed_list(ListOfElements, {NewSum, NewWeight}, ConvergeLength),
    GlobalMapping, Topology, SupervisorPid).

listen_and_converge(CurrentActor, MaxActors, CurrentElement, CurrentElementList, GlobalMapping, Topology, SupervisorPid) ->
  % Get the length of elements to check in the list before converging
  ConvergeLength = list_to_integer(os:getenv("CONVERGE_LENGTH", "10")),

  %% Check if the push sum has converged for the current worker.
  PushSumConverged = util:check_if_push_sum_converge(CurrentElementList, ConvergeLength),

  if
    PushSumConverged ->

      %% If push sum is converged, then notify the supervisor that it (worker) has converged.
      SupervisorPid ! {success, CurrentElement, CurrentActor};

    %% If gossip is not satisfied.
    true ->
      receive
        {message_from_supervisor, {Element, Weight}} ->
          io:format("Element - Weight [~p, ~p] received from the supervisor", [Element, Weight]),
          handle_message(CurrentActor, MaxActors, CurrentElement, CurrentElementList, GlobalMapping, Topology, SupervisorPid, {Element, Weight});
        {message_from_worker, {Element, Weight}} ->
          handle_message(CurrentActor, MaxActors, CurrentElement, CurrentElementList, GlobalMapping, Topology, SupervisorPid, {Element, Weight})
      end

  end.

main(CurrentIndex, MaxIndex, Topology, CurrentElement, SupervisorPid) ->
  GlobalMapping = get_global_state(SupervisorPid),
  listen_and_converge(CurrentIndex, MaxIndex, CurrentElement, [], GlobalMapping, Topology, SupervisorPid).

