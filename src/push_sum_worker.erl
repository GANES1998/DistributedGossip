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
  % Wait to receive the global state - position - Pid mapping for all workers.
  receive
    {global_state, SupervisorPid, Mapping} -> Mapping
  end.

handle_message(CurrentActor, MaxActors, {CurrentSum, CurrentWeight}, ListOfElements, GlobalMapping, Topology, SupervisorPid, {InputSum, InputWeight}) ->

  %% Chose the next actors from the pool based on the topology.
  NextActor = util:get_next_actor(CurrentActor, MaxActors, Topology),

  %% Get PID of the new actors from the mapping
  NextPid = case dict:find(NextActor, GlobalMapping) of
              error -> io:format("Error getting Pid for NextActor [~p] by Current Actor [~p]", [NextActor, CurrentActor]);
              {ok, Pid} -> Pid
            end,

  %% Updated the sum and weight of current element based on input.
  {NewSum, NewWeight} = {(CurrentSum + InputSum) / 2, (CurrentWeight + InputWeight) / 2},

  %% Send New Messages to next Pids
  NextPid ! {message_from_worker, {NewSum, NewWeight}},

  %% Get the length of elements to check in the list before converging
  ConvergeLength = list_to_integer(os:getenv("CONVERGE_LENGTH", "5")),

  %% Listen and Converge with the new sum
  listen_and_converge(CurrentActor, MaxActors, {NewSum, NewWeight},
    util:update_fixed_list(ListOfElements, {NewSum, NewWeight}, ConvergeLength), GlobalMapping, Topology, SupervisorPid).

listen_and_converge(CurrentActor, MaxActors, CurrentElement, CurrentElementList, GlobalMapping, Topology, SupervisorPid) ->
  % Get the length of elements to check in the list before converging
  ConvergeLength = list_to_integer(os:getenv("CONVERGE_LENGTH", "5")),

  %% Check if the push sum has converged for the current worker.
  PushSumConverged = util:check_if_push_sum_converge(CurrentElementList, ConvergeLength),

  %% Get the sum and weight of current element.
  {Sum, CurrentWeight} = CurrentElement,

  if
    PushSumConverged ->

      %% If push sum is converged, then notify the supervisor that it (worker) has converged.
      SupervisorPid ! {success, CurrentElement, CurrentActor};

  %% If gossip is not satisfied.
    true ->
      receive
        %% Wait till you receive message from supervisor or worker.
        {message_from_supervisor, {Element, Weight}} ->

          %% Will be sent to only one worker to start the push sum.
          io:format("Element - Weight [~p, ~p] received from the supervisor", [Element, Weight]),
          handle_message(CurrentActor, MaxActors, CurrentElement, CurrentElementList, GlobalMapping, Topology, SupervisorPid, {Element, Weight});
        {message_from_worker, {Element, Weight}} ->

          %% Workers send among themselves till they converge.
          handle_message(CurrentActor, MaxActors, CurrentElement, CurrentElementList, GlobalMapping, Topology, SupervisorPid, {Element, Weight})
      after 500 ->

        %% If no message is received, send another message to another random worker constrained.
        %% This is overcome when a message is send to a dead worker.
        handle_message(CurrentActor, MaxActors, CurrentElement, CurrentElementList, GlobalMapping, Topology, SupervisorPid, {Sum, CurrentWeight})
      end
  end.

main(CurrentIndex, MaxIndex, Topology, CurrentElement, SupervisorPid) ->
  % Wait and get the global position - Pid mapping of all workers.
  GlobalMapping = get_global_state(SupervisorPid),

  % Listen and wait for all the worker nodes to converge (Receive same (~ difference of 10^-10) sum/wait ratio for 5 times).
  listen_and_converge(CurrentIndex, MaxIndex, CurrentElement, [], GlobalMapping, Topology, SupervisorPid).

