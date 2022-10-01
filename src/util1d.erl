%%%-------------------------------------------------------------------
%%% @author ganesonravichandran
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Sep 2022 2:41 pm
%%%-------------------------------------------------------------------
-module(util1d).
-author("ganesonravichandran").

%% API
-export([send_global_state_1d/3, spawn1D/5]).

% Send Global state starting From current position to Max Position.
send_global_state_1d(CurrentPosition, MaxPosition, Mapping) ->
  if
  % All Workers have been send the global_state, then exit.
    CurrentPosition > MaxPosition ->
      io:format("Global State successfully casted to all workers~n"),
      Mapping;
    true ->
      % Find the worker Pid corresponding to current position
      {ok, WorkerPid} = dict:find(CurrentPosition, Mapping),

      % Worker Id - send global state to that worker
      WorkerPid ! {global_state, self(), Mapping},

      % Send the global state to the next process.
      send_global_state_1d(CurrentPosition + 1, MaxPosition, Mapping)
  end.

% Spawn Worker from current position to Max Position.
spawn1D(CurrentPosition, MaxPositions, Topology, Algorithm, Mapping) ->
  if
  % If all actors are spawned
    CurrentPosition > MaxPositions ->
      io:format("All [~p] workers are spawned~n", [MaxPositions]),
      Mapping;
    true ->
      % Spawn the corresponding process and get the Process Id back
      SpawnedPid = case Algorithm of
                     gossip -> spawn_link(node(), gossip_worker, main, [CurrentPosition, MaxPositions, Topology, self()]);
                     push_sum -> io:format("Push sum algorithm not yet implemented")
                   end,
      spawn1D(CurrentPosition + 1, MaxPositions, Topology, Algorithm, dict:store(CurrentPosition, SpawnedPid, Mapping))
  end.
