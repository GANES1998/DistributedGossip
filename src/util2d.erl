%%%-------------------------------------------------------------------
%%% @author ganesonravichandran
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Sep 2022 2:39 pm
%%%-------------------------------------------------------------------
-module(util2d).
-author("ganesonravichandran").

%% API
-export([send_global_state_2D/3, spawn2D/5]).

%% Send the mapping of {Row, Col} -> Pid to all the workers
send_global_state_2D({CurrentRow, CurrentCol}, {Rows, Cols}, Mapping) ->
  if
  % If all workers are send the global state - stop
    (CurrentRow > Rows) ->
      io:format("Global State casted to all Workers ~n"),
      Mapping;

  % If all All workers of current row are sent the global state, send to first worker in the next row.
    (CurrentCol == Cols) ->

      %% Find the worker pid corresponding to the current row and column
      {ok, WorkerPid} = dict:find({CurrentRow, CurrentCol}, Mapping),

      % Worker Id - send global state to that worker
      WorkerPid ! {global_state, self(), Mapping},

      send_global_state_2D({CurrentRow + 1, 1}, {Rows, Cols}, Mapping);

  % Send the global state to next row and next column
    true ->

      %% Find the worker pid corresponding to the current row and column
      {ok, WorkerPid} = dict:find({CurrentRow, CurrentCol}, Mapping),

      % Worker Id - send global state to that worker
      WorkerPid ! {global_state, self(), Mapping},

      send_global_state_2D({CurrentRow, CurrentCol + 1}, {Rows, Cols}, Mapping)
  end.

spawn2D({CurrentRow, CurrentCol}, {Rows, Cols}, Topology, Algorithm, Mapping) ->
  if
  %% If all the Rows an Columns are spawned - Return success.
    (CurrentRow > Rows) ->
      io:format("Successfully spawned a 2D grid of [~p * ~p]~n", [Rows, Cols]),
      Mapping;
  %% If One Row is completed spawning move to the next row.
    (CurrentCol == Cols) ->
      %% Spawn the corresponding worker and get the Process Id back
      SpawnedPid = case Algorithm of
                     gossip -> spawn_link(node(), gossip_worker, main, [{CurrentRow, CurrentCol}, {Rows, Cols}, Topology, self()]);
                     push_sum -> io:format("Push Sum is not yet implemented and needs to be implemented")
                   end,

      spawn2D({CurrentRow + 1, 1}, {Rows, Cols}, Topology, Algorithm, dict:store({CurrentRow, CurrentCol}, SpawnedPid, Mapping));
  %% Spawn the next column of current row
    true ->
      %% Spawn the corresponding worker and get the Process Id back
      SpawnedPid = case Algorithm of
                     gossip -> spawn_link(node(), gossip_worker, main, [{CurrentRow, CurrentCol}, {Rows, Cols}, Topology, self()]);
                     push_sum -> io:format("Path Sum is not yet implemented and needs to be implemented")
                   end,

      spawn2D({CurrentRow, CurrentCol + 1}, {Rows, Cols}, Topology, Algorithm, dict:store({CurrentRow, CurrentCol}, SpawnedPid, Mapping))
  end.