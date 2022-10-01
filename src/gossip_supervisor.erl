%%%-------------------------------------------------------------------
%%% @author ganesonravichandran
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Sep 2022 10:21 pm
%%%-------------------------------------------------------------------
-module(gossip_supervisor).
-author("ganesonravichandran").

%% API
-export([main/3]).





send_secret_message(WorkerPid) ->
  % Send the secret message to the Worker Pid.
  WorkerPid ! {rumer_from_supervisor, ganeson}.

wait_for_message_completion(CurrentIndex, MaxWorkers) ->
  if
    CurrentIndex > MaxWorkers -> io:format("All [~p] Workers Agreed after gossip", [MaxWorkers]);
    true ->
      receive
        {success, Message, Actor} ->
          io:format("Received Message [~p] from Actor [~p]~n", [Message, Actor]),
          wait_for_message_completion(CurrentIndex + 1, MaxWorkers)
      end
  end.

handle_2D(ActorCount, Topology, Algorithm) ->
  % If the input topology is 2D, and input is not perfect square, get nearby square number.
  AdjustedActorCount = util:get_nearest_square_number(ActorCount),

  if
  % Print that the actor count is adjusted to support the topology
    ActorCount =/= AdjustedActorCount -> io:format("Given Actor Count [~p] Not a square - It's adjusted to [~p]", [ActorCount, AdjustedActorCount]);
    % Given Actor Count is a perfect square, So don't print anything.
    true -> io:format("")
  end,
  % Get rows and columns are sqrt of Actor Count.
  {Rows, Cols} = {trunc(math:sqrt(AdjustedActorCount)), trunc(math:sqrt(AdjustedActorCount))},

  % Get Mapping after spawning all processes corresponding to each row and column.
  Mapping = util2d:spawn2D({1, 1}, {Rows, Cols}, Topology, Algorithm, dict:new()),

  % Send Global State to all the processes
  NewMappings = util2d:send_global_state_2D({1, 1}, {Rows, Cols}, Mapping),

  % Choose the worker to receive secret from the supervisor. Here it is always the first process.
  {ok, FirstWorkerPid} = dict:find({1, 1}, NewMappings),

  % Send the secret message to the chosen worker
  send_secret_message(FirstWorkerPid),

  % Start the clock for finding wall clock time for convergence
  statistics(wall_clock),

  % Wait for all the workers to return success.
  wait_for_message_completion(1, Rows * Cols),

  % Stop the wall clock for calculating the time elapsed for convergence.
  {_, WallClockTime} = statistics(wall_clock),

  %% Print the statistics
  io:format("Wall Clock Time elasped for all [~p] workers to conclude [~p]~n", [Rows * Cols, WallClockTime]).

handle_1D(ActorCount, Topology, Algorithm) ->
  % Get Mapping after spawning all processes corresponding to each row and column.
  Mapping = util1d:spawn1D(1, ActorCount, Topology, Algorithm, dict:new()),

  % Send Global State to all the processes
  NewMappings = util1d:send_global_state_1d(1, ActorCount, Mapping),

  % Choose the worker to receive secret from the supervisor. Here it is always the first process.
  {ok, FirstWorkerPid} = dict:find(1, NewMappings),

  % Send the secret message to the chosen worker
  send_secret_message(FirstWorkerPid),

  % Start the clock for finding wall clock time for convergence
  statistics(wall_clock),

  % Wait for all the workers to return success.
  wait_for_message_completion(1, ActorCount),

  % Stop the wall clock for calculating the time elapsed for convergence.
  {_, WallClockTime} = statistics(wall_clock),

  %% Print the statistics
  io:format("Wall Clock Time elasped for all [~p] workers to conclude [~p]!n", [ActorCount, WallClockTime]).


main(ActorCount, Topology, Algorithm) ->
  case Topology of
    twoD ->
      handle_2D(ActorCount, Topology, Algorithm);
    twoDImperfect ->
      handle_2D(ActorCount, Topology, Algorithm);
    full ->
      handle_1D(ActorCount, Topology, Algorithm);
    line ->
      handle_1D(ActorCount, Topology, Algorithm)
  end.




