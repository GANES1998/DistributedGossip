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





send_secret_message(WorkerPid, Algorithm) ->

  case Algorithm of
    gossip ->
      % Send the secret message to the Worker Pid.
      WorkerPid ! {rumer_from_supervisor, ganeson};
    push_sum ->
      % Send the first node, its sum and weight.
      WorkerPid ! {message_from_supervisor, {1, 1}}
  end.



wait_for_message_completion(CurrentIndex, MaxWorkers) ->
  if
    %% If all the workers have responded to the server.
    CurrentIndex > MaxWorkers ->
      io:format("All [~p] Workers Agreed after gossip ~n", [MaxWorkers]),

      %% Return the total workers count. Same as Max Workers
      CurrentIndex - 1;
    true ->

      %% Wait to receive some message for 5 seconds
      receive
        {success, Message, Actor} ->
          io:format("Received Message [~p] from Actor [~p]~n", [Message, Actor]),
          wait_for_message_completion(CurrentIndex + 1, MaxWorkers)
      %%  If we don't hear anything from workers for 5 seconds straight
      after 5000 ->
        if
          CurrentIndex == 1 ->
            %%  Still no worker has converged, So wait another 5 seconds for initial convergence to start
            wait_for_message_completion(CurrentIndex, MaxWorkers);
          true ->
            %%  Some workers have started converging and no workers have converged for the last 5 seconds. So, it's unlikely the next nodes will converge.
            io:format("Not heard from Any Workers for [ ~p ] ms. Convergance Ratio = ~p~n", [5000, (CurrentIndex - 1) / MaxWorkers]),
            CurrentIndex - 1
        end
      end
  end.

handle_2D(ActorCount, Topology, Algorithm) ->
  % If the input topology is 2D, and input is not perfect square, get nearby square number.
  AdjustedActorCount = util:get_nearest_square_number(ActorCount),

  if
  % Print that the actor count is adjusted to support the topology
    ActorCount =/= AdjustedActorCount ->
      io:format("Given Actor Count [~p] Not a square - It's adjusted to [~p]", [ActorCount, AdjustedActorCount]);
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
  send_secret_message(FirstWorkerPid, Algorithm),

  % Start the clock for finding wall clock time for convergence
  statistics(wall_clock),

  % Wait for all the workers to return success.
  ConvergedNodes = wait_for_message_completion(1, Rows * Cols),

  % Stop the wall clock for calculating the time elapsed for convergence.
  {_, WallClockTime} = statistics(wall_clock),

  %% Print the statistics
  io:format("Wall Clock Time elasped for all [~p] out of [~p] (~p %) workers to conclude [~p]!n", [ConvergedNodes, AdjustedActorCount, (ConvergedNodes / AdjustedActorCount * 100), WallClockTime]).

handle_1D(ActorCount, Topology, Algorithm) ->
  % Get Mapping after spawning all processes corresponding to each row and column.
  Mapping = util1d:spawn1D(1, ActorCount, Topology, Algorithm, dict:new()),

  % Send Global State to all the processes
  NewMappings = util1d:send_global_state_1d(1, ActorCount, Mapping),

  % Choose the worker to receive secret from the supervisor. Here it is always the first process.
  {ok, FirstWorkerPid} = dict:find(1, NewMappings),

  % Send the secret message to the chosen worker
  send_secret_message(FirstWorkerPid, Algorithm),

  % Start the clock for finding wall clock time for convergence
  statistics(wall_clock),

  % Wait for all the workers to return success.
  ConvergedNodes = wait_for_message_completion(1, ActorCount),

  % Stop the wall clock for calculating the time elapsed for convergence.
  {_, WallClockTime} = statistics(wall_clock),

  %% Print the statistics
  io:format("Wall Clock Time elasped for all [~p] out of [~p] (~p %) workers to conclude [~p]!n", [ConvergedNodes, ActorCount, (ConvergedNodes / ActorCount * 100), WallClockTime]).


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




