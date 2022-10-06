%%%-------------------------------------------------------------------
%%% @author ganesonravichandran
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Sep 2022 3:39 pm
%%%-------------------------------------------------------------------
-module(util).
-author("ganesonravichandran").


%% API
-export([get_nearest_square_number/1, get_next_actor/3, check_if_list_converge_gossip/2, normalize_processes_count/2, check_if_push_sum_converge/2, update_fixed_list/3]).


get_nearest_square_number(Number) ->
  Sqrt = math:sqrt(Number),
  SqrtInt = trunc(Sqrt),
  if
    (Number - (SqrtInt * SqrtInt)) > (((SqrtInt + 1) * (SqrtInt + 1)) -Number)
      -> ((SqrtInt + 1) * (SqrtInt + 1));
    true -> (SqrtInt * SqrtInt)
  end.

normalize_processes_count(Number, Algorithm) ->
  case Algorithm of
    full -> Number;
    twoD -> get_nearest_square_number(Number);
    line -> Number
  end.


check_if_list_converge_gossip(List, Message) ->
  lists:all(
    fun(ListElement) -> ListElement == Message end,
    List
  ).

check_if_push_sum_converge(List, MaxLength) ->
  ListLength = lists:flatlength(List),
  LengthSatisfied = ListLength == MaxLength,
  if
    LengthSatisfied ->
      {FirstElementSum, _FirstElementWeight} = lists:nth(1, List),
      lists:all(
        fun({ElementSum, _ElementWeight}) ->
%%          io:format("FirstElementSum ~p Element Sum ~p ~n", [FirstElementSum, ElementSum]),
          erlang:abs(FirstElementSum - ElementSum) < 0.001
        end,
        List
      );
    true -> false
  end.


get_neighbouring_2d_actor(1, 1, _Rows, _Cols) ->
  [{2, 1}, {1, 2}, {2, 2}];
get_neighbouring_2d_actor(1, N, _Rows, Cols) when N == Cols ->
  [{2, N}, {1, N - 1}, {2, N - 1}];
get_neighbouring_2d_actor(1, N, _Rows, _Cols) ->
  [{2, N}, {1, N + 1}, {1, N - 1}, {2, N + 1}, {2, N - 1}];
get_neighbouring_2d_actor(N, 1, Rows, _Cols) when N == Rows ->
  [{N, 2}, {N - 1, 1}, {N - 1, 2}];
get_neighbouring_2d_actor(N, 1, _Rows, _Cols) ->
  [{N, 2}, {N + 1, 1} ,{N - 1, 1}, {N + 1, 2}, {N - 1, 2}];
get_neighbouring_2d_actor(X, Y, Rows, Cols) when (X == Rows) and (Y == Cols) ->
  [{X - 1, Y}, {X, Y - 1}, {X -1, Y - 1}];
get_neighbouring_2d_actor(X, Y, Rows, _Cols) when (X == Rows) ->
  [{X - 1, Y}, {X, Y - 1}, {X, Y + 1}, {X - 1, Y - 1}, {X - 1, Y + 1}];
get_neighbouring_2d_actor(X, Y, _Rows, Cols) when (Y == Cols) ->
  [{X - 1, Y}, {X, Y - 1}, {X + 1, Y}, {X - 1, Y - 1}, {X + 1, Y - 1}];
get_neighbouring_2d_actor(X, Y, _Rows, _Cols) ->
  [{X - 1, Y}, {X, Y - 1}, {X + 1, Y}, {X, Y + 1}, {X - 1, Y - 1}, {X + 1, Y - 1}, {X + 1, Y + 1}, {X - 1, Y + 1}].



get_neighbouring_1d_actor(1, _Max) ->
  [2];
get_neighbouring_1d_actor(N, Max) when N == Max ->
  [N - 1];
get_neighbouring_1d_actor(Position, _Max) ->
  [Position - 1, Position + 1].

update_fixed_list(List, NewElement, MaxLength) ->
  ModifiedList = lists:sublist(List, MaxLength - 1),
  lists:append([NewElement], ModifiedList).

choose_random_element(Array) ->
  ArrayLength = lists:flatlength(Array),
  RandomIndex = random:uniform(ArrayLength),
  lists:nth(RandomIndex, Array).

get_next_actor(CurrentActor, MaxActors, Topology) ->
  case Topology of
    full ->
      NextActor = rand:uniform(MaxActors),
      if
        CurrentActor == NextActor -> get_next_actor(CurrentActor, MaxActors, Topology);
        true -> NextActor
      end;
    twoD ->
      {CurrentX, CurrentY} = CurrentActor,
      {Rows, Cols} = MaxActors,
      NextActors = get_neighbouring_2d_actor(CurrentX, CurrentY, Rows, Cols),
      NextActor = choose_random_element(NextActors),
      {NextX, NextY} = NextActor,
      {NextX, NextY};
    twoDImperfect ->
      {CurrentX, CurrentY} = CurrentActor,
      {Rows, Cols} = MaxActors,
      NextActors = get_neighbouring_2d_actor(CurrentX, CurrentY, Rows, Cols),
      RandomActor = {choose_random_element(lists:seq(1, Rows)), choose_random_element(lists:seq(1, Cols))},
      NextImperfectActors = lists:append(NextActors, [RandomActor]),
      {NextX, NextY} = choose_random_element(NextImperfectActors),
      {NextX, NextY};
    line ->
      NextActors = get_neighbouring_1d_actor(CurrentActor, MaxActors),
      NextActor = choose_random_element(NextActors),
      NextActor
  end.



