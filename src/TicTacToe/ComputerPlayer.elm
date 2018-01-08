module TicTacToe.ComputerPlayer exposing
  (..)

import Random
import Random.List as RL
import TicTacToe.Board exposing (..)

firstAvailable : Board -> Int
firstAvailable board =
  let
    filtered =
      List.indexedMap (,) board
        |> List.filter (\(_, space) -> space == Empty)
  in
    case (List.head filtered) of
      Just (index, _) ->
        index

      _ ->
        Debug.crash "firstAvailable should not have been given a full board"


randomSpaceGenerator : List Space -> Random.Generator Int
randomSpaceGenerator board =
  let
    emptySpaces =
      List.indexedMap (,) board
        |> List.filter (\(_, space) -> space == Empty)
    choice =
      RL.choose emptySpaces
  in
    Random.map tupleToInt choice


tupleToInt : (Maybe (Int, Space), List (Int, Space)) -> Int
tupleToInt (tup, _) =
  case tup of
    Nothing ->
      Debug.crash "tupleToInt should not have been given an empty list"

    Just (int, _) ->
      int
