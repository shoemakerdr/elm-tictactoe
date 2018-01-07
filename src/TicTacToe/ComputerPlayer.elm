module TicTacToe.ComputerPlayer exposing
  (..)

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
        Debug.crash "shouldn't get here"
