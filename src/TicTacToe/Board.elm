module TicTacToe.Board exposing
  ( Board
  , Space(..)
  , Player(..)
  , reset
  , addMove
  , isSpaceOccupied
  )

import Array


type alias Board = List Space


type Player
  = X
  | O


type Space
  = Empty
  | Occupied Player


reset : Board -> Board
reset board =
  [ Empty, Empty, Empty
  , Empty, Empty, Empty
  , Empty, Empty, Empty
  ]


addMove : Player -> Int -> Board -> Board
addMove player index board =
  Array.fromList board
    |> Array.set index (Occupied player)
    |> Array.toList


isSpaceOccupied : Int -> Board -> Bool
isSpaceOccupied index board =
  let
    arrBoard = Array.fromList board
    space = Array.get index arrBoard
  in
    case space of
      Nothing ->
        True

      Just Empty ->
        False

      Just (Occupied a) ->
        True
