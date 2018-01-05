module TicTacToe.Board exposing
  ( Board
  , Space(..)
  , Player(..)
  , reset
  , addMove
  , isSpaceEmpty
  , winningTriplet
  , winningTripletFromList
  , isWinner
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


isSpaceEmpty : Int -> Board -> Bool
isSpaceEmpty index board =
  let
    arrBoard = Array.fromList board
    space = Array.get index arrBoard
  in
    case space of
      Nothing ->
        False

      Just Empty ->
        True

      Just (Occupied a) ->
        False


isWinner : Player -> Board -> Bool
isWinner player board =
  let 
    playerList = List.filter (isPlayer player) (List.indexedMap (,) board)
  in
    winningTripletFromList playerList (0, 1, 2) ||
    winningTripletFromList playerList (3, 4, 5) ||
    winningTripletFromList playerList (6, 7, 8) ||
    winningTripletFromList playerList (0, 3, 6) ||
    winningTripletFromList playerList (1, 4, 7) ||
    winningTripletFromList playerList (2, 5, 8) ||
    winningTripletFromList playerList (0, 4, 8) ||
    winningTripletFromList playerList (2, 4, 6)


isPlayer : Player -> (Int, Space) -> Bool
isPlayer player (index, space) =
  case space of
    Empty ->
      False

    Occupied p ->
      p == player


winningTripletFromList : List (Int, Space) -> (Int, Int, Int) -> Bool
winningTripletFromList list trip =
  List.length (List.filter (winningTriplet trip) list) == 3


winningTriplet : (Int, Int, Int) -> (Int, Space) -> Bool
winningTriplet (i1, i2, i3) (index, _) =
  index == i1 || index == i2 || index == i3
