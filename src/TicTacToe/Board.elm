module TicTacToe.Board exposing
  ( Board
  , Space(..)
  , Player(..)
  , reset
  )

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

