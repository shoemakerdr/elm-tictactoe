module Board exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import TicTacToe.Board exposing
  ( Board
  , Space(..)
  , Player(..)
  , reset
  , addMove
  , isSpaceOccupied
  )


suite : Test
suite =
  describe "Board helper functions"
    [ describe "reset"
      [ test "should take an empty board and return an empty board" <|
          \_ ->
            let
              board =
                [ Empty, Empty, Empty
                , Empty, Empty, Empty
                , Empty, Empty, Empty
                ]
            in
              Expect.equal emptyBoard (reset board)
      , test "should take a board with some occupied spaces and return an empty board" <|
          \_ ->
            let
              board =
                [ Empty, Empty, Occupied O
                , Occupied X, Occupied X, Empty
                , Empty, Occupied O, Empty
                ]
            in
              Expect.equal emptyBoard (reset board)
      ]
    , describe "addMove"
      [ test "should take a Player, an index, and a board and return a board with that Player occupying the board at that index" <|
        \_ ->
          let
            board =
              [ Occupied X, Empty, Empty
              , Empty, Empty, Empty
              , Empty, Empty, Empty
              ]
          in
            Expect.equal board (addMove X 0 emptyBoard)
      ]
    , describe "isSpaceOccupied"
      [ test "should return False when space is empty" <|
          \_ ->
            Expect.equal False (isSpaceOccupied 0 emptyBoard)
      , test "should return True when space is occupied" <|
          \_ -> 
            let 
              board = 
                [ Occupied X, Empty, Empty
                , Empty, Empty, Empty
                , Empty, Empty, Empty
                ]
            in
              Expect.equal True (isSpaceOccupied 0 board)
      ]
    ]


emptyBoard : Board
emptyBoard = 
  [ Empty, Empty, Empty
  , Empty, Empty, Empty
  , Empty, Empty, Empty
  ]
