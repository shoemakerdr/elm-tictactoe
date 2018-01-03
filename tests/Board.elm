module Board exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import TicTacToe.Board exposing
  ( Board
  , Space(..)
  , Player(..)
  , reset
  )


suite : Test
suite =
  describe "Board functions"
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
    ]


emptyBoard : Board
emptyBoard = 
  [ Empty, Empty, Empty
  , Empty, Empty, Empty
  , Empty, Empty, Empty
  ]
