module ComputerPlayer exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import TicTacToe.ComputerPlayer exposing (..)
import TicTacToe.Board exposing (..)


suite : Test
suite =
  describe "ComputerPlayer module"
    [ describe "firstAvailable"
      [ test "should return an index of 0 when given an empty board" <|
        \_ ->
          firstAvailable emptyBoard
            |> Expect.equal 0
      , test "should return an index of 1 when given a board where 1 is the first available" <|
        \_ ->
          let
            board =
              [ Occupied X, Empty, Empty
              , Empty, Empty, Empty
              , Empty, Empty, Empty
              ]
          in
            firstAvailable board
              |> Expect.equal 1
      ]
    ]
