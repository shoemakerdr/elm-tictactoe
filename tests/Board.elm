module Board exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import TicTacToe.Board exposing (..)


suite : Test
suite =
  describe "Board module"
    [ describe "addMove"
      [ test "should take a Player, an index, and a board and return a board with that Player occupying the board at that index" <|
        \_ ->
          let
            board =
              [ Occupied X, Empty, Empty
              , Empty, Empty, Empty
              , Empty, Empty, Empty
              ]
          in
            addMove X 0 emptyBoard
              |> Expect.equal board 
      ]
    , describe "isFull"
      [ test "should return false when given empty board" <|
          \_ ->
            isFull emptyBoard
              |> Expect.false "should return false"
      ]
    , describe "isSpaceEmpty"
      [ test "should return false when space is empty" <|
          \_ ->
            isSpaceEmpty 0 emptyBoard
              |> Expect.true "should be empty"
      , test "should return true when space is occupied" <|
          \_ -> 
            let 
              board = 
                [ Occupied X, Empty, Empty
                , Empty, Empty, Empty
                , Empty, Empty, Empty
                ]
            in
              isSpaceEmpty 0 board
                |> Expect.false "should not be empty"
      ]
    , describe "winningTriplet"
      [ test "should return false when index not in triplet" <|
          \_ ->
            winningTriplet (0,1,2) (3, Occupied X)
              |> Expect.false "should return false"
      , test "should return true when index in triplet" <|
          \_ ->
            winningTriplet (0,1,2) (0, Occupied X)
              |> Expect.true "should return true"
      ]
    , describe "winningTripletFromList"
      [ test "should return false when a winning triplet is not found" <|
          \_ ->
            let 
              playerList =
                [ (3, Occupied X)
                , (4, Occupied X)
                , (5, Occupied X)
                ]
            in
              (0,1,2)
                |> winningTripletFromList playerList
                |> Expect.false "should return false"
      , test "should return true when a winning triplet is found" <|
          \_ ->
            let 
              playerList =
                [ (3, Occupied X)
                , (4, Occupied X)
                , (5, Occupied X)
                ]
            in
              (3,4,5)
                |> winningTripletFromList playerList
                |> Expect.true "should return true"
      , test "should return false when a winning triplet is only partially found" <|
          \_ ->
            let 
              playerList =
                [ (3, Occupied X)
                , (4, Occupied X)
                , (6, Occupied X)
                , (7, Occupied X)
                ]
            in
              (3,4,5)
                |> winningTripletFromList playerList
                |> Expect.false "should return false"
      ]
    , describe "isWinner"
      [ test "should return false when given an empty board" <|
          \_ ->
            emptyBoard
              |> isWinner X
              |> Expect.false "should return false"
      , test "should return true when given the player X and a board with X as winner" <|
          \_ ->
            let
              board =
                [ Occupied X, Occupied X, Occupied X
                , Empty , Empty, Empty
                , Empty , Empty, Empty
                ]
            in
              board
                |> isWinner X
                |> Expect.true "should return true"
      , test "should return false when given the player O and a board with X as winner" <|
          \_ ->
            let
              board =
                [ Occupied X, Occupied X, Occupied X
                , Empty , Empty, Empty
                , Empty , Empty, Empty
                ]
            in
              board
                |> isWinner O
                |> Expect.false "should return false"
      , test "should return true when given the player X and a board with X as column winner" <|
          \_ ->
            let
              board =
                [ Occupied X, Empty, Empty
                , Occupied X , Empty, Empty
                , Occupied X , Empty, Empty
                ]
            in
              board
                |> isWinner X
                |> Expect.true "should return true"
      , test "should return true when given the player X and a board with X as diagonal winner" <|
          \_ ->
            let
              board =
                [ Occupied X, Empty, Empty
                , Empty , Occupied X, Empty
                , Empty , Empty, Occupied X
                ]
            in
              board
                |> isWinner X
                |> Expect.true "should return true"
      , test "should return false when given either player and a board with no winner" <|
          \_ ->
            let
              board =
                [ Occupied X, Occupied O, Occupied X
                , Occupied X , Occupied O, Occupied X
                , Occupied O , Occupied X, Occupied O
                ]
            in
              (isWinner O board, isWinner X board)
                |> Expect.equal (False, False)
      ]
    ]

