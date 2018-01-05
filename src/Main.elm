import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import TicTacToe.Board as Board exposing (..)



main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { board : Board
  , status : Status
  }


type Status
  = NotStarted
  | InProgress Player
  | Winner Player
  | Draw


init : (Model, Cmd Msg)
init =
  ( Model emptyBoard (InProgress X)
  , Cmd.none
  )



-- UPDATE


type Msg
  = Move Int
  | Reset


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Move index ->
      case model.status of
        InProgress player ->
          let 
            newBoard = Board.addMove player index model.board
          in
            ( Model newBoard (newStatus newBoard model.status)
            , Cmd.none
            )

        _  ->
          (model, Cmd.none)

    Reset ->
      init


newStatus : Board -> Status -> Status
newStatus board status =
  case status of
    InProgress player ->
      if Board.isWinner player board == True then
        Winner player
      else if Board.isFull board == True then
        Draw
      else InProgress (nextPlayer player)

    _ ->
      status


nextPlayer : Player -> Player
nextPlayer player =
  case player of
    X ->
      O

    O ->
      X


-- VIEW


view : Model -> Html Msg
view model =
  div [ style [ ("margin", "16px") ] ]
    [ h1 [] [ text "Tic Tac Toe" ]
    , viewBoard model.board
    , h3 [] [ text (showStatus model.status) ]
    , button [ onClick Reset ] [ text "Reset Game" ]
    ]


viewBoard : Board -> Html Msg
viewBoard board =
  div [ style boardStyle ]
    [ viewRow board 0 2
    , viewRow board 3 5
    , viewRow board 6 8
    ]


viewRow : Board -> Int -> Int -> Html Msg
viewRow board from to =
  div [ style rowStyle ]
    ( List.range from to
      |> List.map
        ( \index ->
            div
              [ style spaceStyle, onClick (Move index) ]
              [ text (Board.spaceToString index board) ]
        )
    )


showStatus : Status -> String
showStatus status =
  case status of
    NotStarted ->
      ""

    InProgress player ->
      (showPlayer player) ++ "'s turn"

    Draw ->
      "Draw"

    Winner player ->
      (showPlayer player) ++ " is the winner!"


showPlayer : Player -> String
showPlayer player =
  case player of
    X -> "X"
    O -> "O"


spaceStyle : List (String, String)
spaceStyle =
  [ ("display", "flex")
  , ("justify-content", "center")
  , ("align-items", "center")
  , ("margin", "0 2px")
  , ("background", "#99F")
  , ("width", "64px")
  , ("height", "64px")
  ]


rowStyle : List (String, String)
rowStyle =
  [ ("display", "flex")
  , ("justify-content", "flex-start")
  , ("margin", "2px")
  ]


boardStyle : List (String, String)
boardStyle =
  [ ("display", "flex")
  , ("flex-direction", "column")
  , ("justify-content", "flex-start")
  , ("margin", "16px")
  ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

