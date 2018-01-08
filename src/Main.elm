import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Random
import TicTacToe.Board exposing (..)



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
  , gameType : GameType
  }


type GameType
  = NotChosen
  | TwoPlayer
  | OnePlayerNeedPlayerChoice
  | OnePlayer (PlayerPair Player Player)


type alias PlayerPair human computer = (human, computer)


type Status
  = NotStarted
  | InProgress Player
  | Winner Player
  | Draw


init : (Model, Cmd Msg)
init =
  ( Model emptyBoard (NotStarted) NotChosen
  , Cmd.none
  )


startGame : Player -> GameType -> (Model, Cmd Msg)
startGame startingPlayer gameType =
  ( Model emptyBoard (InProgress startingPlayer) gameType
  , Cmd.none
  )


randomPlayerGenerator : Random.Generator Player
randomPlayerGenerator =
    Random.bool
        |> Random.map
            (\randomBool ->
                if randomBool then
                    X
                else
                    O
            )



-- UPDATE


type Msg
  = Move Int
  | Reset
  | SetInitialPlayer Player
  | SetGameType GameType


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Move index ->
      case model.status of
        InProgress player ->
          case isSpaceEmpty index model.board of
            True ->
              let 
                board =
                    addMove player index model.board
                status =
                    newStatus board model.status
              in
                ( Model board status model.gameType
                , Cmd.none
                )

            False ->
              (model, Cmd.none)

        _  ->
          (model, Cmd.none)

    Reset ->
      init

    SetInitialPlayer player ->
      ( Model emptyBoard (InProgress player) (model.gameType)
      , Cmd.none
      )

    SetGameType gameType ->
      case gameType of
        TwoPlayer ->
          ( { model | gameType = TwoPlayer }
          , Random.generate SetInitialPlayer randomPlayerGenerator
          )

        OnePlayerNeedPlayerChoice ->
          ( { model | gameType = OnePlayerNeedPlayerChoice }
          , Cmd.none
          )

        OnePlayer _ ->
          ( { model | gameType = gameType }
          , Random.generate SetInitialPlayer randomPlayerGenerator
          )

        NotChosen ->
          (model, Cmd.none)


newStatus : Board -> Status -> Status
newStatus board status =
  case status of
    InProgress player ->
      if isWinner player board == True then
        Winner player
      else if isFull board == True then
        Draw
      else InProgress (nextPlayer player)

    _ ->
      status


nextPlayer : Player -> Player
nextPlayer player =
  case player of
    X -> O
    O -> X



-- VIEW


view : Model -> Html Msg
view model =
  div [ style [ ("margin", "16px") ] ]
    [ h1 [] [ text "Tic Tac Toe" ]
    , viewBoard model.board
    , h3 [] [ text (showStatus model.status) ]
    , button [ onClick Reset ] [ text "Reset Game" ]
    , modalFromGameType model.gameType
    ]


modalFromGameType : GameType -> Html Msg
modalFromGameType gameType =
    case gameType of
      NotChosen ->
        viewModal "How many players?"
          [ ("One Player", SetGameType OnePlayerNeedPlayerChoice)
          , ("Two Players", SetGameType TwoPlayer)
          ]

      OnePlayerNeedPlayerChoice ->
        viewModal "Choose your player: "
          [ ("X", SetGameType <| OnePlayer (X, O))
          , ("O", SetGameType <| OnePlayer (O, X))
          ]

      _ ->
        text ""


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
              [ text (spaceToString index board) ]
        )
    )


viewModal : String -> List (String, Msg) -> Html Msg
viewModal prompt pairs =
  div []
    [ text prompt
    , div [] <| List.map buttonFromTuple pairs
    ]


buttonFromTuple : (String, Msg) -> Html Msg
buttonFromTuple (buttonText, msg) =
  button [ onClick msg ] [ text buttonText ]


showStatus : Status -> String
showStatus status =
  case status of
    NotStarted ->
      "Please choose game type to start"

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



-- STYLES


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

