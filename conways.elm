import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Random
import Matrix exposing (Location)
import Matrix.Random
import Time exposing (Time)
import Color
import Element exposing (toHtml)
import Collage exposing (collage)


main : Program Never Model Msg
main =
  Html.program { init = init, view = view, subscriptions = subscriptions, update = update }


-- MODEL

boardWidth : number
boardWidth = 50
boardHeight : number
boardHeight = 50

type alias Cell = Bool

defaultCell : Cell
defaultCell = False

type alias Board = Matrix.Matrix Cell

type alias CellRendition = {color : Color.Color, size : Float}

type alias Model =
  { board : Board
  , rendition : (Matrix.Matrix CellRendition)
  }

randomMatrix : Random.Generator (Matrix.Matrix Float)
randomMatrix = Matrix.Random.matrix (Random.int boardHeight boardWidth) (Random.int boardHeight boardWidth) (Random.float 0 1)

init : (Model, Cmd Msg)
init =
  (Model (Matrix.fromList []) (Matrix.fromList []), Random.generate SetBoard randomMatrix)

-- UPDATE

bit : Maybe Cell -> number
bit c =
  if (Maybe.withDefault defaultCell c) then 1 else 0

neighborhood : Location -> List Location
neighborhood loc =
    [ Matrix.loc (Matrix.row loc) (Matrix.col loc)
    , Matrix.loc (1 + Matrix.row loc) (Matrix.col loc)
    , Matrix.loc (-1 + Matrix.row loc) (Matrix.col loc)
    , Matrix.loc (Matrix.row loc) (1 + Matrix.col loc)
    , Matrix.loc (1 + Matrix.row loc) (1 + Matrix.col loc)
    , Matrix.loc (-1 + Matrix.row loc) (1 + Matrix.col loc)
    , Matrix.loc (Matrix.row loc) (-1 + Matrix.col loc)
    , Matrix.loc (1 + Matrix.row loc) (-1 + Matrix.col loc)
    , Matrix.loc (-1 + Matrix.row loc) (-1 + Matrix.col loc)
    ]

neighborCount : Location -> Board -> number
neighborCount loc board =
  List.sum <| List.map (\n -> bit <| Matrix.get n board) (neighborhood loc)

boardStep : Board -> Board
boardStep board =
  let
    lives loc val =
      List.member (neighborCount loc board) [3,3+(bit <| Just val)]
  in
    Matrix.mapWithLocation lives board

renditionStep : Model -> Model
renditionStep model =
  let
    cap x =
      max 0 (min 1 x)

    updateCellRendition loc rend =
      {rend | size = cap (rend.size + ((Matrix.get loc model.board |> bit)*2 - 1)/10)}
  in
    {model | rendition = Matrix.mapWithLocation updateCellRendition model.rendition}

type Msg
  = Reset
  | SetBoard (Matrix.Matrix Float)
  | NextTurn
  | AnimateTick

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Reset ->
      (model, Random.generate SetBoard randomMatrix)

    SetBoard randomMatrix ->
      let
        randomMatrixToBoard m =
          Matrix.map (\f -> f > 0.7) m

        randomMatrixToRendition m =
          Matrix.map (\f -> {color = Color.hsl (f*2*pi) 0.5 0.8, size = f}) m
      in
        ({board = randomMatrixToBoard randomMatrix, rendition = randomMatrixToRendition randomMatrix}, Cmd.none)

    NextTurn ->
      ({model | board = boardStep model.board}, Cmd.none)

    AnimateTick ->
      (renditionStep model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Time.every (Time.second/10) (\t -> NextTurn)
    , Time.every (Time.second/20) (\t -> AnimateTick)
    ]


-- VIEW

view : Model -> Html Msg
view model =
  let
    makeSquare cell =
      Collage.filled (cell.color) <| Collage.square (cell.size*10)

    toSquares list =
      List.map (\(x,y,cell) -> Collage.move ((x+0.5 - boardWidth/2)*10, (y+0.5 - boardHeight/2)*10) <| makeSquare cell) list

    wrap loc val =
      (toFloat <| Matrix.col loc, toFloat <| Matrix.row loc, val)

    renderBoard board =
      collage (10*boardWidth) (10*boardHeight) (toSquares <| Matrix.flatten <| Matrix.mapWithLocation wrap board)

  in
    div []
      [ toHtml <| renderBoard model.rendition
      , button [ onClick Reset ] [ text "New Board" ]
      ]
