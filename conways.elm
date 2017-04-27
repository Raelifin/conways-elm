import Html exposing (Html, button, div, text, h2, img)
-- import Http
-- import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
-- import Json.Decode as Decode
import Random
import Matrix exposing (Location)
import Matrix.Random


main : Program Never Model Msg
main =
  Html.program { init = init, view = view, subscriptions = subscriptions, update = update }


-- MODEL

type alias Board = Matrix.Matrix Bool

type alias Model =
  { board : Board
  }

init : (Model, Cmd Msg)
init =
  (Model (Matrix.matrix 20 100 (\loc -> False)), Cmd.none)

-- UPDATE

bit : Maybe Bool -> Int
bit b =
  if Maybe.withDefault False b then 1 else 0

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

neighborCount : Location -> Board -> Int
neighborCount loc board =
  List.sum <| List.map (\n -> bit <| Matrix.get n board) (neighborhood loc)

step : Board -> Board
step board =
  let
    lives loc val =
      List.member (neighborCount loc board) [3,3+(bit <| Just val)]

  in
    Matrix.mapWithLocation lives board

type Msg
  = Reset
  | SetBoard Board
  | Step

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Reset ->
      let
        randomMatrix = Matrix.Random.matrix (Random.int 100 100) (Random.int 20 20) (Random.bool)
      in
        (model, Random.generate SetBoard randomMatrix)

    SetBoard newBoard ->
      ({model | board = newBoard}, Cmd.none)

    Step ->
      ({model | board = step model.board}, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW

view : Model -> Html Msg
view model =
  let
    boardstring boardline =
      String.join "" (List.map (\b -> if b then "X" else "_") boardline)

    boardstrings board =
      List.map (\line -> div [] [text (boardstring line)]) board

  in
    div [] ( boardstrings (Matrix.toList model.board)
    ++ [ button [ onClick Reset ] [ text "New Board" ]
       , button [ onClick Step ] [text "Step"]
       ]
    )


-- HTTP
{--
getRandomGif : String -> Cmd Msg
getRandomGif topic =
  let
    url =
      "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic
  in
    Http.send NewGif (Http.get url decodeGifUrl)


decodeGifUrl : Decode.Decoder String
decodeGifUrl =
  Decode.at ["data", "image_url"] Decode.string
--}
