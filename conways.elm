import Html exposing (Html, button, div, text, h2, img)
-- import Http
-- import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
-- import Json.Decode as Decode
import Random


main : Program Never Model Msg
main =
  Html.program { init = init, view = view, subscriptions = subscriptions, update = update }


-- MODEL

type alias Model =
  { board : List (List Bool)
  }

init : (Model, Cmd Msg)
init =
  (Model (List.repeat 20 (List.repeat 100 False)), Cmd.none)


-- UPDATE

type Msg
  = Reset
  | SetBoard Bool

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Reset ->
      (model, Random.generate SetBoard Random.bool)

    SetBoard newVal ->
      ({model | board = (List.repeat 20 (List.repeat 100 newVal))}, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW

view : Model -> Html Msg
view model =
  let
    boardstring boardline =
      String.join "" (List.map (\b -> if b then "#" else ".") boardline)

    boardstrings board =
      List.map (\line -> div [] [text (boardstring line)]) board

  in
    div [] ( boardstrings model.board ++ [ button [ onClick Reset ] [ text "New Board" ] ] )


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
