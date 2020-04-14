module Main exposing (Model, Msg, update, view, init)

import Html exposing (..)
import Browser


type alias Model = {}

init : Model
init = {}

type Msg = Msg1 | Msg2

update : Msg -> Model -> Model
update msg model =
  case msg of
    Msg1 ->
      model

    Msg2 ->
      model


view : Model -> Html Msg
view model =
  div []
    [ text "New Sandbox" ]

main : Program () Model Msg
main =
    Browser.sandbox {
        init = init,
        view = view,
        update = update
    }
