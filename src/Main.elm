module Main exposing (Model, Msg, update, view, init, main)

import Html exposing (Html, div, input, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)
import Browser
import Expression
import ExpressionParser

type alias Model = {
    content: String,
    result: String
  }

init : Model
init = {
    content = "",
    result = ""
  }

type Msg = Change String

handleInput : String -> String
handleInput i =
  case ExpressionParser.run i of
    Ok e ->
      String.fromFloat <| Expression.solve e

    Err _ ->
      "Error"

update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
      { model | content = newContent, result = handleInput newContent }

view : Model -> Html Msg
view model =
  div []
    [
      input [ value model.content, onInput Change ] [],
      div [] [ text model.result ]
    ]

main : Program () Model Msg
main =
  Browser.sandbox {
    init = init,
    view = view,
    update = update
  }
