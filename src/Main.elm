module Main exposing (Model, Msg, update, view, init, main)

import Html exposing (Html, div, input, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)
import Browser
import Parser
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
  let
    toString : Parser.DeadEnd -> String
    toString x = "Error at position: " ++ String.fromInt x.col
  in
  case ExpressionParser.run i of
    Ok e ->
      e
      |> Expression.solve
      |> String.fromFloat

    Err dl ->
      dl
      |> List.map toString
      |> List.foldl (\x y -> x ++ "\n" ++ y) ""

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
