module Main exposing (main)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Element exposing (..)
import Element.Input as Input
import Html exposing (Html)
import Html.Events exposing (onClick)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    Int


init : Model
init =
    0



-- UPDATE


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1



-- VIEW


view : Model -> Html Msg
view model =
    layout [] <|
        column [ spacing 20 ]
            [ Input.button [] { onPress = Just Decrement, label = text "-" }
            , text (String.fromInt model)
            , Input.button [] { onPress = Just Increment, label = text "+" }
            ]
