module Main exposing (main)

import Browser
import Browser.Events
import Cmd.Extra exposing (withNoCmd)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import Json.Decode as Decode exposing (Decoder)
import Material.Icons.Round as Icons
import Material.Icons.Types exposing (Coloring(..), Icon)
import SyntaxHighlight exposing (elm, gitHub, noLang, toBlockHtml, useTheme)


type alias Model =
    { previusSlides : List (Element Msg)
    , currentSlide : Element Msg
    , nextSlides : List (Element Msg)
    }


type Msg
    = GotoNextSlide
    | GotoPreviusSlide
    | NoOp



-- TODO
-- custom theme input
-- slides in slides


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


init : () -> ( Model, Cmd Msg )
init _ =
    { previusSlides = []
    , currentSlide = List.head slides |> Maybe.withDefault none
    , nextSlides = List.tail slides |> Maybe.withDefault []
    }
        |> (update GotoNextSlide >> Tuple.first)
        |> (update GotoNextSlide >> Tuple.first)
        |> (update GotoNextSlide >> Tuple.first)
        |> (update GotoNextSlide >> Tuple.first)
        |> (update GotoNextSlide >> Tuple.first)
        |> (update GotoNextSlide >> Tuple.first)
        |> withNoCmd


slides : List (Element msg)
slides =
    [ column [] [ text "hi", text "ich heiße janek", el [ height <| px 16 ] <| none, text "dieser talk ist über elm" ]
    , column [ spacing 16 ]
        [ text "Was gibt dieser Code aus?"
        , codeBlock noLang "print(x)"
        ]
    , column [ spacing 16 ]
        [ text "Was gibt dieser Code aus?"
        , codeBlock noLang "print(x)"
        , codeBlock noLang """Traceback (most recent call last):
  File "main.py", line 1, in <module>
    print(x)
          ^
NameError: name 'x' is not defined"""
        ]
    , column [ spacing 16 ]
        [ text "Was gibt dieser Code aus?"
        , codeBlock noLang """x = 42
print(x)"""
        ]
    , column [ spacing 16 ]
        [ text "Was gibt dieser Code aus?"
        , codeBlock noLang """x = 42
print(x)"""
        , codeBlock noLang """
main.c:1:1: warning: data definition has no type or storage class 
1 | x = 42
  | ^
main.c:2:1: error: expected ‘,’ or ‘;’ before ‘print’
2 | print(x)
  | ^~~~~"""
        ]
    , column [ width (fill |> maximum 600), spacing 32 ] [ paragraph [] [ text "Jetzt sagt ihr das ist total unrealistisch. Aber warum? Warum passieren uns nicht solche Fehler jeden Tag?" ], text "Wir bekommen den Fehler früh genug mit." ]
    , column [ width (fill |> minimum 600) ]
        [ row [ spacing 16, width fill ]
            [ row [ width fill ]
                [ el [ height (px 1), width fill, Background.color (rgb 0 0 0) ] <| none
                , el [ moveLeft 12 ] <| materialIcon Icons.arrow_right
                ]
            , text "Zeit"
            ]
        , row [ spacing 48 ]
            [ text "t0"
            , text "1s"
            , text "1m"
            , text "1h"
            , text "1w"
            , text "1mon"
            , text "1j"
            ]
        ]

    {-
               Wann kriegen wir welche Fehler mit?
       - syntax fehler
       - sematische fehler
       - laufzeitfehler
       - null pointer exception
       - memory leak
       - logik bug
    -}
    , column [] [ el [ Font.size 32, Font.bold ] <| text "elm" ]
    , codeBlock elm """
module Main exposing (..)

type alias Model =
    { previusSlides : List (Element Msg)
    , currentSlide : Element Msg
    , nextSlides : List (Element Msg)
    }


type Msg
    = GotoNextSlide
    | GotoPreviusSlide
    | NoOp
 
"""
    ]


codeBlock : (String -> Result x SyntaxHighlight.HCode) -> String -> Element msg
codeBlock lang code =
    el [ paddingXY 32 8, Border.rounded 16, Border.width 2, Border.color (rgba 0 0 0 0.2), width fill ] <| (lang code |> Result.map (toBlockHtml (Just 1)) |> Result.map (\x -> html (Html.div [] [ useTheme gitHub, x ])) |> Result.withDefault (text code))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (case msg of
        NoOp ->
            model

        GotoNextSlide ->
            case ( List.head model.nextSlides, List.tail model.nextSlides ) of
                ( Just head, Just tail ) ->
                    { model | previusSlides = model.currentSlide :: model.previusSlides, currentSlide = head, nextSlides = tail }

                _ ->
                    model

        GotoPreviusSlide ->
            case ( List.head model.previusSlides, List.tail model.previusSlides ) of
                ( Just last, Just frontTail ) ->
                    { model | previusSlides = frontTail, currentSlide = last, nextSlides = model.currentSlide :: model.nextSlides }

                _ ->
                    model
    )
        |> withNoCmd


view : Model -> Html.Html Msg
view model =
    layout [ monospace, height fill, width fill ] <|
        column [ height fill, width fill ]
            [ html <| ptMonoLink
            , html <| useTheme gitHub
            , el [ centerX, centerY ] <| model.currentSlide
            , row [ alignBottom, centerX ]
                [ Input.button
                    []
                    { onPress = Just GotoPreviusSlide, label = materialIcon Icons.skip_previous }
                , text <| String.fromInt <| (List.length model.previusSlides + 1)
                , text "/"
                , text <| String.fromInt <| (List.length model.nextSlides + List.length model.previusSlides + 1)
                , Input.button
                    []
                    { onPress = Just GotoNextSlide, label = materialIcon Icons.skip_next }
                ]
            ]


materialIcon : Icon msg -> Element msg
materialIcon icon =
    el [] <| html <| icon 24 Inherit


monospace =
    Font.family [ Font.typeface "PT Mono", Font.monospace ]


ptMonoLink : Html.Html msg
ptMonoLink =
    Html.node "link"
        [ Html.Attributes.href "https://fonts.googleapis.com/css2?family=PT+Mono&display=swap"
        , Html.Attributes.rel "stylesheet"
        ]
        []


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ keyDecoder |> Decode.map keyToMsg |> Browser.Events.onKeyDown

        -- , keyDecoder |> Decode.map keyToMsg |> Browser.Events.onKeyUp
        ]


keyDecoder : Decoder String
keyDecoder =
    Decode.field "key" Decode.string


keyToMsg : String -> Msg
keyToMsg key =
    case key of
        "Enter" ->
            GotoNextSlide

        " " ->
            GotoNextSlide

        "ArrowRight" ->
            GotoNextSlide

        "ArrowLeft" ->
            GotoPreviusSlide

        _ ->
            NoOp
