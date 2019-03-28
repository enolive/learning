module Main exposing (Model, Msg(..), calculateFizzBuzz, calculateIt, divisibleBy, getValue, init, main, update, view)

import Browser
import Html exposing (Html, button, div, form, h1, img, input, text)
import Html.Attributes exposing (placeholder, src, type_, value)
import Html.Events exposing (onInput, onSubmit)
import String exposing (toInt)



---- MODEL ----


type alias Model =
    { result : String
    , input : Maybe Int
    }


init : ( Model, Cmd Msg )
init =
    ( { result = "", input = Just 0 }, Cmd.none )



---- UPDATE ----


type Msg
    = Calculate
    | Change String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Calculate ->
            ( { model | result = calculateIt model.input }, Cmd.none )

        Change input ->
            ( { model | input = toInt input }, Cmd.none )


calculateIt : Maybe Int -> String
calculateIt maybeNumber =
    case maybeNumber of
        Just number ->
            calculateFizzBuzz number

        _ ->
            ""


calculateFizzBuzz : Int -> String
calculateFizzBuzz number =
    if divisibleBy 15 number then
        "Fizz-Buzz"

    else if divisibleBy 3 number then
        "Fizz"

    else if divisibleBy 5 number then
        "Buzz"

    else
        String.fromInt number


divisibleBy : Int -> Int -> Bool
divisibleBy divisor number =
    modBy divisor number == 0



---- VIEW ----


getValue : Maybe Int -> String
getValue maybeNumber =
    maybeNumber |> Maybe.withDefault 0 |> String.fromInt


view : Model -> Html Msg
view model =
    div []
        [ viewForm model
        ]


viewForm : Model -> Html Msg
viewForm model =
    form [ onSubmit Calculate ]
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Welcome to Fizz-Buzz!" ]
        , input
            [ type_ "number"
            , placeholder "Enter a number"
            , value (model.input |> getValue)
            , onInput Change
            ]
            []
        , button [] [ text "Calculate!" ]
        , div [] [ text model.result ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
