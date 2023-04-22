module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, option, select, text)
import Html.Attributes exposing (checked, type_, value)
import Html.Events exposing (onClick, onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type Mode
    = Program
    | Run


type alias Node =
    { enabled : Bool
    , active : Bool
    }


type alias Channel =
    { label : String
    , steps : List Node
    }


emptyNode =
    { enabled = False, active = False }


emptyChannel =
    List.repeat 16 emptyNode


type alias Model =
    { mode : Mode
    , channels : List Channel
    , steps : Int
    }


init : Model
init =
    { mode = Program
    , channels =
        [ { label = "Kick", steps = emptyChannel }
        , { label = "Snare", steps = emptyChannel }
        , { label = "Tom", steps = emptyChannel }
        , { label = "Hat", steps = emptyChannel }
        ]
    , steps = 16
    }



-- UPDATE


type Msg
    = ToggleStep String Int
    | SetSequenceLength String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleStep label index ->
            let
                newChannels =
                    List.map (\channel -> replaceChannel channel label index) model.channels
            in
            { model | channels = newChannels }

        SetSequenceLength lengthStr ->
            let
                length =
                    case String.toInt lengthStr of
                        Just intLength ->
                            intLength

                        Nothing ->
                            16

                newChannels =
                    List.map (\channel -> changeChannelLength length channel) model.channels
            in
            { model | steps = length, channels = newChannels }


changeChannelLength length channel =
    let
        currentLength =
            List.length channel.steps
    in
    if length <= currentLength then
        { channel | steps = List.take length channel.steps }

    else
        { channel | steps = List.append channel.steps (List.repeat (length - currentLength) emptyNode) }


replaceChannel channel label index =
    if channel.label == label then
        replaceSteps channel index

    else
        channel


replaceSteps channel targetIndex =
    let
        newSteps =
            channel.steps
                |> List.indexedMap Tuple.pair
                |> List.map (\step -> toggleTheStep step targetIndex)
    in
    { channel | steps = newSteps }


toggleTheStep ( index, step ) targetIndex =
    if index == targetIndex then
        { step | enabled = not step.enabled }

    else
        step



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ select [ onInput SetSequenceLength ]
            [ option [ value "1" ] [ text "1" ]
            , option [ value "2" ] [ text "2" ]
            , option [ value "3" ] [ text "3" ]
            , option [ value "4" ] [ text "4" ]
            , option [ value "5" ] [ text "5" ]
            , option [ value "6" ] [ text "6" ]
            , option [ value "7" ] [ text "7" ]
            , option [ value "8" ] [ text "8" ]
            , option [ value "9" ] [ text "9" ]
            , option [ value "10" ] [ text "10" ]
            , option [ value "11" ] [ text "11" ]
            , option [ value "12" ] [ text "12" ]
            , option [ value "13" ] [ text "13" ]
            , option [ value "14" ] [ text "14" ]
            , option [ value "15" ] [ text "15" ]
            , option [ value "16" ] [ text "16" ]
            ]
        , div [] (List.map renderChannel model.channels)
        ]


renderChannel channel =
    div []
        [ div [] [ text channel.label ]
        , div [] (renderSteps channel.label channel.steps)
        ]


renderSteps label steps =
    steps
        |> List.indexedMap Tuple.pair
        |> List.map (\step -> renderStep label step)


renderStep label ( index, step ) =
    input [ type_ "checkbox", checked step.enabled, onClick (ToggleStep label index) ] []
