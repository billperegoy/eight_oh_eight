module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (checked, type_)
import Html.Events exposing (onClick)



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


type alias User =
    { name : String
    , age : Int
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


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleStep label index ->
            let
                newChannels =
                    List.map (\channel -> replaceChannel channel label index) model.channels
            in
            { model | channels = newChannels }


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
        [ div [] (List.map renderChannel model.channels)
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
