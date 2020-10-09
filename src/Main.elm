module Main exposing (..)

import Browser
import Html exposing (button, div, fieldset, input, text)
import Html.Attributes exposing (class, list, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, field, index, list, string)


main : Program {} Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { name : String
    , heading : String
    , subtext : String
    }


type Msg
    = Name String
    | GetRandomName
    | GotRandomName (Result Http.Error String)


init : {} -> ( Model, Cmd Msg )
init _ =
    ( { name = "", heading = getHeading "...", subtext = "" }
    , Http.get
        { url = "https://randomuser.me/api/"
        , expect = Http.expectJson GotRandomName nameDecoder
        }
    )


view : Model -> Browser.Document Msg
view model =
    { title = "Greetings - Elm Demo"
    , body =
        [ div [ class "container" ]
            [ div [ class "greeting" ]
                [ text model.heading
                ]
            , div [ class "subtext" ]
                [ text model.subtext ]
            , div [ class "inputs" ]
                [ text "My name is "
                , input [ type_ "text", onInput Name, placeholder "Enter Name", value model.name ] []
                ]
            , div
                []
                [ button [ onClick GetRandomName ] [ text "Guess Again" ]
                ]
            ]
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Name name ->
            ( { name = name, heading = getHeading name, subtext = getQuestion }, Cmd.none )

        GetRandomName ->
            getModelWithRandomName

        GotRandomName result ->
            case result of
                Err _ ->
                    ( { name = "", heading = getHeading "Umm... 😅", subtext = "I am having trouble coming up with a name!" }, Cmd.none )

                Ok text ->
                    ( { name = "", heading = getHeading text ++ "!", subtext = getQuestion }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


nameDecoder : Decoder String
nameDecoder =
    field "results" (index 0 (field "name" (field "first" string)))


getModelWithRandomName : ( Model, Cmd Msg )
getModelWithRandomName =
    ( { name = "", heading = getHeading "...", subtext = "" }
    , Http.get
        { url = "https://randomuser.me/api/"
        , expect = Http.expectJson GotRandomName nameDecoder
        }
    )


getQuestion : String
getQuestion =
    "Is that really your name?"


getHeading : String -> String
getHeading value =
    "Hello " ++ value
