module Main exposing (..)

import Json.Decode exposing (field, int, string, Decoder)
import Html exposing (text, div, input, button, p, Html)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html exposing (program)
import Http exposing (get, Error, Response, Error(..))
import Task

type alias Model =
    { username: String
    , password: String
    }
type alias Setter = Model -> String -> Model
setUsername : Setter
setUsername model value =
    { model | username = value }

type Msg = SetField Setter String

init : ( Model, Cmd Msg )
init = ( { username = "", password = ""}, Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetField setter value ->
            ( setter model value, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

main : Program Never Model Msg
main =
    program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }

view : Model -> Html Msg
view model =
    div [] 
        [ text "Login"
        , input [placeholder "Username", value model.username, onInput (SetField setUsername)] []
        , input [type_ "password", placeholder "Password"] []
        , button [] [ text "Login" ]
        ]

