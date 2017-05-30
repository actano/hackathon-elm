module Main exposing (..)

import Json.Decode exposing (null, field, int, string, Decoder)
import Html exposing (text, div, input, button, p, Html)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html exposing (program)
import Http exposing (get, stringBody, Error, Response, Error(..))
import Task

type alias Model =
    { username: String
    , password: String
    , errorMessage: String
    }
type alias Setter = Model -> String -> Model

setUsername : Setter
setUsername model value =
    { model | username = value }

setPassword : Setter
setPassword model value =
    { model | password = value }

type Msg = SetField Setter String
         | Login
         | LoginResult (Result Http.Error ())

init : ( Model, Cmd Msg )
init = ( { username = ""
         , password = ""
         , errorMessage = ""
         }
       , Cmd.none
       )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetField setter value ->
            ( setter model value, Cmd.none )
        Login ->
            (model, (login model.username model.password))
        LoginResult result ->
            case result of
                Ok () -> ( model, Cmd.none )
                Err err ->
                  case err of
                    BadUrl errMsg -> ( { model | errorMessage = ("bad url: " ++ errMsg )}, Cmd.none )
                    Timeout -> ( { model | errorMessage = "timeout"}, Cmd.none )
                    NetworkError -> ( { model | errorMessage = "network error"}, Cmd.none )
                    BadStatus response -> ( { model | errorMessage = ("bad status: " ++ (toString response.status) ++ ", " ++ response.body) }, Cmd.none )
                    BadPayload dbgMsg response ->  ( { model | errorMessage = ("bad payload: " ++ dbgMsg ++ ", " ++ (toString response.status) ++ ", " ++ response.body) }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

login : String -> String -> Cmd Msg
login username password =
    Http.post
        "http://localhost:8081/api/auth/token"
        (stringBody "application/json" ("{ \"grant_type\": \"password\", \"username\": \"" ++ username ++ "\" , \"password\": \"" ++ password ++ "\"}"))
        (null ())
        |> Http.send LoginResult

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
        , input [type_ "password", placeholder "Password", value model.password, onInput (SetField setPassword)] []
        , div [] [ text model.errorMessage ]
        , button [onClick Login] [ text "Login" ]
        ]

