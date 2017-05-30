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
    , accessToken: Maybe AccessToken
    , userHomeId: Maybe UserHomeId
    }
type alias Setter = Model -> String -> Model
type alias AccessToken = String
type alias UserHomeId = String

setUsername : Setter
setUsername model value =
    { model | username = value }

setPassword : Setter
setPassword model value =
    { model | password = value }

type Msg = SetField Setter String
         | Login
         | LoginResult (Result Http.Error AccessToken)
         | SetUserHomeId (Result Http.Error UserHomeId)

init : ( Model, Cmd Msg )
init = ( { username = ""
         , password = ""
         , errorMessage = ""
         , accessToken = Nothing
         , userHomeId = Nothing
         }
       , Cmd.none
       )

setError : Http.Error -> Model -> ( Model, Cmd Msg )
setError err model =
  case err of
    BadUrl errMsg -> ( { model | errorMessage = ("bad url: " ++ errMsg )}, Cmd.none )
    Timeout -> ( { model | errorMessage = "timeout"}, Cmd.none )
    NetworkError -> ( { model | errorMessage = "network error"}, Cmd.none )
    BadStatus response -> ( { model | errorMessage = ("bad status: " ++ (toString response.status) ++ ", " ++ response.body) }, Cmd.none )
    BadPayload dbgMsg response ->  ( { model | errorMessage = ("bad payload: " ++ dbgMsg ++ ", " ++ (toString response.status) ++ ", " ++ response.body) }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetField setter value ->
            ( setter model value, Cmd.none )
        Login ->
            (model, (login model.username model.password))
        LoginResult result ->
            case result of
                Ok token -> ( { model | accessToken = (Just token) }, getUserHome token )
                Err err -> setError err model
        SetUserHomeId result ->
            case result of
                Ok userHomeId -> ( { model | userHomeId = (Just userHomeId) }, Cmd.none )
                Err err -> setError err model

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

login : String -> String -> Cmd Msg
login username password =
    Http.post
        "http://localhost:8081/api/auth/token"
        (stringBody "application/json" ("{ \"grant_type\": \"password\", \"username\": \"" ++ username ++ "\" , \"password\": \"" ++ password ++ "\"}"))
        (field "access_token" string)
        |> Http.send LoginResult

getUserHome : AccessToken -> Cmd Msg
getUserHome accessToken =
    Http.request
     { method = "GET"
     , headers = [ (Http.header "Cookie" ("language=de; SID=" ++ accessToken))
                 , (Http.header "Authorization" ("Bearer " ++ accessToken))
                 ]
     , url = "http://localhost:8081/api/user/current/root"
     , body = Http.emptyBody
     , expect = Http.expectJson (field "rootPlanningObjectId" string)
     , timeout = Nothing
     , withCredentials = False
     }
        |> Http.send SetUserHomeId


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
        (renderLoginOrPlaceholder model)

renderLoginOrPlaceholder : Model -> List (Html Msg)
renderLoginOrPlaceholder model =
    let
        nodeList = case model.accessToken of
            Just _ ->
                [ text "You are logged in"
                , div [] [ text (toString model.userHomeId) ]
                ]
            Nothing ->
                [ text "Login"
                , input [placeholder "Username", value model.username, onInput (SetField setUsername)] []
                , input [type_ "password", placeholder "Password", value model.password, onInput (SetField setPassword)] []
                , button [onClick Login] [ text "Login" ]
                , div [] [ text model.errorMessage ]
                ]
    in
        nodeList
