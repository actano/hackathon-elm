import Html exposing (Html, input, text, div, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

main =
  Html.beginnerProgram { model = model, view = view, update = update }

-- MODEL
type alias Model =
    { content: String
    , foo: Maybe String
    }

model : Model
model =
    { content = ""
    , foo = Nothing
    }


-- UPDATE
type Msg = UpdateText String
    | SetFoo

update : Msg -> Model -> Model
update msg model =
  case msg of
    UpdateText text ->
        { model | content = text}
    SetFoo ->
        { model | foo = Just "bar"}


-- VIEW

view : Model -> Html Msg
view model =
    let
        fooText = case model.foo of
            Just t -> t
            Nothing -> "placeholder"
    in
  div []
    [ input [ placeholder "Type word", onInput UpdateText, value model.content ] []
    , div [] [ text ((String.toUpper (String.reverse model.content)) ++ " --- " ++ fooText) ]
    , button [ onClick SetFoo ] [ text "Set foo"]
    ]
