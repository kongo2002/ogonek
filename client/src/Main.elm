module Main exposing ( main )

import Html exposing (..)


type Msg
  = NoOp


type alias Model =
  { state : List String
  }


model : Model
model = Model []


view : Model -> Html Msg
view model =
  p [] [ text "hi, from elm!" ]


update : Msg -> Model -> Model
update msg model =
  case msg of
    NoOp -> model


main : Program Never Model Msg
main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }

-- vim: et sw=2 sts=2
