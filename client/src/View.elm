module View exposing ( view )

import Html exposing (..)

import Types exposing (..)


view : Model -> Html Msg
view model =
  p [] [ text "hi, from elm!" ]


-- vim: et sw=2 sts=2
