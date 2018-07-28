module View exposing ( view )

import Html exposing (..)
import Html.Attributes exposing (..)

import Types exposing (..)


view : Model -> Html Msg
view model =
  div [ class "container" ]
  [ navigation model
  , body model
  ]


navigation : Model -> Html Msg
navigation model =
  let link ref name =
        li [] [ a [ href ref ] [ text name ] ]
      login = link model.auth.loginUrl "login"
  in div [ class "row" ]
     [ div [ id "brand", class "four columns" ]
       [ h1 [] [ text "ogonek" ] ]
     , div [ id "nav", class "eight columns" ]
       [ ul []
         [ login
         , link "#help" "help"
         ]
       ]
     ]


body : Model -> Html Msg
body model =
  div [ class "row" ]
  [ p [] [ text "welcome to ogonek!" ]
  ]


-- vim: et sw=2 sts=2
