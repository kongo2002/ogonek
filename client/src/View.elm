module View exposing ( view )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onClick, onWithOptions )
import Json.Decode

import Types exposing (..)


view : Model -> Html Msg
view model =
  div [ class "container" ]
  [ navigation model
  , body model
  ]


numbClick msg =
  let opts = { stopPropagation = False, preventDefault = True }
  in  onWithOptions "click" opts (Json.Decode.succeed msg)


navigation : Model -> Html Msg
navigation model =
  let link ref name =
        li [] [ a [ href ref, numbClick (NewUrl ref) ] [ text name ] ]
      login = link model.auth.loginUrl "login"
  in div [ class "row" ]
     [ div [ id "brand", class "four columns" ]
       [ a [ href "/", numbClick (NewUrl "/") ] [
         h1 [] [ text "ogonek" ]
         ]
       ]
     , div [ id "nav", class "eight columns" ]
       [ ul []
         [ login
         , link "/help" "help"
         ]
       ]
     ]


body : Model -> Html Msg
body model =
  div [ class "row" ]
  [ p [] [ text "welcome to ogonek!" ]
  ]


-- vim: et sw=2 sts=2
