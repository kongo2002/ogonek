module View exposing ( view )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onClick, onWithOptions )
import Json.Decode

import Types exposing (..)
import Routing


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
  let loginRoute =
        case model.login of
          Just _ -> Types.LogoutRoute
          Nothing -> Types.LoginRoute

      routes =
        [ Types.HomeRoute
        , Types.HelpRoute
        ]

      link args route =
        let ref  = Routing.routeToPath route
            name = Routing.routeToName route
        in  li args [ a [ href ref, numbClick (NewUrl ref) ] [ text name ] ]

      routesLinks = List.map (link []) routes
      loginLink = link [class "u-pull-right"] loginRoute
      links = routesLinks ++ [loginLink]

  in div [ class "row" ]
     [ div [ id "brand", class "four columns" ]
       [ a [ href "/", numbClick (NewUrl "/") ] [
         h1 [] [ text "ogonek" ]
         ]
       ]
     , div [ id "nav", class "eight columns" ]
       [ ul [] links
       ]
     ]


body : Model -> Html Msg
body model =
  div [ class "row" ]
  [ p [] [ text "welcome to ogonek!" ]
  ]


-- vim: et sw=2 sts=2
