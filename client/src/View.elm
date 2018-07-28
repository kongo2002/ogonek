module View exposing ( view )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onClick, onWithOptions )
import Json.Decode

import Types exposing (..)
import Routing


view : Model -> Html Msg
view model =
  let content =
        case model.route of
          LoginRoute -> login
          _ -> body
  in
    div [ class "container" ]
    [ navigation model
    , content model
    ]


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
            active = route == model.route
            acls = if active then [ class "active" ] else []
            clss = acls ++ args
        in  li clss [ a [ href ref, numbClick (NewUrl ref) ] [ text name ] ]

      routesLinks = List.map (link []) routes
      loginLink = link [toRight] loginRoute
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


login : Model -> Html Msg
login model =
  let provider = model.auth.provider
      login = model.auth.loginUrl
  in
    div [ class "row" ]
    [ h1 [] [ text "login" ]
    , h3 [] [ text ("via " ++ provider) ]
    , p []
      [ a [ class "button button-primary", href login ] [ text "login" ]
      ]
    ]



numbClick msg =
  let opts = { stopPropagation = False, preventDefault = True }
  in  onWithOptions "click" opts (Json.Decode.succeed msg)


toRight = class "u-pull-right"


body : Model -> Html Msg
body model =
  div [ class "row" ]
  [ p [] [ text "welcome to ogonek!" ]
  ]


-- vim: et sw=2 sts=2
