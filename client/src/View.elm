-- Copyright 2018 Gregor Uhlenheuer
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

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
          _ -> home
  in
    div [ class "container" ]
    [ navigation model
    , content model
    ]


navigation : Model -> Html Msg
navigation model =
  let loginRoute =
        case model.user of
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
        in  li clss [ a [ href ref, numbClick (NewUrl route) ] [ text name ] ]

      userInfo =
        case model.user of
          Just user -> [ li [toRight, class "username"] [ text user.name ] ]
          Nothing -> []

      routesLinks = List.map (link []) routes
      loginLink = link [toRight] loginRoute
      links = routesLinks ++ [loginLink] ++ userInfo

  in div [ class "row" ]
     [ div [ id "brand", class "four columns" ]
       [ a [ href "/", numbClick (NewUrl HomeRoute) ] [
         h1 [] [ text "ogonek" ]
         ]
       ]
     , div [ id "nav", class "eight columns" ]
       [ ul [] links
       ]
     ]


login : Model -> Html Msg
login model =
  let provider = model.authInfo.provider
      login = model.authInfo.loginUrl
  in
    div [ class "row" ]
    [ h2 [] [ text "login" ]
    , h3 [] [ text ("via " ++ provider) ]
    , p []
      [ a [ class "button button-primary", href login ] [ text "login" ]
      ]
    ]


numbClick : a -> Attribute a
numbClick msg =
  let opts = { stopPropagation = False, preventDefault = True }
  in  onWithOptions "click" opts (Json.Decode.succeed msg)


toRight : Attribute a
toRight = class "u-pull-right"


home : Model -> Html Msg
home model =
  case model.planet of
    Just planet -> homePlanet planet model
    Nothing -> noPlanet


homePlanet : ActivePlanet -> Model -> Html Msg
homePlanet active model =
  let planet = active.planet
      name = "Planet at " ++ coordStr planet.position
      header name = td [] [ text name ]
  in
    div [ class "row" ]
    [ h2 [] [ text name ]
    , h3 [] [ text "Buildings" ]
    , table [ id "buildings" ]
      [ thead []
        [ header "building"
        , header "level"
        , header "workers"
        , header "power"
        , header "iron ore"
        , header "gold"
        , header "H2O"
        , header "oil"
        , header "H2"
        , header "uranium"
        , header "PVC"
        , header "kyanite"
        -- TODO: operations column
        , header ""
        ]
      , tbody [] (List.map buildingRow active.buildings)
      ]
    ]


buildingRow : BuildingInfo -> Html Msg
buildingRow binfo =
  let col val =
      let txt = if val == 0 then "-" else toString val
      in td [] [ text txt ]
  in
    tr []
    [ td [] [ text binfo.name ]
    , col binfo.level
    , col binfo.workers
    , col binfo.power
    , col binfo.ironOre
    , col binfo.gold
    , col binfo.h2o
    , col binfo.oil
    , col binfo.h2
    , col binfo.uranium
    , col binfo.pvc
    , col binfo.kyanite
    -- TODO: operations
    , td [] []
    ]


coordStr : (Int, Int, Int) -> String
coordStr coord =
  let (x, y, z) = coord
  in  "(" ++ toString x ++ "," ++ toString y ++ "," ++ toString z ++ ")"


noPlanet : Html Msg
noPlanet =
  div [ class "row" ]
  [ p [] [ text "welcome to ogonek!" ]
  ]


-- vim: et sw=2 sts=2
