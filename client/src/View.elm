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

import Dict
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
      rows = navigation model :: content model
  in
    div [ class "container" ] rows


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


login : Model -> List (Html Msg)
login model =
  let fromAuth auth =
        let provider = auth.provider
            login = auth.loginUrl
        in div [ class "row" ]
           [ h3 [] [ text ("via " ++ provider) ]
           , p []
             [ a [ class "button button-primary", href login ] [ text "login" ]
             ]
           ]
      providers = List.map fromAuth model.authInfo
  in [ div [ class "row" ]
       [ h2 [] [ text "login" ] ]
     ] ++ providers


numbClick : a -> Attribute a
numbClick msg =
  let opts = { stopPropagation = False, preventDefault = True }
  in  onWithOptions "click" opts (Json.Decode.succeed msg)


toRight : Attribute a
toRight = class "u-pull-right"


home : Model -> List (Html Msg)
home model =
  case model.planet of
    Just planet -> homePlanet planet model
    Nothing -> noPlanet


homePlanet : ActivePlanet -> Model -> List (Html Msg)
homePlanet active model =
  let planet = active.planet
      name = "Planet at " ++ coordStr planet.position
      header name = th [] [ text name ]
      buildings = Dict.values active.buildings
      res = active.resources
      toRow = buildingRow res
      desc (name, value) =
        li []
          [ span [ class "resource" ] [ text name, text ": " ]
          , numberSpan value
          ]
      resources =
        [ ("workers", res.workers)
        , ("power", res.power)
        , ("iron ore", res.ironOre)
        , ("gold", res.gold)
        , ("H2O", res.h2o)
        , ("oil", res.oil)
        , ("H2", res.h2)
        , ("uranium", res.uranium)
        , ("pvc", res.pvc)
        , ("kyanite", res.kyanite)
        ]
  in
    [ div [ class "row" ]
      [ h2 [] [ text name ] ]
    , div [ class "row" ]
      [ h3 [] [ text "Resources" ]
      , div [ id "resources" ]
        [ ul [ class "inline" ] (List.map desc resources)
        ]
      ]
    , div [ class "row" ]
      [ h3 [] [ text "Buildings" ]
      , table [ id "buildings" ]
        [ thead []
          [ tr []
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
            , header ""
            ]
          ]
        , tbody [] (List.map toRow buildings)
        ]
      ]
    ]


numberSpan : Int -> Html Msg
numberSpan = numberSpanTo -1


numberSpanTo : Int -> Int -> Html Msg
numberSpanTo relativeTo value =
  let negative = value < 0
      range =
        if relativeTo < 0 then
          "zero"
        else if value > relativeTo then
          "negative"
        else if value < relativeTo then
          "positive"
        else
          "zero"
      strValue = toString value
      trimmed = if negative then String.dropLeft 1 strValue else strValue
      splitted = String.join "," (splitThousands trimmed)
      result = if negative then "-" ++ splitted else splitted
  in span [ class ("number " ++ range) ] [ text result ]


splitThousands : String -> List String
splitThousands integers =
    let reversedSplit value =
          if String.length value > 3 then
            value
            |> String.dropRight 3
            |> reversedSplit
            |> (::) (String.right 3 value)
          else
            [ value ]
    in integers |> reversedSplit |> List.reverse


buildingRow : ResourceInfo -> BuildingInfo -> Html Msg
buildingRow res binfo =
  let col label val relative = td [ attribute "data-label" label ] [ numberSpanTo relative val ]
      possible = buildPossible res binfo
      buildReq = BuildBuildingRequest res.planetId binfo.name (binfo.level + 1)
      request = ApiRequest buildReq
      buildCls =
        if possible then [ href "#", numbClick request ]
        else [ class "inactive" ]
      desc = span [ class "description" ] [ text "Build " ]
      build = a (class "icon" :: buildCls) [ desc, icon "cog" ]
      ops = [ build ]
  in
    tr []
    [ td [ class "building" ] [ text (translateBuilding binfo) ]
    , col "level" binfo.level -1
    , col "workers" binfo.workers res.workers
    , col "power" binfo.power res.power
    , col "iron ore" binfo.ironOre res.ironOre
    , col "gold" binfo.gold res.gold
    , col "H2O" binfo.h2o res.h2o
    , col "oil" binfo.oil res.oil
    , col "H2" binfo.h2 res.h2
    , col "uranium" binfo.uranium res.uranium
    , col "PVC" binfo.pvc res.pvc
    , col "kyanite" binfo.kyanite res.kyanite
    , td [ class "operations" ] ops
    ]


buildPossible : ResourceInfo -> BuildingInfo -> Bool
buildPossible res info =
  res.workers >= info.workers &&
  res.power >= info.power &&
  res.ironOre >= info.ironOre &&
  res.gold >= info.gold &&
  res.h2o >= info.h2o &&
  res.oil >= info.oil &&
  res.h2 >= info.h2 &&
  res.uranium >= info.uranium &&
  res.pvc >= info.pvc &&
  res.kyanite >= info.kyanite


icon : String -> Html Msg
icon name =
  let clazz = "fas fa-" ++ name
  in  i [ class clazz ] []


translateBuilding : BuildingInfo -> String
translateBuilding binfo =
  case binfo.name of
    "construction_center" -> "construction center"
    "research_lab" -> "research lab"
    "oil_rig" -> "oil rig"
    "water_rig" -> "water rig"
    "ore_mine" -> "ore mine"
    "gold_mine" -> "gold mine"
    "oil_tank" -> "oil tank"
    "water_tank" -> "water tank"
    "ore_depot" -> "ore depot"
    "gold_depot" -> "gold depot"
    "power_plant" -> "power plant"
    "wind_turbine" -> "wind turbine"
    "apartment" -> "apartment"
    "apartment_block" -> "apartment block"
    unknown -> unknown


coordStr : (Int, Int, Int) -> String
coordStr coord =
  let (x, y, z) = coord
  in  "(" ++ toString x ++ "," ++ toString y ++ "," ++ toString z ++ ")"


noPlanet : List (Html Msg)
noPlanet =
  [ div [ class "row" ]
    [ p [] [ text "welcome to ogonek!" ]
    ]
  ]


-- vim: et sw=2 sts=2
