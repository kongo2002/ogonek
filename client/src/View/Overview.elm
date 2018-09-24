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

module View.Overview exposing ( overview )

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Time.DateTime

import Assets
import Routing
import Types exposing (..)
import View.Research
import View.Utils exposing (..)
import Utils exposing ( orEmpty, zonedIso8601 )


overview : Model -> List (Html Msg)
overview model =
  let planets = Dict.values model.planets
      perPlanet = planetView model
      views = List.concatMap perPlanet planets
  in  research model :: views


research : Model -> Html Msg
research model =
  let res = model.research
      link = Routing.routeToPath ResearchRoute
      click = numbClick (NewUrl ResearchRoute)
      status = View.Research.status model res
      iconPath = Assets.path Assets.physics
      icon = img [ class "planet", src iconPath ] []
      title =
        div [ class "row" ]
        [ div [ class "nine columns" ]
          [ h3 [] [ a [ href link, click ] [ text "Research" ] ]
          ]
        , div [ class "three columns" ] [ icon ]
        ]
  in  div [ class "research" ]
      [ title
      , div [ class "row" ]
        [ p [] [ text status ] ]
      ]


planetView : Model -> ActivePlanet -> List (Html Msg)
planetView model info =
  let planet = info.planet
      id = planet.id
      name = planetName planet

      toEntry (iconType, name, finish) =
        let finished = zonedIso8601 model finish
            delta = Time.DateTime.delta finish >> Utils.deltaToString
            duration = Maybe.map delta model.lastTimeStamp |> Maybe.withDefault ""
        in  tr []
            [ td [] [ icon iconType ]
            , td [] [ text name ]
            , td [] [ text duration ]
            , td [ class "no-mobile" ] [ text finished ]
            ]

      sortedConstructions =
        Dict.values info.constructions
        |> List.map (\c -> ("home", translateBuildingName c.building, c.finish))

      sortedWeaponOrders =
        Dict.values info.weaponOrders
        |> List.map (\w -> ("bolt", translateWeaponOrder w, w.finish))

      orderEntries =
        -- constructions
        sortedConstructions ++
        -- weapons
        sortedWeaponOrders
        -- ships
        -- etc

      sortedEntries =
        orderEntries
        |> List.sortBy (\(_, _, finish) -> Time.DateTime.toTimestamp finish)
        |> List.map toEntry

      orders =
        if List.isEmpty orderEntries then
          div [] [ p [] [ text "no orders" ] ]
        else
          table [ class "twelve columns" ]
          [ thead []
            [ tr []
              [ th [] []
              , th [] [ text "Name" ]
              , th [] [ text "Duration" ]
              , th [ class "no-mobile" ] [ text "Completion" ]
              ]
            ]
          , tbody [] sortedEntries
          ]

      menuIcon icon0 route name =
        let route0 = route id
            link = Routing.routeToPath route0
            desc = span [ class "mobile" ] [ text (" " ++ name) ]
        in  div [ class "three columns" ]
            [ a [ href link, numbClick (NewUrl route0), title name ]
              [ h3 [ class "text-centered" ] [ icon icon0, desc ] ]
            ]

  in  [ div [ class "row" ]
        [ div [ class "nine columns" ] [ h3 [] [ text name ] ]
        , div [ class "three columns" ]
          [ planetImg planet ]
        ]
      , div [ class "row" ]
        [ menuIcon "globe-africa" PlanetRoute "planet"
        , menuIcon "home" BuildingsRoute "buildings"
        , menuIcon "rocket" ShipsRoute "ships"
        , menuIcon "bolt" WeaponsRoute "weapons"
        ]
      , div [ class "row" ]
        [ orders
        ]
      ]


-- vim: et sw=2 sts=2
