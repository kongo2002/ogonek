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
import Time.Iso8601

import Routing
import Types exposing (..)
import View.Research
import View.Utils exposing (..)


overview : Model -> List (Html Msg)
overview model =
  let planets = Dict.values model.planets
      views = List.concatMap planetView planets
  in  research model.research :: views


research : ResearchInfo -> Html Msg
research res =
  let link = Routing.routeToPath ResearchRoute
      click = numbClick (NewUrl ResearchRoute)
      status = View.Research.researchStatus res
  in  div [ class "research" ]
      [ h3 [] [ a [ href link, class "no-deco", click ] [ text "Research" ] ]
      , div [ class "row" ]
        [ p [] [ text status ] ]
      ]


planetView : ActivePlanet -> List (Html Msg)
planetView info =
  let planet = info.planet
      id = planet.id
      route = PlanetRoute id
      link = Routing.routeToPath route
      name = "Planet at " ++ coordStr planet.position
      title =
        h3 [] [ a [ href link, class "no-deco", numbClick (NewUrl route) ] [ text name ] ]

      construction constr =
        let name = translateBuildingName constr.building
            finished = Time.Iso8601.fromDateTime constr.finish
        in  tr []
            [ td [] [ text name ]
            , td [] [ text finished ]
            ]

      constructions =
        if Dict.isEmpty info.constructions then
          div [] [ p [] [ text "no constructions" ] ]
        else
          table [ class "eight columns" ]
          [ thead []
            [ tr []
              [ th [] [ text "Name" ]
              , th [] [ text "Completion" ]
              ]
            ]
          , tbody [] (Dict.values info.constructions |> List.map construction)
          ]

  in  [ div [ class "row" ]
        [ div [ class "nine columns" ] [ title ]
        , div [ class "three columns" ]
          [ planetImg planet ]
        ]
      , div [ class "row" ]
        [ -- constructions
          constructions
          -- weapons
          -- ships
          -- etc
        ]
      ]


-- vim: et sw=2 sts=2
