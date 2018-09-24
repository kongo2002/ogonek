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

module View.Ships exposing ( ships )

import Html exposing (..)
import Html.Attributes exposing (..)

import Types exposing (..)
import View.Utils exposing (..)


ships : ActivePlanet -> Model -> List (Html Msg)
ships active model =
  let planet = active.planet
      name = planetName planet
  in
    [ h2 [] [ text name ]
    , h3 [] [ text "Ships" ]
    , div [ class "row" ]
      [ p [] [ text "ship production is not possible yet" ]
      ]
    ]


-- vim: et sw=2 sts=2
