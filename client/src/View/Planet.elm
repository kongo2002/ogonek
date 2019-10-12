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


module View.Planet exposing (planet)

import Html exposing (..)
import Html.Attributes exposing (..)
import Types exposing (..)
import Utils exposing (capacityPercent, zonedIso8601)
import View.Utils exposing (..)


planet : ActivePlanet -> Model -> List (Html Msg)
planet active model =
    let
        planet0 =
            active.planet

        name =
            planetName planet0
    in
    [ h2 [] [ text name ]
    , div [ class "row" ]
        [ ul [ id "planet-description", class "no-mobile nine columns" ]
            [ li []
                [ span [ class "description" ] [ text "Type: " ]
                , span [ class "value" ] [ text <| Utils.planetToString planet0.planetType ]
                ]
            , li []
                [ span [ class "description" ] [ text "Size: " ]
                , span [ class "value" ] [ text <| String.fromInt planet0.size ]
                ]
            , li []
                [ span [ class "description" ] [ text "Position: " ]
                , span [ class "value" ] [ text <| coordStr planet0.position ]
                ]
            ]
        , div [ class "u-pull-right three columns" ]
            [ div [ class "u-full-width text-right" ]
                [ planetImg planet0
                ]
            ]
        ]
    ]



-- vim: et sw=2 sts=2
