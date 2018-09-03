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

module View.Production exposing ( production )

import Html exposing (..)
import Html.Attributes exposing (..)

import Const
import Types exposing (..)
import View.Utils exposing (..)
import Utils


production : ActivePlanet -> Model -> List (Html Msg)
production active model =
  let planet = active.planet
      prod = active.production
      cap = active.capacity
      res = active.resources
      pname = "Planet at " ++ coordStr planet.position
      light = class "light"
      hdr = class "header"

      dataLabel = attribute "data-label"

      gatherRow name production resources capacity =
        let storage = Utils.capacityPercent resources capacity
        in  tr []
            [ td [ hdr ] [ text name ]
            , td [ dataLabel "Utilization" ] [ span [ light ] [ text "100 %" ] ]
            , td [ dataLabel "Production" ] [ text (toString production), span [ light ] [ text " /h" ] ]
            , td [ dataLabel "Storage" ] [ text storage ]
            ]

      selector active resource =
        let attrs =
            if active then [ class "form-inline" ]
            else [ attribute "disabled" "", class "form-inline form-disabled" ]
        in  select attrs
            [ option [ value "100" ] [ text "100%" ]
            , option [ value "90" ] [ text "90%" ]
            , option [ value "80" ] [ text "80%" ]
            , option [ value "70" ] [ text "70%" ]
            , option [ value "60" ] [ text "60%" ]
            , option [ value "50" ] [ text "50%" ]
            , option [ value "40" ] [ text "40%" ]
            , option [ value "30" ] [ text "30%" ]
            , option [ value "20" ] [ text "20%" ]
            , option [ value "10" ] [ text "10%" ]
            , option [ value "0" ] [ text "0%" ]
            ]

      prodRow name production resources capacity =
        let storage = Utils.capacityPercent resources capacity
            hasProduction = production > 0
        in  tr []
            [ td [ hdr ] [ text name ]
            , td [ dataLabel "Utilization" ] [ selector hasProduction name ]
            , td [ dataLabel "Production" ] [ text (toString production), span [ light ] [ text " /h" ] ]
            , td [ dataLabel "Storage" ] [ text storage ]
            ]

      rows =
        [ gatherRow Const.ironOre prod.ironOre res.ironOre cap.ironOre
        , gatherRow Const.h2o prod.h2o res.h2o cap.h2o
        , gatherRow Const.gold prod.gold res.gold cap.gold
        , gatherRow Const.oil prod.oil res.oil cap.oil
        , prodRow Const.h2 prod.h2 res.h2 cap.h2
        , gatherRow Const.uranium prod.uranium res.uranium cap.uranium
        , prodRow Const.pvc prod.pvc res.pvc cap.pvc
        , prodRow Const.titan prod.titan res.titan cap.titan
        , gatherRow Const.kyanite prod.kyanite res.kyanite cap.kyanite
        ]
  in
    [ h2 [] [ text "Production" ]
    , h3 [] [ text pname ]
    , table [ class "table-responsive u-full-width" ]
      [ thead []
        [ tr []
          [ th [] [ text "Resource" ]
          , th [] [ text "Utilization" ]
          , th [] [ text "Production" ]
          , th [] [ text "Storage" ]
          ]
        ]
      , tbody [] rows
      ]
    ]

-- vim: et sw=2 sts=2
