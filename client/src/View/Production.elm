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


module View.Production exposing (production)

import Html exposing (..)
import Html.Attributes exposing (..)
import Types exposing (..)
import Utils
import View.Utils exposing (..)


production : ActivePlanet -> Model -> List (Html Msg)
production active model =
    let
        planet =
            active.planet

        prod =
            active.production

        cap =
            active.capacity

        util =
            active.utilization

        res =
            active.resources

        pname =
            "Planet at " ++ coordStr planet.position

        light =
            class "light"

        hdr =
            class "header"

        dataLabel =
            attribute "data-label"

        storage resources capacity =
            let
                storagePcnt =
                    Utils.capacityPercent resources capacity

                res0 =
                    numberSpan resources

                cap0 =
                    numberSpan capacity

                total =
                    span [ toRight, class "no-mobile" ] [ text " (", res0, text "/", cap0, text ")" ]
            in
            [ text storagePcnt, total ]

        gatherRow resource production0 resources capacity =
            let
                name =
                    translateResource resource

                storage0 =
                    storage resources capacity
            in
            tr []
                [ td [ hdr ] [ text name ]
                , td [ dataLabel "Utilization" ] [ span [ light ] [ text "100 %" ] ]
                , td [ dataLabel "Production" ] [ text (String.fromInt production0), span [ light ] [ text " /h" ] ]
                , td [ dataLabel "Storage" ] storage0
                ]

        selector resource active0 utilization =
            let
                selected =
                    String.fromInt utilization

                action =
                    onChangeInt (SetUtilizationRequest planet.id resource >> ApiRequest)

                attrs =
                    if active0 then
                        [ action, class "form-inline" ]

                    else
                        [ action, attribute "disabled" "", class "form-inline form-disabled" ]

                opt value0 =
                    if value0 == selected then
                        [ value value0, attribute "selected" "" ]

                    else
                        [ value value0 ]
            in
            select attrs
                [ option (opt "100") [ text "100%" ]
                , option (opt "90") [ text "90%" ]
                , option (opt "80") [ text "80%" ]
                , option (opt "70") [ text "70%" ]
                , option (opt "60") [ text "60%" ]
                , option (opt "50") [ text "50%" ]
                , option (opt "40") [ text "40%" ]
                , option (opt "30") [ text "30%" ]
                , option (opt "20") [ text "20%" ]
                , option (opt "10") [ text "10%" ]
                , option (opt "0") [ text "0%" ]
                ]

        prodRow resource production0 resources capacity utilization =
            let
                storage0 =
                    storage resources capacity

                name =
                    translateResource resource

                hasProduction =
                    production0 >= 0
            in
            tr []
                [ td [ hdr ] [ text name ]
                , td [ dataLabel "Utilization" ] [ selector resource hasProduction utilization ]
                , td [ dataLabel "Production" ] [ text (String.fromInt production0), span [ light ] [ text " /h" ] ]
                , td [ dataLabel "Storage" ] storage0
                ]

        rows =
            [ gatherRow IronOre prod.ironOre res.ironOre cap.ironOre
            , gatherRow H2O prod.h2o res.h2o cap.h2o
            , gatherRow Gold prod.gold res.gold cap.gold
            , gatherRow Oil prod.oil res.oil cap.oil
            , prodRow H2 prod.h2 res.h2 cap.h2 util.h2
            , gatherRow Uranium prod.uranium res.uranium cap.uranium
            , prodRow PVC prod.pvc res.pvc cap.pvc util.pvc
            , prodRow Titan prod.titan res.titan cap.titan util.titan
            , gatherRow Kyanite prod.kyanite res.kyanite cap.kyanite
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
