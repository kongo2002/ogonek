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


module View.Building exposing (building)

import Html exposing (..)
import Html.Attributes exposing (..)
import Routing
import Types exposing (..)
import View.Utils exposing (translateBuildingName)


building : String -> Model -> List (Html Msg)
building name model =
    let
        cnt =
            content name

        translated =
            translateBuildingName name
    in
    [ h2 [] [ text "Building" ]
    , h3 [] [ text translated ]
    , div [ class "row building" ] cnt
    ]


content : String -> List (Html Msg)
content name =
    case name of
        "apartment" ->
            apartment

        "apartment_block" ->
            apartmentBlock

        "apartment_complex" ->
            apartmentComplex

        "chemical_factory" ->
            []

        "construction_center" ->
            constructionCenter

        "ext_gold_mine" ->
            []

        "ext_oil_rig" ->
            []

        "ext_ore_mine" ->
            []

        "ext_water_rig" ->
            []

        "gold_depot" ->
            goldDepot

        "gold_mine" ->
            goldMine

        "h2_depot" ->
            []

        "hydro_plant" ->
            []

        "kyanite_depot" ->
            []

        "kyanite_mine" ->
            []

        "oil_rig" ->
            oilRig

        "oil_tank" ->
            oilTank

        "ore_depot" ->
            oreDepot

        "ore_mine" ->
            oreMine

        "plastic_factory" ->
            []

        "power_plant" ->
            []

        "pvc_depot" ->
            []

        "research_lab" ->
            researchLab

        "smelting_plant" ->
            []

        "space_shipyard" ->
            []

        "titan_depot" ->
            []

        "uranium_depot" ->
            []

        "uranium_mine" ->
            []

        "water_rig" ->
            waterRig

        "water_tank" ->
            waterTank

        "wind_turbine" ->
            []

        unknown ->
            [ p [] [ text "unknown building" ] ]


apartment : List (Html Msg)
apartment =
    [ p [] [ text "The apartment is a small civilian building that provides living space for up to 10 persons." ]
    , p [] [ text "You always have to provide enough civilian buildings for your workers to live in." ]
    ]


apartmentBlock : List (Html Msg)
apartmentBlock =
    [ p [] [ text "The apartment is a medium sized civilian building that provides living space for up to 50 persons." ]
    , p [] [ text "You always have to provide enough civilian buildings for your workers to live in." ]
    ]


apartmentComplex : List (Html Msg)
apartmentComplex =
    [ p [] [ text "The apartment is a large civilian building that provides living space for up to 250 persons." ]
    , p [] [ text "You always have to provide enough civilian buildings for your workers to live in." ]
    ]


constructionCenter : List (Html Msg)
constructionCenter =
    let
        short =
            "The construction center is the base of your colony - every construction is managed in this facility."

        desc =
            "By expanding your construction center you will gain up to three concurrent construction slots - at level 15 and 50. Moreover each ten levels of your construction center you get the possibility to build one more colonization ship that has the ability to colonize additional planets."
    in
    [ p [] [ text short ]
    , p [] [ text desc ]
    ]


researchLab : List (Html Msg)
researchLab =
    [ p [] [ text "The research lab is the main research facility that allows you to discover new technologies. By issuing new research orders you can unlock various new research topics that themselves unlock new buildings and research topics in your colony. " ]
    , p [] [ text "The higher the level of your research lab, the faster a research order will be finished. As soon as half of the total research's duration you will be able to observe what the research staff is currently working on. However you have no possibility to influence what topic will be researched." ]
    ]


goldMine : List (Html Msg)
goldMine =
    [ p [] [ text "The gold mine is the primary source of the gold production of your colony. By gold being one of the four elemental materials, this building is crucial for your production and therefore your colony's overall growth." ]
    , p [] [ text "The higher your gold mine's level, the higher your gold production will be." ]
    , resources [ "gold_depot", "ext_gold_mine" ]
    ]


goldDepot : List (Html Msg)
goldDepot =
    [ p [] [ text "The gold depot increases the storage capacity of gold in the colony with each level." ]
    , resources [ "gold_mine", "ext_gold_mine" ]
    ]


oreMine : List (Html Msg)
oreMine =
    [ p [] [ text "The ore mine is the primary source of the iron ore production of your colony. By iron ore being one of the four elemental materials, this building is crucial for your production and therefore your colony's overall growth. Moreover you will require a decent iron ore supply in order to produce titan later on." ]
    , p [] [ text "The higher your iron mine's level, the higher your iron ore production will be." ]
    , resources [ "ore_depot", "ext_ore_mine", "smelting_plant" ]
    ]


oreDepot : List (Html Msg)
oreDepot =
    [ p [] [ text "The ore depot increases the storage capacity of iron ore in the colony with each level." ]
    , resources [ "ore_mine", "ext_ore_mine" ]
    ]


oilRig : List (Html Msg)
oilRig =
    [ p [] [ text "The oil rig is the primary source of the oil production of your colony. By oil being one of the four elemental materials, this building is crucial for your production and therefore your colony's overall growth. Moreover you will require a decent oil supply in order to produce PVC later on." ]
    , p [] [ text "The higher your oil rig's level, the higher your oil production will be." ]
    , resources [ "oil_tank", "ext_oil_rig", "plastic_factory" ]
    ]


oilTank : List (Html Msg)
oilTank =
    [ p [] [ text "The oil tank increases the storage capacity of oil in the colony with each level." ]
    , resources [ "oil_rig", "ext_oil_rig" ]
    ]


waterRig : List (Html Msg)
waterRig =
    [ p [] [ text "The water rig is the primary source of the water production of your colony. By water being one of the four elemental materials, this building is crucial for your production and therefore your colony's overall growth. Moreover you will require a decent water supply in order to produce hydrogen later on." ]
    , p [] [ text "The higher your water rig's level, the higher your water production will be." ]
    , resources [ "water_tank", "ext_water_rig", "chemical_factory" ]
    ]


waterTank : List (Html Msg)
waterTank =
    [ p [] [ text "The water tank increases the storage capacity of water in the colony with each level." ]
    , resources [ "water_rig", "ext_water_rig" ]
    ]


resources : List String -> Html Msg
resources rss =
    let
        buildLink name =
            let
                route =
                    BuildingRoute name

                translated =
                    translateBuildingName name

                link =
                    Routing.routeToPath route
            in
            li [ class "compact" ] [ a [ href link ] [ text translated ] ]

        elems =
            List.map buildLink rss

        header =
            h5 [] [ text "Resources" ]

        list =
            ul [] elems
    in
    div [] [ header, list ]



-- vim: et sw=2 sts=2
