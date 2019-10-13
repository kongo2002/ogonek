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


module View.Buildings exposing (buildings)

import Const
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Routing
import Types exposing (..)
import Utils exposing (buildingLevel, capacityPercent, zonedIso8601)
import View.Utils exposing (..)


type alias ResourceRow =
    { name : ResourceType
    , resources : Int
    , capacity : Int
    , production : Int
    }


buildings : ActivePlanet -> Model -> List (Html Msg)
buildings active model =
    let
        planet =
            active.planet

        cap =
            active.capacity

        prod =
            active.production

        name =
            planetName planet

        header name0 =
            th [] [ text name0 ]

        rHeader resource =
            header (translateResource resource)

        buildings0 =
            Dict.values active.buildings

        res =
            active.resources

        maxConstr =
            maxConcurrentConstructions active

        numConstr =
            Dict.size active.constructions

        toRow =
            buildingRow model active (maxConstr > numConstr)

        desc rowDesc =
            let
                progStr =
                    capacityPercent rowDesc.resources rowDesc.capacity

                hasProduction =
                    rowDesc.production > 0

                prodStr =
                    if hasProduction then
                        String.fromInt rowDesc.production ++ " /h"

                    else
                        ""

                prodTitle =
                    if hasProduction then
                        [ title prodStr ]

                    else
                        []

                prodSpan =
                    if hasProduction then
                        [ span [ class "mobile light" ] [ text <| " (" ++ prodStr ++ ")" ] ]

                    else
                        []
            in
            div ([ class "resource four columns" ] ++ prodTitle)
                [ div [ class "meter" ]
                    [ h6 [ class "description" ]
                        ([ text (translateResource rowDesc.name)
                         , text ": "
                         , numberSpan rowDesc.resources
                         ]
                            ++ prodSpan
                        )
                    , span [ style "width" progStr ] []
                    ]
                ]

        constructionInfo =
            if maxConstr <= numConstr then
                div [ class "row cinfo" ]
                    [ p []
                        [ text ("Max. number of concurrent constructions: " ++ String.fromInt maxConstr) ]
                    ]

            else
                div [] []

        filterButton filter =
            let
                isActive =
                    active.buildingsFilter == filter

                name0 =
                    buildingFilterName filter

                click =
                    onClick (SetBuildingsFilter filter)

                cls =
                    if isActive then
                        [ class "button-primary u-full-width", click ]

                    else
                        [ class "u-full-width", click ]
            in
            div [ class "four columns" ]
                [ button cls [ text name0 ]
                ]

        buildingFilters =
            div [ class "row" ]
                [ filterButton AllBuildings
                , filterButton AvailableBuildings
                , filterButton InProgressBuildings
                ]

        filteredBuildings =
            case active.buildingsFilter of
                AllBuildings ->
                    buildings0

                AvailableBuildings ->
                    if maxConstr > numConstr then
                        let
                            possible =
                                buildPossible res
                        in
                        List.filter possible buildings0

                    else
                        []

                InProgressBuildings ->
                    let
                        inProgress b =
                            Dict.member b.name active.constructions
                    in
                    List.filter inProgress buildings0

        groupedBuildings =
            Utils.groupBy .group filteredBuildings

        groupedRows ( group, buildings1 ) =
            let
                translated =
                    translateBuildingGroup group

                header0 =
                    tr [ class "subheader" ]
                        [ td [ colspan 14 ] [ text translated ] ]

                rows =
                    List.map toRow buildings1
            in
            header0 :: rows

        energies =
            [ ResourceRow Workers res.workers 0 0
            , ResourceRow Power res.power 0 0
            ]

        resourceRow1 =
            [ ResourceRow IronOre res.ironOre cap.ironOre prod.ironOre
            , ResourceRow Gold res.gold cap.gold prod.gold
            , ResourceRow H2O res.h2o cap.h2o prod.h2o
            ]

        resourceRow2 =
            [ ResourceRow Oil res.oil cap.oil prod.oil
            , ResourceRow H2 res.h2 cap.h2 prod.h2
            , ResourceRow Uranium res.uranium cap.uranium prod.uranium
            ]

        resourceRow3 =
            [ ResourceRow PVC res.pvc cap.pvc prod.pvc
            , ResourceRow Titan res.titan cap.titan prod.titan
            , ResourceRow Kyanite res.kyanite cap.kyanite prod.kyanite
            ]

        productionLink =
            let
                route =
                    ProductionRoute planet.id

                link =
                    Routing.routeToPath route

                title0 =
                    title "Production"
            in
            span [ class "spaced", toRight, title0 ]
                [ a [ href link ] [ icon "sliders-h" ] ]
    in
    [ h2 [] [ text name ]
    , h3 [] [ text "Resources", productionLink ]
    , div [ id "resources" ]
        [ div [ class "row" ] (List.map desc energies)
        , div [ class "row" ] (List.map desc resourceRow1)
        , div [ class "row" ] (List.map desc resourceRow2)
        , div [ class "row" ] (List.map desc resourceRow3)
        ]
    , div [ class "row" ]
        [ h3 [] [ text "Buildings" ]
        , constructionInfo
        , buildingFilters
        , table [ id "buildings", class "table-responsive table-resources u-full-width" ]
            [ thead []
                [ tr []
                    [ header Const.building
                    , header Const.level
                    , rHeader Workers
                    , rHeader Power
                    , rHeader IronOre
                    , rHeader Gold
                    , rHeader H2O
                    , rHeader Oil
                    , rHeader H2
                    , rHeader Uranium
                    , rHeader PVC
                    , rHeader Titan
                    , rHeader Kyanite
                    , header ""
                    ]
                ]
            , tbody [] (Dict.toList groupedBuildings |> List.concatMap groupedRows)
            ]
        ]
    ]


buildingFilterName : BuildingsFilter -> String
buildingFilterName filter =
    case filter of
        AllBuildings ->
            "all"

        AvailableBuildings ->
            "available"

        InProgressBuildings ->
            "in progress"


maxConcurrentConstructions : ActivePlanet -> Int
maxConcurrentConstructions planet =
    let
        ccLevel =
            buildingLevel planet "construction_center"
    in
    ccLevel // 10 + 1


buildingRow : Model -> ActivePlanet -> Bool -> BuildingInfo -> Html Msg
buildingRow model planet constrPossible binfo =
    let
        col label val relative =
            let
                dataLabel =
                    attribute "data-label" label

                attrs =
                    if val == relative then
                        [ class "no-mobile", dataLabel ]

                    else
                        [ dataLabel ]
            in
            td attrs [ numberSpanTo relative val ]

        rCol resource =
            col (translateResource resource)

        res =
            planet.resources

        construction =
            Dict.get binfo.name planet.constructions

        buildColumns =
            [ rCol Workers binfo.workers res.workers
            , rCol Power binfo.power res.power
            , rCol IronOre binfo.ironOre res.ironOre
            , rCol Gold binfo.gold res.gold
            , rCol H2O binfo.h2o res.h2o
            , rCol Oil binfo.oil res.oil
            , rCol H2 binfo.h2 res.h2
            , rCol Uranium binfo.uranium res.uranium
            , rCol PVC binfo.pvc res.pvc
            , rCol Titan binfo.titan res.titan
            , rCol Kyanite binfo.kyanite res.kyanite
            ]

        columns =
            case construction of
                Just constr ->
                    [ td [ class "operations", colspan 12 ] [ constructionOperation model constr ] ]

                Nothing ->
                    buildColumns
                        ++ [ td [ class "operations" ] [ buildOperation constrPossible res binfo ] ]

        name =
            let
                route =
                    BuildingRoute binfo.name

                translated =
                    translateBuilding binfo

                link =
                    Routing.routeToPath route
            in
            a [ href link ] [ text translated ]
    in
    tr []
        ([ td [ class "header" ] [ name ]
         , col Const.level binfo.level -1
         ]
            ++ columns
        )


constructionOperation : Model -> ConstructionInfo -> Html Msg
constructionOperation model constr =
    let
        finishStr =
            zonedIso8601 model constr.finish

        finish =
            title ("finished: " ++ finishStr)

        durationDesc =
            case model.lastTimeStamp of
                Just now ->
                    "finished in " ++ Utils.deltaToString (Utils.posixDelta constr.finish now)

                Nothing ->
                    "in construction "
    in
    a [ class "icon inactive construction", finish ]
        [ span [] [ text durationDesc ]
        , span [] [ icon "gavel" ]
        ]


buildOperation : Bool -> ResourceInfo -> BuildingInfo -> Html Msg
buildOperation constrPossible res binfo =
    let
        possible =
            constrPossible && buildPossible res binfo

        buildReq =
            BuildBuildingRequest res.planetId binfo.name (binfo.level + 1)

        request =
            ApiRequest buildReq

        duration =
            Utils.deltaToString binfo.duration

        buildCls =
            if possible then
                [ href "#", numbClick request ]

            else
                [ class "inactive" ]

        desc =
            span [ class "description" ]
                [ text <| "Build (" ++ duration ++ ") " ]
    in
    a (title duration :: class "icon" :: buildCls) [ desc, icon "cog" ]


buildPossible : ResourceInfo -> BuildingInfo -> Bool
buildPossible res info =
    res.workers
        >= info.workers
        && res.power
        >= info.power
        && res.ironOre
        >= info.ironOre
        && res.gold
        >= info.gold
        && res.h2o
        >= info.h2o
        && res.oil
        >= info.oil
        && res.h2
        >= info.h2
        && res.uranium
        >= info.uranium
        && res.pvc
        >= info.pvc
        && res.titan
        >= info.titan
        && res.kyanite
        >= info.kyanite



-- vim: et sw=2 sts=2
