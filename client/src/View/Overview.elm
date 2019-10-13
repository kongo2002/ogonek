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


module View.Overview exposing (overview)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Routing
import Time
import Types exposing (..)
import Utils exposing (orEmpty, zonedIso8601)
import View.Research
import View.Utils exposing (..)


overview : Model -> List (Html Msg)
overview model =
    let
        planets =
            Dict.values model.planets

        perPlanet =
            planetView model

        views =
            List.concatMap perPlanet planets
    in
    research model :: views


research : Model -> Html Msg
research model =
    let
        res =
            model.research

        now =
            model.lastTimeStamp

        link =
            Routing.routeToPath ResearchRoute

        status =
            View.Research.status model res

        title =
            div [ class "row" ]
                [ div [ class "twelve columns" ]
                    [ h3 []
                        [ text "Research "
                        , a [ href link ]
                            [ span [ class "spaced" ] [ icon "flask" ]
                            ]
                        ]
                    ]
                ]

        content =
            case model.research.status of
                Just rstate ->
                    let
                        target =
                            View.Research.target rstate

                        finished =
                            zonedIso8601 model rstate.finish

                        duration0 =
                            now
                                |> Maybe.map (View.Research.duration rstate)
                                |> Maybe.withDefault ""
                    in
                    table [ class "twelve columns" ]
                        [ thead []
                            [ tr []
                                [ th [] []
                                , th [] [ text "Name" ]
                                , th [] [ text "Duration" ]
                                , th [ class "no-mobile" ] [ text "Completion" ]
                                ]
                            ]
                        , tbody []
                            [ tr []
                                [ td [] [ span [ class "spaced" ] [ icon "flask" ] ]
                                , td [] [ text target ]
                                , td [] [ text duration0 ]
                                , td [ class "no-mobile" ] [ text finished ]
                                ]
                            ]
                        ]

                Nothing ->
                    p [] [ text "no research in progress" ]
    in
    div [ class "research" ]
        [ title
        , div [ class "row" ] [ content ]
        ]


planetView : Model -> ActivePlanet -> List (Html Msg)
planetView model info =
    let
        planet =
            info.planet

        id =
            planet.id

        name =
            planetName planet

        toEntry ( iconType, name0, finish ) =
            let
                finished =
                    zonedIso8601 model finish

                delta =
                    Utils.posixDelta finish >> Utils.deltaToString

                duration =
                    Maybe.map delta model.lastTimeStamp |> Maybe.withDefault ""
            in
            tr []
                [ td [] [ span [ class "spaced" ] [ icon iconType ] ]
                , td [] [ text name0 ]
                , td [] [ text duration ]
                , td [ class "no-mobile" ] [ text finished ]
                ]

        sortedConstructions =
            Dict.values info.constructions
                |> List.map (\c -> ( "home", translateBuildingName c.building, c.finish ))

        sortedWeaponOrders =
            Dict.values info.weaponOrders
                |> List.map (\w -> ( "bolt", translateWeaponOrder w, w.finish ))

        orderEntries =
            -- constructions
            sortedConstructions
                ++ -- weapons
                   sortedWeaponOrders

        -- ships
        -- etc
        sortedEntries =
            orderEntries
                |> List.sortBy (\( _, _, finish ) -> Time.posixToMillis finish)
                |> List.map toEntry

        orders =
            if List.isEmpty orderEntries then
                div [] [ p [] [ text "no production orders placed" ] ]

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

        menuIcon icon0 route name0 =
            let
                route0 =
                    route id

                link =
                    Routing.routeToPath route0

                desc =
                    span [ class "mobile" ] [ text (" " ++ name0) ]
            in
            div [ class "three columns" ]
                [ a [ href link, title name0 ]
                    [ h3 [ class "text-centered" ] [ icon icon0, desc ] ]
                ]
    in
    [ div [ class "row" ]
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
