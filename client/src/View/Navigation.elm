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


module View.Navigation exposing (navigation)

import Html exposing (..)
import Html.Attributes exposing (..)
import Routing
import Types exposing (..)
import View.Utils exposing (..)


navigation : Model -> Html Msg
navigation model =
    let
        loggedIn0 =
            loggedIn model

        loggedInRoutes =
            if loggedIn0 then
                [ Types.ResearchRoute ]

            else
                []

        routes =
            [ Types.HomeRoute ]
                ++ loggedInRoutes
                ++ [ Types.HelpRoute ]

        link args route =
            let
                ref =
                    Routing.routeToPath route

                name =
                    Routing.routeToName route

                active =
                    route == model.route

                acls =
                    if active then
                        [ class "active" ]

                    else
                        []

                clss =
                    acls ++ args
            in
            li clss [ a [ href ref ] [ text name ] ]

        loginRoute =
            if loggedIn0 then
                []

            else
                [ link [ toRight ] Types.LoginRoute ]

        userInfo =
            case model.user of
                Just user ->
                    let
                        path =
                            Routing.routeToPath Types.UserRoute

                        userIcon =
                            span [ class "spaced" ] [ icon "user-circle" ]

                        name =
                            span [ class "large-only" ] [ text user.name ]

                        link0 =
                            a [ href path ]
                                [ name, userIcon ]
                    in
                    [ li [ toRight ] [ link0 ] ]

                Nothing ->
                    []

        connection =
            [ li [ toRight ] [ connectionIcon model ] ]

        routesLinks =
            List.map (link []) routes

        links =
            routesLinks ++ loginRoute ++ connection ++ userInfo
    in
    div [ class "row" ]
        [ div [ class "four columns" ]
            [ a [ href "/", numbClick (NewUrl HomeRoute) ]
                [ h1 [] [ text "ogonek" ]
                ]
            ]
        , div [ id "nav", class "eight columns" ]
            [ ul [] links
            ]
        ]


connectionIcon : Model -> Html Msg
connectionIcon model =
    let
        status =
            case model.websocket of
                Disconnected ->
                    "disconnected"

                Connecting ->
                    "connecting"

                Connected _ ->
                    "connected"

        statusClass =
            class status

        title0 =
            "server: " ++ status
    in
    span [ class "spaced", class "websocket", statusClass, title title0 ] [ icon "plug" ]



-- vim: et sw=2 sts=2
