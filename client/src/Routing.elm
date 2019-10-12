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


module Routing exposing (auth, help, home, login, logout, parse, research, routeToName, routeToPath, user)

import Types
import Url
import Url.Parser exposing (..)
import Url.Parser.Query as Query


home : String
home =
    "/"


research : String
research =
    "/research"


login : String
login =
    "/login"


logout : String
logout =
    "/logout"


auth : String
auth =
    "/auth"


user : String
user =
    "/user"


help : String
help =
    "/help"


routeToPath : Types.Route -> String
routeToPath route =
    case route of
        Types.HomeRoute ->
            home

        Types.PlanetRoute planet ->
            "/planets/" ++ planet ++ "/planet"

        Types.WeaponsRoute planet ->
            "/planets/" ++ planet ++ "/weapons"

        Types.ShipsRoute planet ->
            "/planets/" ++ planet ++ "/ships"

        Types.BuildingsRoute planet ->
            "/planets/" ++ planet ++ "/buildings"

        Types.ProductionRoute planet ->
            "/planets/" ++ planet ++ "/production"

        Types.BuildingRoute building ->
            "/buildings/building/" ++ building

        Types.ResearchRoute ->
            research

        Types.LoginRoute ->
            login

        Types.LogoutRoute ->
            logout

        Types.UserRoute ->
            user

        Types.AuthRoute _ _ _ _ ->
            auth

        Types.HelpRoute ->
            help


routeToName : Types.Route -> String
routeToName route =
    case route of
        Types.HomeRoute ->
            "Home"

        Types.PlanetRoute _ ->
            "Planet"

        Types.ProductionRoute _ ->
            "Production"

        Types.BuildingsRoute _ ->
            "Buildings"

        Types.WeaponsRoute _ ->
            "Weapons"

        Types.ShipsRoute _ ->
            "Ships"

        Types.BuildingRoute _ ->
            "Building"

        Types.ResearchRoute ->
            "Research"

        Types.LoginRoute ->
            "Login"

        Types.LogoutRoute ->
            "Logout"

        Types.UserRoute ->
            "User"

        Types.AuthRoute _ _ _ _ ->
            "Authorization"

        Types.HelpRoute ->
            "Help"


parse : Url.Url -> Types.Route
parse location =
    case Url.Parser.parse matchers location of
        Just route ->
            route

        Nothing ->
            Types.HomeRoute


matchers : Parser (Types.Route -> a) a
matchers =
    oneOf
        [ map Types.HomeRoute top

        -- planet specific routes
        , map Types.PlanetRoute (s "planets" </> string </> s "planet")
        , map Types.ProductionRoute (s "planets" </> string </> s "production")
        , map Types.BuildingsRoute (s "planets" </> string </> s "buildings")
        , map Types.WeaponsRoute (s "planets" </> string </> s "weapons")
        , map Types.ShipsRoute (s "planets" </> string </> s "ships")

        -- general routes
        , map Types.ResearchRoute (s "research")
        , map Types.BuildingRoute (s "buildings" </> s "building" </> string)

        -- auth routes
        , map Types.AuthRoute (s "auth" <?> Query.string "code" <?> Query.string "state" <?> Query.string "scope" <?> Query.string "provider")
        , map Types.LoginRoute (s "login")
        , map Types.LogoutRoute (s "logout")
        , map Types.UserRoute (s "user")

        -- misc routes
        , map Types.HelpRoute (s "help")
        ]



-- vim: et sw=2 sts=2
