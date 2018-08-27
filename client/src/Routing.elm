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

module Routing exposing ( home, research, login, logout, auth, help, parse, routeToPath, routeToName )

import Navigation exposing ( Location )
import UrlParser exposing (..)

import Types


home : String
home = "/"


research : String
research = "/research"


login : String
login = "/login"


logout : String
logout = "/logout"


auth : String
auth = "/auth"


help : String
help = "/help"


routeToPath : Types.Route -> String
routeToPath route =
  case route of
    Types.HomeRoute -> home
    Types.PlanetRoute planet -> "/planets/planet/" ++ planet
    Types.ResearchRoute -> research
    Types.LoginRoute -> login
    Types.LogoutRoute -> logout
    Types.AuthRoute _ _ _ _ -> auth
    Types.HelpRoute -> help


routeToName : Types.Route -> String
routeToName route =
  case route of
    Types.HomeRoute -> "Home"
    Types.PlanetRoute _ -> "Planet"
    Types.ResearchRoute -> "Research"
    Types.LoginRoute -> "Login"
    Types.LogoutRoute -> "Logout"
    Types.AuthRoute _ _ _ _ -> "Authorization"
    Types.HelpRoute -> "Help"


parse : Location -> Types.Route
parse location =
  case parsePath matchers location of
    Just route -> route
    Nothing    -> Types.HomeRoute


matchers : Parser (Types.Route -> a) a
matchers =
  oneOf
    [ map Types.HomeRoute top
    , map Types.PlanetRoute (s "planets" </> s "planet" </> string)
    , map Types.ResearchRoute (s "research")
    , map Types.AuthRoute (s "auth" <?> stringParam "code" <?> stringParam "state" <?> stringParam "scope" <?> stringParam "provider")
    , map Types.LoginRoute (s "login")
    , map Types.LogoutRoute (s "logout")
    , map Types.HelpRoute (s "help")
    ]


-- vim: et sw=2 sts=2
