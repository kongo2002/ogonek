module Routing exposing ( home, login, logout, auth, help, parse, routeToPath, routeToName )

import Navigation exposing ( Location )
import UrlParser exposing (..)

import Types


home : String
home = "/"


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
    Types.LoginRoute -> login
    Types.LogoutRoute -> logout
    Types.AuthRoute _ _ -> auth
    Types.HelpRoute -> help


routeToName : Types.Route -> String
routeToName route =
  case route of
    Types.HomeRoute -> "Home"
    Types.LoginRoute -> "Login"
    Types.LogoutRoute -> "Logout"
    Types.AuthRoute _ _ -> "Authorization"
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
    , map Types.AuthRoute (s "auth" <?> stringParam "code" <?> stringParam "state")
    , map Types.LoginRoute (s "login")
    , map Types.LogoutRoute (s "logout")
    , map Types.HelpRoute (s "help")
    ]


-- vim: et sw=2 sts=2
