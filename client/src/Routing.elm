module Routing exposing ( home, login, auth, parse, routeToPath )

import Navigation exposing ( Location )
import UrlParser exposing (..)

import Types


home : String
home = "#"


login : String
login = "#login"


auth : String
auth = "#auth"


routeToPath : Types.Route -> String
routeToPath route =
  case route of
    Types.HomeRoute -> home
    Types.LoginRoute -> login
    Types.AuthRoute _ _ -> auth


parse : Location -> Types.Route
parse location =
  case parseHash matchers location of
    Just route -> route
    Nothing    -> Types.HomeRoute


matchers : Parser (Types.Route -> a) a
matchers =
  oneOf
    [ map Types.HomeRoute top
    , map Types.LoginRoute (s "login")
    , map Types.AuthRoute (s "auth" <?> stringParam "code" <?> stringParam "state")
    ]


-- vim: et sw=2 sts=2
