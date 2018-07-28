module Types exposing (..)

import Navigation exposing ( Location )


type Msg
  = NoOp
  -- internals
  | NavigationChange Location
  | NewUrl String
  -- API
  | ApiResponseError String
  | ApiResponse ApiContent
  | ApiRequest String


type Route
  = HomeRoute
  | LoginRoute
  | LogoutRoute
  -- auth (code, state)
  | AuthRoute (Maybe String) (Maybe String)
  | HelpRoute


type ApiContent
  = Auth AuthInformation
  | Error ApiError


type alias AuthInformation =
  { provider : String
  , loginUrl : String
  }


type alias ApiError =
  { error : Bool
  , message: String
  }


type alias LoginInfo =
  { id : String
  , email : String
  }


type alias Flags =
  { websocketHost : String
  }


type alias Model =
  { route : Route
  , login : Maybe LoginInfo
  , auth : AuthInformation
  , websocketHost : String
  }


-- vim: et sw=2 sts=2
