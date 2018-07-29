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
  | ApiRequest Request


type Route
  = HomeRoute
  | LoginRoute
  | LogoutRoute
  -- auth (code, state, scope)
  | AuthRoute (Maybe String) (Maybe String) (Maybe String)
  | HelpRoute


type ApiContent
  = Auth AuthInformation
  | User UserInfo
  | Error ApiError


type Request
  = AuthorizeRequest Authorize


type alias AuthInformation =
  { provider : String
  , loginUrl : String
  }


type alias Authorize =
  { code : String
  , state : String
  , scope : String
  }


type alias ApiError =
  { error : Bool
  , message: String
  }


type alias UserInfo =
  { id : String
  , name : String
  , email : String
  , provider : String
  , img : String
  }


type alias Flags =
  { websocketHost : String
  }


type alias Model =
  { route : Route
  , user : Maybe UserInfo
  , authInfo : AuthInformation
  , websocketHost : String
  }


-- vim: et sw=2 sts=2
