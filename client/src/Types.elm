module Types exposing (..)


type Msg
  = NoOp
  | ApiResponseError String
  | ApiResponse ApiContent
  | ApiRequest String


type ApiContent
  = Auth AuthInformation


type alias AuthInformation =
  { provider : String
  , loginUrl : String
  }


type alias Login =
  { id : String
  , email : String
  }


type alias Flags =
  { websocketHost : String
  }


type alias Model =
  { login : Maybe Login
  , websocketHost : String
  }


-- vim: et sw=2 sts=2
