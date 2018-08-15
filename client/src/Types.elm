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

module Types exposing (..)

import Dict exposing ( Dict )

import Navigation exposing ( Location )


type Msg
  = NoOp
  -- internals
  | NavigationChange Location
  | NewUrl Route
  -- API
  | ApiResponseError String
  | ApiResponse ApiContent
  | ApiRequest Request


type Route
  = HomeRoute
  | LoginRoute
  | LogoutRoute
  -- auth (code, state, scope, provider)
  | AuthRoute (Maybe String) (Maybe String) (Maybe String) (Maybe String)
  | HelpRoute


type ApiContent
  = Auth AuthInformation
  | User UserInfo
  | Planet PlanetInfo
  | Building BuildingInfo
  | Resources ResourceInfo
  | Error ApiError


type Request
  = AuthorizeRequest Authorize
  | BuildBuildingRequest String String Int
  | PlanetInfoRequest
  | LogoutRequest


type alias AuthInformation =
  { provider : String
  , loginUrl : String
  }


type alias Authorize =
  { code : String
  , state : String
  , scope : String
  , provider : String
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


type PlanetType
  = EarthPlanet
  | FirePlanet
  | WaterPlanet
  | IcePlanet


type alias PlanetInfo =
  { id : String
  , position : (Int, Int, Int)
  , size : Int
  , planetType : PlanetType
  , index : Int
  }


type alias Flags =
  { websocketHost : String
  }


type alias BuildingInfo =
  { workers : Int
  , power : Int
  , ironOre : Int
  , gold : Int
  , h2o : Int
  , oil : Int
  , h2 : Int
  , uranium : Int
  , pvc : Int
  , kyanite : Int
  , name : String
  , planetId : String
  , level : Int
  }


type alias ResourceInfo =
  { workers : Int
  , power : Int
  , ironOre : Int
  , gold : Int
  , h2o : Int
  , oil : Int
  , h2 : Int
  , uranium : Int
  , pvc : Int
  , kyanite : Int
  , planetId: String
  }


type alias ActivePlanet =
  { planet : PlanetInfo
  , buildings : Dict String BuildingInfo
  , resources : ResourceInfo
  }


type alias Model =
  { route : Route
  , user : Maybe UserInfo
  , authInfo : List AuthInformation
  , planets : Dict String PlanetInfo
  , planet : Maybe ActivePlanet
  , websocketHost : String
  }


-- vim: et sw=2 sts=2
