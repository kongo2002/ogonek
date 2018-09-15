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
import Time.DateTime
import Time.TimeZone

import Navigation exposing ( Location )


type Msg
  = NoOp
  -- internals
  | NavigationChange Location
  | NewUrl Route
  | Tick Time.DateTime.DateTime
  -- forms
  | FormContent String String
  | LocalLogin
  -- buildings
  | SetBuildingsFilter BuildingsFilter
  -- API
  | ApiResponseError String
  | ApiResponse ApiContent
  | ApiRequest Request


type Route
  = HomeRoute
  | ResearchRoute
  | LoginRoute
  | LogoutRoute
  | PlanetRoute String
  | ProductionRoute String
  | BuildingRoute String
  -- auth (code, state, scope, provider)
  | AuthRoute (Maybe String) (Maybe String) (Maybe String) (Maybe String)
  | HelpRoute


type ApiContent
  = Auth AuthInformation
  | User UserInfo
  | Planet PlanetInfo
  | Building BuildingInfo
  | Construction ConstructionInfo
  | Capacity CapacityInfo
  | Resources ResourceInfo
  | Research ResearchInfo
  | Production ResourceInfo
  | Utilization ResourceInfo
  | Error ApiError


type Request
  = AuthorizeRequest Authorize
  | BuildBuildingRequest String String Int
  | StartResearchRequest
  | PlanetInfoRequest
  -- get_utilization (planet-id)
  | UtilizationRequest String
  -- set_utilization (planet-id, resource, value)
  | SetUtilizationRequest String String Int
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
  { defaultTimeZone : String
  , websocketHost : String
  }


type alias BuildingInfo =
  { ironOre : Int
  , gold : Int
  , h2o : Int
  , oil : Int
  , h2 : Int
  , uranium : Int
  , pvc : Int
  , titan : Int
  , kyanite : Int
  , workers : Int
  , power : Int
  , name : String
  , planetId : String
  , level : Int
  , duration : Time.DateTime.DateTimeDelta
  , group : String
  }


type alias ConstructionInfo =
  { timeLeft : Maybe String
  , planetId : String
  , building : String
  , level : Int
  , created : Time.DateTime.DateTime
  , finish : Time.DateTime.DateTime
  }


type alias ResourceInfo =
  { ironOre : Int
  , gold : Int
  , h2o : Int
  , oil : Int
  , h2 : Int
  , uranium : Int
  , pvc : Int
  , titan : Int
  , kyanite : Int
  , workers : Int
  , power : Int
  , planetId: String
  }


type alias CapacityInfo =
  { ironOre : Int
  , gold : Int
  , h2o : Int
  , oil : Int
  , h2 : Int
  , uranium : Int
  , pvc : Int
  , titan : Int
  , kyanite : Int
  , planetId: String
  }


type alias ResearchStatusInfo =
  { created : Time.DateTime.DateTime
  , finish : Time.DateTime.DateTime
  , name : Maybe String
  }


type alias ResearchInfo =
  { research : List (String, Int)
  , duration : Time.DateTime.DateTimeDelta
  , status : Maybe ResearchStatusInfo
  }


type BuildingsFilter
  = AllBuildings
  | AvailableBuildings
  | InProgressBuildings


type alias ActivePlanet =
  { planet : PlanetInfo
  , buildings : Dict String BuildingInfo
  , constructions : Dict String ConstructionInfo
  , resources : ResourceInfo
  , capacity : CapacityInfo
  , production : ResourceInfo
  , utilization : ResourceInfo
  , buildingsFilter : BuildingsFilter
  }


type alias Model =
  { route : Route
  , user : Maybe UserInfo
  , authInfo : Dict String AuthInformation
  , planets : Dict String ActivePlanet
  , research : ResearchInfo
  , timeZone : Time.TimeZone.TimeZone
  , websocketHost : String
  , formContents : Dict String String
  , lastTimeStamp : Maybe Time.DateTime.DateTime
  }


-- vim: et sw=2 sts=2
