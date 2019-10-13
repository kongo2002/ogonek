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

import Browser
import Browser.Navigation
import Dict exposing (Dict)
import Time
import Url


type alias Duration =
    Int


type Msg
    = NoOp
      -- internals
    | NavigationChange Url.Url
    | NewUrl Route
    | ClickedLink Browser.UrlRequest
    | Tick Time.Posix
    | TimezoneHere Time.Zone
      -- forms
    | FormContent String String
    | LocalLogin
      -- buildings
    | SetBuildingsFilter BuildingsFilter
      -- API
    | ApiResponseError String
    | ApiResponse ApiContent
    | ApiRequest Request
      -- websocket
    | WebsocketConnecting
    | WebsocketConnected String
    | WebsocketError
    | WebsocketClosed


type Route
    = HomeRoute
    | ResearchRoute
    | LoginRoute
    | LogoutRoute
    | PlanetRoute String
    | BuildingsRoute String
    | ProductionRoute String
    | WeaponsRoute String
    | ShipsRoute String
    | BuildingRoute String
    | UserRoute
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
    | Weapon WeaponInfo
    | WeaponOrder WeaponOrderInfo
      -- w_order_finished (planet, orderId)
    | WeaponOrderFinished String String
    | Research ResearchInfo
    | Production ResourceInfo
    | Utilization ResourceInfo
    | Error ApiError


type Request
    = AuthorizeRequest Authorize
      -- build_building (planet, type, level)
    | BuildBuildingRequest String String Int
      -- build_weapon (planet, weapon)
    | BuildWeaponRequest String String
    | StartResearchRequest
    | PlanetInfoRequest
      -- get_utilization (planet-id)
    | UtilizationRequest String
      -- set_utilization (planet-id, resource, value)
    | SetUtilizationRequest String ResourceType Int
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
    , message : String
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
    , position : ( Int, Int, Int )
    , size : Int
    , planetType : PlanetType
    , index : Int
    }


type alias Flags =
    {}


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
    , duration : Duration
    , group : String
    }


type alias ConstructionInfo =
    { planetId : String
    , building : String
    , level : Int
    , created : Time.Posix
    , finish : Time.Posix
    }


type alias WeaponOrderInfo =
    { id : String
    , planetId : String
    , weapon : String
    , created : Time.Posix
    , finish : Time.Posix
    }


type alias WeaponInfo =
    { ironOre : Int
    , gold : Int
    , h2o : Int
    , oil : Int
    , h2 : Int
    , uranium : Int
    , pvc : Int
    , titan : Int
    , kyanite : Int
    , name : String
    , planetId : String
    , count : Int
    , duration : Duration
    , space : Int
    , power : Int
    , damage : Float
    , load : Int
    }


type ResourceType
    = IronOre
    | Gold
    | H2O
    | Oil
    | H2
    | Uranium
    | PVC
    | Titan
    | Kyanite
    | Workers
    | Power


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
    , planetId : String
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
    , planetId : String
    }


type alias ResearchStatusInfo =
    { created : Time.Posix
    , finish : Time.Posix
    , name : Maybe String
    }


type alias ResearchInfo =
    { research : List ( String, Int )
    , duration : Maybe Duration
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
    , weapons : Dict String WeaponInfo
    , weaponOrders : Dict String WeaponOrderInfo
    , resources : ResourceInfo
    , capacity : CapacityInfo
    , production : ResourceInfo
    , utilization : ResourceInfo
    , buildingsFilter : BuildingsFilter
    }


type WebsocketStatus
    = Disconnected
    | Connecting
    | Connected String


type alias Model =
    { route : Route
    , key : Browser.Navigation.Key
    , user : Maybe UserInfo
    , authInfo : Dict String AuthInformation
    , planets : Dict String ActivePlanet
    , research : ResearchInfo
    , timeZone : Time.Zone
    , formContents : Dict String String
    , lastTimeStamp : Maybe Time.Posix
    , websocket : WebsocketStatus
    }



-- vim: et sw=2 sts=2
