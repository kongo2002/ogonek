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

module Api exposing ( send, websocket )

import Json.Decode as JD
import Json.Encode as JE
import Time.DateTime exposing ( DateTime )
import Time.Iso8601
import WebSocket

import Types


websocket : Types.Model -> Sub Types.Msg
websocket model =
  let ws = model.websocketHost
  in  WebSocket.listen ws parseWsJson


send : Types.Model -> Types.Request -> Cmd Types.Msg
send model msg =
  let ws = model.websocketHost
      req = toRequest msg
  in  WebSocket.send ws req


toRequest : Types.Request -> String
toRequest request =
  let json = requestEncoder request
  in  JE.encode 0 json


requestEncoder : Types.Request -> JE.Value
requestEncoder req =
  case req of
    Types.AuthorizeRequest auth ->
      JE.object
        [ requestType "authorize"
        , ("code", JE.string auth.code)
        , ("state", JE.string auth.state)
        , ("scope", JE.string auth.scope)
        , ("provider", JE.string auth.provider)
        ]
    Types.BuildBuildingRequest planet building level ->
      JE.object
        [ requestType "build_building"
        , ("planet", JE.string planet)
        , ("type", JE.string building)
        , ("level", JE.int level)
        ]
    Types.UtilizationRequest planet ->
      JE.object
        [ requestType "get_utilization"
        , ("planet", JE.string planet)
        ]
    Types.SetUtilizationRequest planet resource value ->
      JE.object
        [ requestType "set_utilization"
        , ("planet", JE.string planet)
        , ("resource", JE.string resource)
        , ("value", JE.int value)
        ]
    Types.PlanetInfoRequest ->
      JE.object [ requestType "planet_info" ]
    Types.StartResearchRequest ->
      JE.object [ requestType "start_research" ]
    Types.LogoutRequest ->
      JE.object
        [ requestType "logout"
        ]


requestType : String -> (String, JE.Value)
requestType typ =
  ("t", JE.string typ)


parseWsJson : String -> Types.Msg
parseWsJson payload =
  case JD.decodeString payloadDecoder payload of
    -- for now we will map the error content into the
    -- generic error response type
    Ok (Types.Error err) -> Types.ApiResponseError err.message
    Ok content -> Types.ApiResponse content
    Err error -> Types.ApiResponseError error


payloadDecoder : JD.Decoder Types.ApiContent
payloadDecoder =
  -- use the "t" key to determine which decoder to use
  (JD.field "t" JD.string)
  |> JD.andThen (\t ->
    case t of
      "resources" -> JD.map Types.Resources resourceInfoDecoder
      "building" -> JD.map Types.Building buildingInfoDecoder
      "construction" -> JD.map Types.Construction constructionDecoder
      "planet" -> JD.map Types.Planet planetDecoder
      "capacity" -> JD.map Types.Capacity capacityInfoDecoder
      "production" -> JD.map Types.Production resourceInfoDecoder
      "utilization" -> JD.map Types.Utilization resourceInfoDecoder
      "research" -> JD.map Types.Research researchInfoDecoder
      "w_order" -> JD.map Types.WeaponOrder weaponOrderInfoDecoder
      "authinfo" -> JD.map Types.Auth authInfoDecoder
      "user" -> JD.map Types.User userInfoDecoder
      "error" -> JD.map Types.Error errorDecoder
      _ -> JD.fail ("unexpected message " ++ t))


userInfoDecoder : JD.Decoder Types.UserInfo
userInfoDecoder =
  JD.map5 Types.UserInfo
    (JD.field "_id" JD.string)
    (JD.field "name" JD.string)
    (JD.field "email" JD.string)
    (JD.field "provider" JD.string)
    (JD.field "img" JD.string)


planetDecoder : JD.Decoder Types.PlanetInfo
planetDecoder =
  JD.map5 Types.PlanetInfo
    (JD.field "_id" JD.string)
    (JD.field "pos" coordDecoder)
    (JD.field "size" JD.int)
    (JD.field "type" planetTypeDecoder)
    (JD.field "idx" JD.int)


constructionDecoder : JD.Decoder Types.ConstructionInfo
constructionDecoder =
  JD.map5 Types.ConstructionInfo
    (JD.field "planet" JD.string)
    (JD.field "building" JD.string)
    (JD.field "level" JD.int)
    (JD.field "created" dateTimeDecoder)
    (JD.field "finish" dateTimeDecoder)


weaponOrderInfoDecoder : JD.Decoder Types.WeaponOrderInfo
weaponOrderInfoDecoder =
  JD.map4 Types.WeaponOrderInfo
    (JD.field "planet" JD.string)
    (JD.field "weapon" JD.string)
    (JD.field "created" dateTimeDecoder)
    (JD.field "finish" dateTimeDecoder)


buildingInfoDecoder : JD.Decoder Types.BuildingInfo
buildingInfoDecoder =
  resources Types.BuildingInfo
    |: (JD.field "workers" JD.int)
    |: (JD.field "power" JD.int)
    |: (JD.field "type" JD.string)
    |: (JD.field "planet" JD.string)
    |: (JD.field "level" JD.int)
    |: (JD.field "duration" dateTimeDeltaDecoder)
    |: (JD.field "group" JD.string)


resourceInfoDecoder : JD.Decoder Types.ResourceInfo
resourceInfoDecoder =
  resources Types.ResourceInfo
    |: (JD.field "workers" JD.int)
    |: (JD.field "power" JD.int)
    |: (JD.field "planet" JD.string)


capacityInfoDecoder : JD.Decoder Types.CapacityInfo
capacityInfoDecoder =
  resources Types.CapacityInfo
    |: (JD.field "planet" JD.string)


researchInfoDecoder : JD.Decoder Types.ResearchInfo
researchInfoDecoder =
  let research =
        JD.map2 (,)
          (JD.field "name" JD.string)
          (JD.field "level" JD.int)
      status =
        JD.map3 Types.ResearchStatusInfo
          (JD.field "created" dateTimeDecoder)
          (JD.field "finish" dateTimeDecoder)
          (JD.maybe (JD.field "name" JD.string))
  in JD.map3 Types.ResearchInfo
       (JD.field "research" (JD.list research))
       (JD.field "duration" dateTimeDeltaDecoder)
       (JD.maybe (JD.field "status" status))


resources : (Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> a) -> JD.Decoder a
resources f =
  JD.map f (JD.field "iron_ore" JD.int)
  |: (JD.field "gold" JD.int)
  |: (JD.field "h2o" JD.int)
  |: (JD.field "oil" JD.int)
  |: (JD.field "h2" JD.int)
  |: (JD.field "uranium" JD.int)
  |: (JD.field "pvc" JD.int)
  |: (JD.field "titan" JD.int)
  |: (JD.field "kyanite" JD.int)


dateTimeDeltaDecoder : JD.Decoder Time.DateTime.DateTimeDelta
dateTimeDeltaDecoder =
  let toDelta sec =
        let epoch = Time.DateTime.epoch
            dt = Time.DateTime.addSeconds sec epoch
        in  Time.DateTime.delta dt epoch
  in JD.int |> JD.map toDelta


coordDecoder : JD.Decoder (Int, Int, Int)
coordDecoder =
  let toCoord input =
        case input of
          [x, y, z] -> JD.succeed (x, y, z)
          _ -> JD.fail "expecting triple of integers"
  in JD.list JD.int |> JD.andThen toCoord


planetTypeDecoder : JD.Decoder Types.PlanetType
planetTypeDecoder =
  let mapper input =
    case input of
      "earth" -> JD.succeed Types.EarthPlanet
      "fire" -> JD.succeed Types.FirePlanet
      "water" -> JD.succeed Types.WaterPlanet
      "ice" -> JD.succeed Types.IcePlanet
      invalid -> JD.fail ("invalid planet type: " ++ invalid)
  in JD.string |> JD.andThen mapper


authInfoDecoder : JD.Decoder Types.AuthInformation
authInfoDecoder =
  JD.map2 Types.AuthInformation
    (JD.field "provider" JD.string)
    (JD.field "loginUrl" JD.string)


errorDecoder : JD.Decoder Types.ApiError
errorDecoder =
  JD.map2 Types.ApiError
    (JD.field "error" JD.bool)
    (JD.field "message" JD.string)


dateTimeDecoder : JD.Decoder DateTime
dateTimeDecoder =
  let iso8601 input =
      case Time.Iso8601.toDateTime input of
        Ok dt -> JD.succeed dt
        _ -> JD.fail "invalid ISO8601 datetime given"
  in JD.string |> JD.andThen iso8601


apply : JD.Decoder (a -> b) -> JD.Decoder a -> JD.Decoder b
apply f aDecoder =
  f |> JD.andThen (\f0 -> JD.map f0 aDecoder)


(|:) : JD.Decoder (a -> b) -> JD.Decoder a -> JD.Decoder b
(|:) = apply


-- vim: et sw=2 sts=2
