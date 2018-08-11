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
        ]
    Types.BuildBuildingRequest building level ->
      JE.object
        [ requestType "build_building"
        , ("type", JE.string building)
        , ("level", JE.int level)
        ]
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
      "planet" -> JD.map Types.Planet planetDecoder
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


buildingInfoDecoder : JD.Decoder Types.BuildingInfo
buildingInfoDecoder =
  resources Types.BuildingInfo
    |: (JD.field "type" JD.string)
    |: (JD.field "planet" JD.string)
    |: (JD.field "level" JD.int)


resourceInfoDecoder : JD.Decoder Types.ResourceInfo
resourceInfoDecoder =
  resources Types.ResourceInfo
    |: (JD.field "planet" JD.string)


resources : (Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> a) -> JD.Decoder a
resources f =
  JD.map f (JD.field "workers" JD.int)
  |: (JD.field "power" JD.int)
  |: (JD.field "iron_ore" JD.int)
  |: (JD.field "gold" JD.int)
  |: (JD.field "h2o" JD.int)
  |: (JD.field "oil" JD.int)
  |: (JD.field "h2" JD.int)
  |: (JD.field "uranium" JD.int)
  |: (JD.field "pvc" JD.int)
  |: (JD.field "kyanite" JD.int)


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



apply : JD.Decoder (a -> b) -> JD.Decoder a -> JD.Decoder b
apply f aDecoder =
  f |> JD.andThen (\f0 -> JD.map f0 aDecoder)


(|:) : JD.Decoder (a -> b) -> JD.Decoder a -> JD.Decoder b
(|:) = apply


-- vim: et sw=2 sts=2
