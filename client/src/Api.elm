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


module Api exposing (connect, listen, send)

import Json.Decode as JD
import Json.Encode as JE
import Ports
import Resources
import Time
import Types


listen : Sub Types.Msg
listen =
    Ports.fromWebsocket parseWsJson


connect : Cmd msg
connect =
    let
        cmd =
            message "connect" (JE.object [])
    in
    Ports.toWebsocket cmd


send : Types.Request -> Cmd Types.Msg
send msg =
    let
        req =
            toRequest msg
    in
    Ports.toWebsocket req


message : String -> JE.Value -> JE.Value
message msgType msg =
    JE.object
        [ ( "type", JE.string msgType )
        , ( "msg", msg )
        ]


toRequest : Types.Request -> JE.Value
toRequest request =
    let
        json =
            requestEncoder request
    in
    message "send" json


requestEncoder : Types.Request -> JE.Value
requestEncoder req =
    case req of
        Types.AuthorizeRequest auth ->
            JE.object
                [ requestType "authorize"
                , ( "code", JE.string auth.code )
                , ( "state", JE.string auth.state )
                , ( "scope", JE.string auth.scope )
                , ( "provider", JE.string auth.provider )
                ]

        Types.BuildBuildingRequest planet building level ->
            JE.object
                [ requestType "build_building"
                , ( "planet", JE.string planet )
                , ( "type", JE.string building )
                , ( "level", JE.int level )
                ]

        Types.BuildWeaponRequest planet weapon ->
            JE.object
                [ requestType "build_weapon"
                , ( "planet", JE.string planet )
                , ( "weapon", JE.string weapon )
                ]

        Types.UtilizationRequest planet ->
            JE.object
                [ requestType "get_utilization"
                , ( "planet", JE.string planet )
                ]

        Types.SetUtilizationRequest planet resource value ->
            JE.object
                [ requestType "set_utilization"
                , ( "planet", JE.string planet )
                , ( "resource", JE.string (Resources.resourceKey resource) )
                , ( "value", JE.int value )
                ]

        Types.PlanetInfoRequest ->
            JE.object [ requestType "planet_info" ]

        Types.StartResearchRequest ->
            JE.object [ requestType "start_research" ]

        Types.LogoutRequest ->
            JE.object
                [ requestType "logout"
                ]


requestType : String -> ( String, JE.Value )
requestType typ =
    ( "t", JE.string typ )


parseWebsocket : JD.Decoder Types.Msg
parseWebsocket =
    JD.field "type" JD.string
        |> JD.andThen
            (\t ->
                case t of
                    "send" ->
                        JD.at [ "msg", "data" ] (JD.map fromApiContent payloadDecoder)

                    "connected" ->
                        websocketConnected

                    "connecting" ->
                        JD.succeed Types.WebsocketConnecting

                    "error" ->
                        JD.succeed Types.WebsocketError

                    "closed" ->
                        JD.succeed Types.WebsocketClosed

                    _ ->
                        JD.fail ("unexpected websocket message: " ++ t)
            )


websocketConnected : JD.Decoder Types.Msg
websocketConnected =
    JD.at [ "msg", "url" ] JD.string |> JD.map Types.WebsocketConnected


fromApiContent : Types.ApiContent -> Types.Msg
fromApiContent content =
    case content of
        Types.Error err ->
            Types.ApiResponseError err.message

        content0 ->
            Types.ApiResponse content0


parseWsJson : JD.Value -> Types.Msg
parseWsJson payload =
    case JD.decodeValue parseWebsocket payload of
        -- for now we will map the error content into the
        -- generic error response type
        Ok parsed ->
            parsed

        Err error0 ->
            Types.ApiResponseError (JD.errorToString error0)


payloadDecoder : JD.Decoder Types.ApiContent
payloadDecoder =
    -- use the "t" key to determine which decoder to use
    JD.field "t" JD.string
        |> JD.andThen
            (\t ->
                case t of
                    "resources" ->
                        JD.map Types.Resources resourceInfoDecoder

                    "building" ->
                        JD.map Types.Building buildingInfoDecoder

                    "construction" ->
                        JD.map Types.Construction constructionDecoder

                    "planet" ->
                        JD.map Types.Planet planetDecoder

                    "capacity" ->
                        JD.map Types.Capacity capacityInfoDecoder

                    "production" ->
                        JD.map Types.Production resourceInfoDecoder

                    "utilization" ->
                        JD.map Types.Utilization resourceInfoDecoder

                    "research" ->
                        JD.map Types.Research researchInfoDecoder

                    "weapon" ->
                        JD.map Types.Weapon weaponInfoDecoder

                    "w_order" ->
                        JD.map Types.WeaponOrder weaponOrderInfoDecoder

                    "w_order_finished" ->
                        weaponOrderFinishedDecoder

                    "authinfo" ->
                        JD.map Types.Auth authInfoDecoder

                    "user" ->
                        JD.map Types.User userInfoDecoder

                    "error" ->
                        JD.map Types.Error errorDecoder

                    _ ->
                        JD.fail ("unexpected API message " ++ t)
            )


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
    JD.map5 Types.WeaponOrderInfo
        (JD.field "_id" JD.string)
        (JD.field "planet" JD.string)
        (JD.field "weapon" JD.string)
        (JD.field "created" dateTimeDecoder)
        (JD.field "finish" dateTimeDecoder)


weaponOrderFinishedDecoder : JD.Decoder Types.ApiContent
weaponOrderFinishedDecoder =
    JD.map2 Types.WeaponOrderFinished
        (JD.field "planet" JD.string)
        (JD.field "_id" JD.string)


buildingInfoDecoder : JD.Decoder Types.BuildingInfo
buildingInfoDecoder =
    resources Types.BuildingInfo
        |> andMap (JD.field "workers" JD.int)
        |> andMap (JD.field "power" JD.int)
        |> andMap (JD.field "type" JD.string)
        |> andMap (JD.field "planet" JD.string)
        |> andMap (JD.field "level" JD.int)
        |> andMap (JD.field "duration" dateTimeDeltaDecoder)
        |> andMap (JD.field "group" JD.string)


weaponInfoDecoder : JD.Decoder Types.WeaponInfo
weaponInfoDecoder =
    resources Types.WeaponInfo
        |> andMap (JD.field "name" JD.string)
        |> andMap (JD.field "planet" JD.string)
        |> andMap (JD.field "count" JD.int)
        |> andMap (JD.field "duration" dateTimeDeltaDecoder)
        |> andMap (JD.field "space" JD.int)
        |> andMap (JD.field "power" JD.int)
        |> andMap (JD.field "dmg" JD.float)
        |> andMap (JD.field "load" JD.int)


resourceInfoDecoder : JD.Decoder Types.ResourceInfo
resourceInfoDecoder =
    resources Types.ResourceInfo
        |> andMap (JD.field "workers" JD.int)
        |> andMap (JD.field "power" JD.int)
        |> andMap (JD.field "planet" JD.string)


capacityInfoDecoder : JD.Decoder Types.CapacityInfo
capacityInfoDecoder =
    resources Types.CapacityInfo
        |> andMap (JD.field "planet" JD.string)


researchInfoDecoder : JD.Decoder Types.ResearchInfo
researchInfoDecoder =
    let
        research =
            JD.map2 (\a b -> ( a, b ))
                (JD.field "name" JD.string)
                (JD.field "level" JD.int)

        status =
            JD.map3 Types.ResearchStatusInfo
                (JD.field "created" dateTimeDecoder)
                (JD.field "finish" dateTimeDecoder)
                (JD.maybe (JD.field "name" JD.string))
    in
    JD.map3 Types.ResearchInfo
        (JD.field "research" (JD.list research))
        (JD.maybe (JD.field "duration" dateTimeDeltaDecoder))
        (JD.maybe (JD.field "status" status))


resources : (Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> a) -> JD.Decoder a
resources f =
    JD.succeed f
        |> andMap (JD.field "iron_ore" JD.int)
        |> andMap (JD.field "gold" JD.int)
        |> andMap (JD.field "h2o" JD.int)
        |> andMap (JD.field "oil" JD.int)
        |> andMap (JD.field "h2" JD.int)
        |> andMap (JD.field "uranium" JD.int)
        |> andMap (JD.field "pvc" JD.int)
        |> andMap (JD.field "titan" JD.int)
        |> andMap (JD.field "kyanite" JD.int)


dateTimeDeltaDecoder : JD.Decoder Int
dateTimeDeltaDecoder =
    let
        toMillis =
            (*) 1000
    in
    JD.int |> JD.map toMillis


coordDecoder : JD.Decoder ( Int, Int, Int )
coordDecoder =
    let
        toCoord input =
            case input of
                [ x, y, z ] ->
                    JD.succeed ( x, y, z )

                _ ->
                    JD.fail "expecting triple of integers"
    in
    JD.list JD.int |> JD.andThen toCoord


planetTypeDecoder : JD.Decoder Types.PlanetType
planetTypeDecoder =
    let
        mapper input =
            case input of
                "earth" ->
                    JD.succeed Types.EarthPlanet

                "fire" ->
                    JD.succeed Types.FirePlanet

                "water" ->
                    JD.succeed Types.WaterPlanet

                "ice" ->
                    JD.succeed Types.IcePlanet

                invalid ->
                    JD.fail ("invalid planet type: " ++ invalid)
    in
    JD.string |> JD.andThen mapper


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


dateTimeDecoder : JD.Decoder Time.Posix
dateTimeDecoder =
    JD.int |> JD.map Time.millisToPosix


apply : JD.Decoder (a -> b) -> JD.Decoder a -> JD.Decoder b
apply f aDecoder =
    f |> JD.andThen (\f0 -> JD.map f0 aDecoder)


andMap : JD.Decoder a -> JD.Decoder (a -> b) -> JD.Decoder b
andMap =
    JD.map2 (|>)



-- vim: et sw=2 sts=2
