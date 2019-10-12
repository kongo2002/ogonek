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


module View.Utils exposing (..)

import Assets
import Const
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (preventDefaultOn)
import Json.Decode
import Types


translateBuilding : Types.BuildingInfo -> String
translateBuilding binfo =
    translateBuildingName binfo.name


translateBuildingName : String -> String
translateBuildingName name =
    case name of
        "apartment" ->
            "apartment"

        "apartment_block" ->
            "apartment block"

        "apartment_complex" ->
            "apartment complex"

        "chemical_factory" ->
            "chemical factory"

        "construction_center" ->
            "construction center"

        "ext_gold_mine" ->
            "extended gold mine"

        "ext_oil_rig" ->
            "extended oil rig"

        "ext_ore_mine" ->
            "extended ore mine"

        "ext_water_rig" ->
            "extended water rig"

        "gold_depot" ->
            "gold depot"

        "gold_mine" ->
            "gold mine"

        "h2_depot" ->
            "hydrogen depot"

        "hydro_plant" ->
            "hydro plant"

        "kyanite_depot" ->
            "kyanite depot"

        "kyanite_mine" ->
            "kyanite mine"

        "oil_rig" ->
            "oil rig"

        "oil_tank" ->
            "oil tank"

        "ore_depot" ->
            "ore depot"

        "ore_mine" ->
            "ore mine"

        "plastic_factory" ->
            "plastic factory"

        "power_plant" ->
            "power plant"

        "pvc_depot" ->
            "PVC depot"

        "research_lab" ->
            "research lab"

        "smelting_plant" ->
            "smelting plant"

        "space_shipyard" ->
            "space shipyard"

        "titan_depot" ->
            "titan depot"

        "uranium_depot" ->
            "uranium depot"

        "uranium_mine" ->
            "uranium mine"

        "water_rig" ->
            "water rig"

        "water_tank" ->
            "water tank"

        "weapon_manufacture" ->
            "weapon manufacture"

        "wind_turbine" ->
            "wind turbine"

        unknown ->
            unknown


translateResearch : String -> String
translateResearch name =
    case name of
        "alternative_energies" ->
            "alternative energies"

        "chemistry" ->
            "chemistry"

        "construction" ->
            "construction"

        "economy" ->
            "economy"

        "mining" ->
            "mining"

        "nuclear_fission" ->
            "nuclear fission"

        "nuclear_fusion" ->
            "nuclear fusion"

        "particle_physics" ->
            "particle physics"

        "radioactivity" ->
            "radioactivity"

        "military" ->
            "military"

        unknown ->
            unknown


translateBuildingGroup : String -> String
translateBuildingGroup name =
    case name of
        "general" ->
            "general buildings"

        "resource" ->
            "resource gathering and production"

        "storage" ->
            "depots and tanks"

        "power_living" ->
            "power and living buildings"

        "military" ->
            "military buildings"

        unknown ->
            unknown


translateWeaponOrder : Types.WeaponOrderInfo -> String
translateWeaponOrder order =
    translateWeaponName order.weapon


translateWeaponName : String -> String
translateWeaponName name =
    let
        upper =
            String.toUpper name
    in
    "weapon " ++ upper


translateResource : String -> String
translateResource name =
    case name of
        "workers" ->
            Const.workers

        "power" ->
            Const.power

        "iron_ore" ->
            Const.ironOre

        "gold" ->
            Const.gold

        "h2o" ->
            Const.h2o

        "oil" ->
            Const.oil

        "h2" ->
            Const.h2

        "uranium" ->
            Const.uranium

        "pvc" ->
            Const.pvc

        "titan" ->
            Const.titan

        "kyanite" ->
            Const.kyanite

        unknown ->
            unknown


planetImg : Types.PlanetInfo -> Html Types.Msg
planetImg planet =
    let
        typ =
            planet.planetType

        asset =
            Assets.planetAsset typ

        path =
            Assets.path asset
    in
    img [ class "planet", src path ] []


numberSpan : Int -> Html Types.Msg
numberSpan =
    numberSpanTo -1


numberSpanTo : Int -> Int -> Html Types.Msg
numberSpanTo relativeTo value =
    let
        negative =
            value < 0

        range =
            if relativeTo < 0 then
                "zero"

            else if value > relativeTo then
                "negative"

            else if value < relativeTo then
                "positive"

            else
                "zero"

        strValue =
            String.fromInt value

        trimmed =
            if negative then
                String.dropLeft 1 strValue

            else
                strValue

        splitted =
            String.join "," (splitThousands trimmed)

        result =
            if negative then
                "-" ++ splitted

            else
                splitted
    in
    span [ class ("number " ++ range) ] [ text result ]


splitThousands : String -> List String
splitThousands integers =
    let
        reversedSplit value =
            if String.length value > 3 then
                value
                    |> String.dropRight 3
                    |> reversedSplit
                    |> (::) (String.right 3 value)

            else
                [ value ]
    in
    integers |> reversedSplit |> List.reverse


icon : String -> Html Types.Msg
icon name =
    let
        clazz =
            "fas fa-" ++ name
    in
    i [ class clazz ] []


numbClick : a -> Attribute a
numbClick msg =
    preventDefaultOn "click" (Json.Decode.map alwaysPreventDefault (Json.Decode.succeed msg))


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


toRight : Attribute a
toRight =
    class "u-pull-right"


loggedIn : Types.Model -> Bool
loggedIn model =
    case model.user of
        Just _ ->
            True

        Nothing ->
            False


onChange : (String -> a) -> Attribute a
onChange msg =
    let
        parser =
            Html.Events.targetValue |> Json.Decode.map msg
    in
    Html.Events.on "change" parser


onChangeInt : (Int -> a) -> Attribute a
onChangeInt msg =
    let
        int value =
            case String.toInt value of
                Just int0 ->
                    Json.Decode.succeed int0

                Nothing ->
                    Json.Decode.fail "invalid integer value given"

        parser =
            Html.Events.targetValue
                |> Json.Decode.andThen int
                |> Json.Decode.map msg
    in
    Html.Events.on "change" parser


planetName : Types.PlanetInfo -> String
planetName planet =
    let
        coord =
            coordStr planet.position
    in
    "Planet at " ++ coord


coordStr : ( Int, Int, Int ) -> String
coordStr coord =
    let
        ( x, y, z ) =
            coord
    in
    "(" ++ String.fromInt x ++ "," ++ String.fromInt y ++ "," ++ String.fromInt z ++ ")"



-- vim: et sw=2 sts=2
