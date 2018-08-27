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

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onWithOptions )
import Json.Decode

import Assets
import Types


translateBuilding : Types.BuildingInfo -> String
translateBuilding binfo =
  translateBuildingName binfo.name


translateBuildingName : String -> String
translateBuildingName name =
  case name of
    "construction_center" -> "construction center"
    "research_lab" -> "research lab"
    "oil_rig" -> "oil rig"
    "water_rig" -> "water rig"
    "ore_mine" -> "ore mine"
    "gold_mine" -> "gold mine"
    "oil_tank" -> "oil tank"
    "water_tank" -> "water tank"
    "ore_depot" -> "ore depot"
    "gold_depot" -> "gold depot"
    "power_plant" -> "power plant"
    "wind_turbine" -> "wind turbine"
    "apartment" -> "apartment"
    "apartment_block" -> "apartment block"
    unknown -> unknown


planetImg : Types.PlanetInfo -> Html Types.Msg
planetImg planet =
  let typ = planet.planetType
      asset = Assets.planetAsset typ
      path = Assets.path asset
  in  img [ class "planet", src path ] []


numberSpan : Int -> Html Types.Msg
numberSpan = numberSpanTo -1


numberSpanTo : Int -> Int -> Html Types.Msg
numberSpanTo relativeTo value =
  let negative = value < 0
      range =
        if relativeTo < 0 then
          "zero"
        else if value > relativeTo then
          "negative"
        else if value < relativeTo then
          "positive"
        else
          "zero"
      strValue = toString value
      trimmed = if negative then String.dropLeft 1 strValue else strValue
      splitted = String.join "," (splitThousands trimmed)
      result = if negative then "-" ++ splitted else splitted
  in span [ class ("number " ++ range) ] [ text result ]


splitThousands : String -> List String
splitThousands integers =
    let reversedSplit value =
          if String.length value > 3 then
            value
            |> String.dropRight 3
            |> reversedSplit
            |> (::) (String.right 3 value)
          else
            [ value ]
    in integers |> reversedSplit |> List.reverse


icon : String -> Html Types.Msg
icon name =
  let clazz = "fas fa-" ++ name
  in  i [ class clazz ] []


numbClick : a -> Attribute a
numbClick msg =
  let opts = { stopPropagation = False, preventDefault = True }
  in  onWithOptions "click" opts (Json.Decode.succeed msg)


toRight : Attribute a
toRight = class "u-pull-right"


loggedIn : Types.Model -> Bool
loggedIn model =
  case model.user of
    Just _ -> True
    Nothing -> False


coordStr : (Int, Int, Int) -> String
coordStr coord =
  let (x, y, z) = coord
  in  "(" ++ toString x ++ "," ++ toString y ++ "," ++ toString z ++ ")"


-- vim: et sw=2 sts=2
