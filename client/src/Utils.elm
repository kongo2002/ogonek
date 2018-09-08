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

module Utils exposing (..)

import Dict
import Time.DateTime
import Time.Iso8601
import Time.ZonedDateTime

import Types exposing (..)


deltaToString : Time.DateTime.DateTimeDelta -> String
deltaToString delta =
  let fmt value oneUnit multiples =
      let unit = if value == 1 then oneUnit else multiples
      in  toString value ++ " " ++ unit
  in  if delta.days > 0 && delta.hours >= 24 then
        fmt delta.days "day" "days"
      else if delta.hours > 0 && delta.minutes >= 60 then
        fmt delta.hours "hour" "hours"
      else if delta.minutes > 0 then
        fmt delta.minutes "minute" "minutes"
      else if delta.seconds <= 0 then
        "finished"
      else
        fmt delta.seconds "second" "seconds"


zonedIso8601 : Model -> Time.DateTime.DateTime -> String
zonedIso8601 model date =
  let timeZone = model.timeZone
      zoned = Time.ZonedDateTime.fromDateTime timeZone date
  in  Time.Iso8601.fromZonedDateTime zoned


orEmpty : Maybe String -> String
orEmpty value =
  case value of
    Just str -> str
    Nothing -> ""


planetToString : PlanetType -> String
planetToString planet =
  case planet of
    EarthPlanet -> "earth"
    FirePlanet -> "fire"
    WaterPlanet -> "water"
    IcePlanet -> "ice"


nothing : Maybe a -> Bool
nothing maybe =
  case maybe of
    Nothing -> True
    _ -> False


just : Maybe a -> Bool
just = nothing >> not


find : (a -> Bool) -> List a -> Maybe a
find predicate list =
  case list of
    hd :: tl ->
      if predicate hd then Just hd
      else find predicate tl
    _ -> Nothing


capacityPercent : Int -> Int -> String
capacityPercent prod capacity =
  let progress = prod * 100 // capacity
      progress0 = toString progress
  in  progress0 ++ "%"


groupBy : (a -> comparable) -> List a -> Dict.Dict comparable (List a)
groupBy by input =
  let go v acc =
        let key = by v
            existing = Dict.get key acc
            updated =
              case existing of
                Just vs -> v :: vs
                Nothing -> [v]
        in  Dict.insert key updated acc

  in  List.foldr go Dict.empty input


-- vim: et sw=2 sts=2
