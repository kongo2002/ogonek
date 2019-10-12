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
import Time
import Types exposing (..)


posixDelta : Time.Posix -> Time.Posix -> Duration
posixDelta start end =
    let
        start0 =
            Time.posixToMillis start

        end0 =
            Time.posixToMillis end
    in
    abs (end0 - start0)


deltaToString : Duration -> String
deltaToString delta =
    let
        secondsPerMin =
            60

        minutesPerHour =
            60

        hoursPerDay =
            24

        seconds =
            delta // 1000

        minutes =
            seconds // secondsPerMin

        hours =
            minutes // minutesPerHour

        days =
            hours // hoursPerDay

        fmt value oneUnit multiples =
            let
                unit =
                    if value == 1 then
                        oneUnit

                    else
                        multiples
            in
            String.fromInt value ++ " " ++ unit
    in
    if days > 0 && hours >= 24 then
        fmt days "day" "days"

    else if hours > 0 && minutes >= 60 then
        fmt hours "hour" "hours"

    else if minutes > 0 then
        fmt minutes "minute" "minutes"

    else if seconds <= 0 then
        "finished"

    else
        fmt seconds "second" "seconds"


zonedIso8601 : Model -> Time.Posix -> String
zonedIso8601 model date =
    let
        zone =
            model.timeZone

        year =
            Time.toYear zone date |> String.fromInt

        month =
            Time.toMonth zone date |> monthIndex |> intStr

        day =
            Time.toDay zone date |> intStr

        hour =
            Time.toHour zone date |> intStr

        minute =
            Time.toMinute zone date |> intStr

        second =
            Time.toSecond zone date |> intStr
    in
    year ++ "-" ++ month ++ "-" ++ day ++ "T" ++ hour ++ ":" ++ minute ++ ":" ++ second


monthIndex : Time.Month -> Int
monthIndex month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


intStr : Int -> String
intStr value =
    if value > 9 then
        String.fromInt value

    else
        "0" ++ String.fromInt value


orEmpty : Maybe String -> String
orEmpty value =
    case value of
        Just str ->
            str

        Nothing ->
            ""


planetToString : PlanetType -> String
planetToString planet =
    case planet of
        EarthPlanet ->
            "earth"

        FirePlanet ->
            "fire"

        WaterPlanet ->
            "water"

        IcePlanet ->
            "ice"


buildingLevel : ActivePlanet -> String -> Int
buildingLevel planet building =
    let
        buildings =
            planet.buildings
    in
    Dict.get building buildings
        |> Maybe.map .level
        |> Maybe.withDefault 0


nothing : Maybe a -> Bool
nothing maybe =
    case maybe of
        Nothing ->
            True

        _ ->
            False


just : Maybe a -> Bool
just =
    nothing >> not


find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        hd :: tl ->
            if predicate hd then
                Just hd

            else
                find predicate tl

        _ ->
            Nothing


capacityPercent : Int -> Int -> String
capacityPercent prod capacity =
    let
        progress =
            prod * 100 // capacity

        progress0 =
            String.fromInt progress
    in
    progress0 ++ "%"


groupBy : (a -> comparable) -> List a -> Dict.Dict comparable (List a)
groupBy by input =
    let
        go v acc =
            let
                key =
                    by v

                existing =
                    Dict.get key acc

                updated =
                    case existing of
                        Just vs ->
                            v :: vs

                        Nothing ->
                            [ v ]
            in
            Dict.insert key updated acc
    in
    List.foldr go Dict.empty input



-- vim: et sw=2 sts=2
