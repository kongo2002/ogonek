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


module View.Research exposing (duration, progress, research, status, target)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Time
import Types exposing (..)
import Utils exposing (deltaToString, zonedIso8601)
import View.Utils exposing (..)


research : Model -> List (Html Msg)
research model =
    let
        res =
            model.research

        row ( name, level ) =
            tr []
                [ td [] [ text (translateResearch name) ]
                , td [] [ text (String.fromInt level) ]
                ]

        statusText =
            status model res

        status0 =
            case ( res.status, res.duration ) of
                ( _, Nothing ) ->
                    div []
                        [ p [] [ text statusText ]
                        ]

                ( Just status1, _ ) ->
                    div []
                        [ p [] [ text "research target: ", text (target status1) ]
                        , p [] [ text statusText ]
                        ]

                ( Nothing, _ ) ->
                    div []
                        [ p [] [ text statusText ]
                        , button [ onClick (ApiRequest StartResearchRequest) ] [ text "Research" ]
                        ]
    in
    [ h2 [] [ text "Research" ]
    , status0
    , h3 [] [ text "Overview" ]
    , div [ class "row" ]
        [ div [ class "six columns" ]
            [ table [ class "u-full-width" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Research" ]
                        , th [] [ text "Level" ]
                        ]
                    ]
                , tbody [] (List.map row res.research)
                ]
            ]
        ]
    ]


target : ResearchStatusInfo -> String
target status0 =
    status0.name
        |> Maybe.map translateResearch
        |> Maybe.withDefault "still unknown"


status : Model -> ResearchInfo -> String
status model research0 =
    case research0.status of
        Just status0 ->
            case model.lastTimeStamp of
                Just now ->
                    let
                        duration0 =
                            duration status0 now
                    in
                    "research finished in: " ++ duration0

                Nothing ->
                    "research finished at: " ++ zonedIso8601 model status0.finish

        Nothing ->
            case research0.duration of
                Just duration0 ->
                    "no research in progress - duration (" ++ deltaToString duration0 ++ ")"

                Nothing ->
                    "no research possible yet"


duration : ResearchStatusInfo -> Time.Posix -> String
duration status0 now =
    let
        prog =
            progress status0 now

        duration0 =
            Utils.posixDelta status0.finish now

        duration1 =
            Utils.deltaToString duration0
    in
    duration1 ++ " (" ++ String.fromInt prog ++ " %)"


progress : ResearchStatusInfo -> Time.Posix -> Int
progress status0 now =
    let
        started =
            Time.posixToMillis status0.created

        finished =
            Time.posixToMillis status0.finish

        now0 =
            Time.posixToMillis now

        durationMs =
            finished - started

        progress0 =
            now0 - started
    in
    (toFloat progress0 * 100 / toFloat durationMs) |> round |> Basics.max 0 |> Basics.min 100



-- vim: et sw=2 sts=2
