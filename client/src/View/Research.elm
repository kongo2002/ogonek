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

module View.Research exposing ( research, status, progress, target )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onClick )
import Time.DateTime

import Types exposing (..)
import Utils exposing ( zonedIso8601 )
import View.Utils exposing (..)


research : Model -> List (Html Msg)
research model =
  let res = model.research
      row (name, level) =
        tr []
        [ td [] [ text (translateResearch name) ]
        , td [] [ text (toString level) ]
        ]

      statusText = status model res
      status0 =
        case res.status of
          Just status ->
            div []
            [ p [] [ text "research target: ", text (target status) ]
            , p [] [ text statusText ]
            ]
          Nothing ->
            div []
            [ p [] [ text statusText ]
            , button [ onClick (ApiRequest StartResearchRequest) ] [ text "Research" ]
            ]

  in  [ h2 [] [ text "Research" ]
      , status0
      , h3 [] [ text "Overview"]
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
target status =
  status.name
  |> Maybe.map translateResearch
  |> Maybe.withDefault "still unknown"


status : Model -> ResearchInfo -> String
status model research =
  case research.status of
    Just status ->
      case model.lastTimeStamp of
        Just now ->
          let progress0 = progress status now
              duration = Time.DateTime.delta status.finish now
              durStr = Utils.deltaToString duration
          in  "research finished in: " ++ durStr ++ " (" ++ toString progress0 ++ " %)"
        Nothing ->
          "research finished at: " ++ zonedIso8601 model status.finish
    Nothing ->
      "no research in progress"


progress : ResearchStatusInfo -> Time.DateTime.DateTime -> Int
progress status now =
  let started = Time.DateTime.toTimestamp status.created
      finished = Time.DateTime.toTimestamp status.finish
      now0 = Time.DateTime.toTimestamp now
      durationMs = finished - started
      progress = now0 - started
  in  progress * 100 / durationMs |> round |> Basics.max 0 |> Basics.min 100


-- vim: et sw=2 sts=2
