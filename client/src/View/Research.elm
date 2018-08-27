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

module View.Research exposing ( research )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onClick )
import Time.Iso8601

import Types exposing (..)


research : Model -> List (Html Msg)
research model =
  let res = model.research
      row (name, level) =
        tr []
        [ td [] [ text name ]
        , td [] [ text (toString level) ]
        ]

      researchStatus =
        case res.status of
          Just status ->
            div []
            [ p [] [ text ("research finished at: " ++ Time.Iso8601.fromDateTime status.finish) ]
            ]
          Nothing ->
            div []
            [ p [] [ text "no current research in progress" ]
            , button [ onClick (ApiRequest StartResearchRequest) ] [ text "Research" ]
            ]

  in  [ h2 [] [ text "Research" ]
      , researchStatus
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


-- vim: et sw=2 sts=2
