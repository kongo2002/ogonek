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

module View.Building exposing ( building )

import Html exposing (..)
import Html.Attributes exposing (..)

import Types exposing (..)
import View.Utils exposing ( translateBuildingName )


building : String -> Model -> List (Html Msg)
building name model =
  let cnt = content name
      translated = translateBuildingName name
  in  [ h2 [] [ text "Building" ]
      , h3 [] [ text translated ]
      , div [ class "row building" ] cnt
      ]


content : String -> List (Html Msg)
content name =
  case name of
    "construction_center" -> constructionCenter
    unknown ->
      [ p [] [ text "unknown building" ] ]


constructionCenter : List (Html Msg)
constructionCenter =
  let short = "The construction center is the base of your colony - every construction is managed in this facility."
      desc = "By expanding your construction center you will gain up to three concurrent construction slots - at level 15 and 50. Moreover each ten levels of your construction center you get the possibility to build one more colonization ship that has the ability to colonize additional planets."
  in  [ p [] [ text short ]
      , p [] [ text desc ]
      ]


-- vim: et sw=2 sts=2
