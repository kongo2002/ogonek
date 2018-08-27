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

module View exposing ( view )

import Dict
import Html exposing ( Html, div )
import Html.Attributes exposing ( class )

import Types
import View.Login
import View.Navigation
import View.Overview
import View.Planet
import View.Research


view : Types.Model -> Html Types.Msg
view model =
  let content =
        case model.route of
          -- home/overview
          Types.HomeRoute ->
            View.Overview.overview
          -- planet view
          Types.PlanetRoute planet ->
            case Dict.get planet model.planets of
              Just p ->
                -- view a specific/known planet
                View.Planet.planet p
              Nothing ->
                -- unknown planet-id -> fallback to overview
                View.Overview.overview
          -- research
          Types.ResearchRoute ->
            View.Research.research
          -- login
          Types.LoginRoute ->
            View.Login.login
          -- general fallback to home/overview
          _ -> View.Overview.overview

      rows = View.Navigation.navigation model :: content model
  in
    div [ class "container" ] rows


-- vim: et sw=2 sts=2
