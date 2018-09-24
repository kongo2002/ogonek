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
import Html exposing ( Html, div, p, text )
import Html.Attributes exposing ( class )

import Types
import View.Building
import View.Buildings
import View.Login
import View.Navigation
import View.Overview
import View.Planet
import View.Production
import View.Research
import View.Utils exposing ( loggedIn )
import View.Weapons


view : Types.Model -> Html Types.Msg
view model =
  let loggedOutContent =
        case model.route of
          Types.HomeRoute ->
            home
          Types.LoginRoute ->
            View.Login.login
          _ ->
            home

      withPlanet view0 planet =
        case Dict.get planet model.planets of
          -- view a specific/known planet
          Just pl -> view0 pl
          -- unknown planet-id -> fallback to overview
          Nothing -> View.Overview.overview

      loggedInContent =
        case model.route of
          -- home/overview
          Types.HomeRoute ->
            View.Overview.overview
          -- planet view
          Types.PlanetRoute planet ->
            withPlanet View.Planet.planet planet
          -- buildings view
          Types.BuildingsRoute planet ->
            withPlanet View.Buildings.buildings planet
          -- weapons
          Types.WeaponsRoute planet ->
            withPlanet View.Weapons.weapons planet
          -- production
          Types.ProductionRoute planet ->
            withPlanet View.Production.production planet
          -- building
          Types.BuildingRoute building ->
            View.Building.building building
          -- research
          Types.ResearchRoute ->
            View.Research.research
          -- login
          Types.LoginRoute ->
            View.Login.login
          -- general fallback to home/overview
          _ ->
            View.Overview.overview

      content =
        if loggedIn model then loggedInContent
        else loggedOutContent

      rows = View.Navigation.navigation model :: content model
  in
    div [ class "container" ] rows


home : Types.Model -> List (Html Types.Msg)
home _ =
  [ div []
    [ p [] [ text "Welcome to ogonek!" ]
    ]
  ]


-- vim: et sw=2 sts=2
