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
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onClick )
import Time.Iso8601

import Const
import Types exposing (..)
import Utils
import View.Login
import View.Navigation
import View.Overview
import View.Research
import View.Utils exposing (..)


view : Model -> Html Msg
view model =
  let content =
        case model.route of
          HomeRoute -> View.Overview.overview
          PlanetRoute planet ->
            case Dict.get planet model.planets of
              Just p -> homePlanet p
              Nothing -> View.Overview.overview
          LoginRoute -> View.Login.login
          ResearchRoute -> View.Research.research
          _ -> View.Overview.overview
      rows = View.Navigation.navigation model :: content model
  in
    div [ class "container" ] rows


homePlanet : ActivePlanet -> Model -> List (Html Msg)
homePlanet active model =
  let planet = active.planet
      cap = active.capacity
      prod = active.production
      name = "Planet at " ++ coordStr planet.position
      header name = th [] [ text name ]
      buildings = Dict.values active.buildings
      res = active.resources
      maxConstr = maxConcurrentConstructions active
      numConstr = Dict.size active.constructions
      toRow = buildingRow active (maxConstr > numConstr)

      desc (name, value, capacity, production) =
        let progress = value * 100 // capacity
            progStr = (toString progress) ++ "%"
            hasProduction = production > 0
            prodStr = if hasProduction then (toString production) ++ " /h" else ""
            prodTitle =
              if hasProduction then [ title prodStr ]
              else []
            prodSpan =
              if hasProduction then
                [ span [ class "mobile light" ] [ text <| " (" ++ prodStr ++ ")" ] ]
              else []
        in div ([ class "resource four columns" ] ++ prodTitle)
           [ div [ class "meter" ]
             [ h6 [ class "description" ]
               ([ text name , text ": ", numberSpan value
               ] ++ prodSpan)
             , span [ style [("width", progStr)] ] []
             ]
           ]

      constructionInfo =
        if maxConstr <= numConstr then
          div [ class "row cinfo" ]
            [ p []
              [ text ("Max. number of concurrent constructions: " ++ (toString maxConstr)) ]
            ]
        else div [] []

      filterButton filter =
        let isActive = active.buildingsFilter == filter
            name = buildingFilterName filter
            click = onClick (SetBuildingsFilter filter)
            cls =
              if isActive then
                [ class "button-primary u-full-width", click ]
              else [ class "u-full-width", click ]
        in  div [ class "four columns" ]
            [ button cls [ text name ]
            ]

      buildingFilters =
        div [ class "row" ]
        [ filterButton AllBuildings
        , filterButton AvailableBuildings
        , filterButton InProgressBuildings
        ]

      filteredBuildings =
        case active.buildingsFilter of
          AllBuildings -> buildings
          AvailableBuildings ->
            if maxConstr > numConstr then
              let possible = buildPossible res
              in  List.filter possible buildings
            else []
          InProgressBuildings ->
            let inProgress b = Dict.member b.name active.constructions
            in  List.filter inProgress buildings

      energies =
        [ (Const.workers, res.workers, 0, 0)
        , (Const.power, res.power, 0, 0)
        ]

      resourceRow1 =
        [ (Const.ironOre, res.ironOre, cap.ironOre, prod.ironOre)
        , (Const.gold, res.gold, cap.gold, prod.gold)
        , (Const.h2o, res.h2o, cap.h2o, prod.h2o)
        ]

      resourceRow2 =
        [ (Const.oil, res.oil, cap.oil, prod.oil)
        , (Const.h2, res.h2, cap.h2, prod.h2)
        , (Const.uranium, res.uranium, cap.uranium, prod.uranium)
        ]

      resourceRow3 =
        [ (Const.pvc, res.pvc, cap.pvc, prod.pvc)
        , (Const.titan, res.titan, cap.titan, prod.titan)
        , (Const.kyanite, res.kyanite, cap.kyanite, prod.kyanite)
        ]
  in
    [ h2 [] [ text name ]
    , div [ class "row" ]
      [ ul [ id "planet-description", class "no-mobile nine columns" ]
        [ li []
          [ span [ class "description" ] [ text "Type: " ]
          , span [ class "value" ] [ text <| Utils.planetToString planet.planetType ]
          ]
        , li []
          [ span [ class "description" ] [ text "Size: " ]
          , span [ class "value" ] [ text <| toString planet.size ]
          ]
        , li []
          [ span [ class "description" ] [ text "Position: " ]
          , span [ class "value" ] [ text <| coordStr planet.position ]
          ]
        ]
      , div [ class "u-pull-right three columns" ]
        [ div [ class "u-full-width text-right" ]
          [ planetImg planet
          ]
        ]
      ]
    , h3 [] [ text "Resources" ]
    , div [ id "resources" ]
      [ div [ class "row" ] (List.map desc energies)
      , div [ class "row" ] (List.map desc resourceRow1)
      , div [ class "row" ] (List.map desc resourceRow2)
      , div [ class "row" ] (List.map desc resourceRow3)
      ]
    , div [ class "row" ]
      [ h3 [] [ text "Buildings" ]
      , constructionInfo
      , buildingFilters
      , table [ id "buildings", class "table-responsive u-full-width" ]
        [ thead []
          [ tr []
            [ header Const.building
            , header Const.level
            , header Const.workers
            , header Const.power
            , header Const.ironOre
            , header Const.gold
            , header Const.h2o
            , header Const.oil
            , header Const.h2
            , header Const.uranium
            , header Const.pvc
            , header Const.titan
            , header Const.kyanite
            , header ""
            ]
          ]
        , tbody [] (List.map toRow filteredBuildings)
        ]
      ]
    ]


buildingFilterName : BuildingsFilter -> String
buildingFilterName filter =
  case filter of
    AllBuildings -> "all"
    AvailableBuildings -> "available"
    InProgressBuildings -> "in progress"


maxConcurrentConstructions : ActivePlanet -> Int
maxConcurrentConstructions planet =
  let buildings = planet.buildings
      ccLevel =
        Dict.get "construction_center" buildings
        |> Maybe.map .level
        |> Maybe.withDefault 0
  in  (ccLevel + 9) // 10


buildingRow : ActivePlanet -> Bool -> BuildingInfo -> Html Msg
buildingRow planet constrPossible binfo =
  let col label val relative =
      let dataLabel = attribute "data-label" label
          attrs =
            if val == relative then
              [ class "no-mobile", dataLabel ]
            else [ dataLabel ]
      in  td attrs [ numberSpanTo relative val ]

      res = planet.resources
      construction = Dict.get binfo.name planet.constructions

      buildColumns =
        [ col Const.workers binfo.workers res.workers
        , col Const.power binfo.power res.power
        , col Const.ironOre binfo.ironOre res.ironOre
        , col Const.gold binfo.gold res.gold
        , col Const.h2o binfo.h2o res.h2o
        , col Const.oil binfo.oil res.oil
        , col Const.h2 binfo.h2 res.h2
        , col Const.uranium binfo.uranium res.uranium
        , col Const.pvc binfo.pvc res.pvc
        , col Const.titan binfo.titan res.titan
        , col Const.kyanite binfo.kyanite res.kyanite
        ]

      columns =
        case construction of
          Just constr ->
            [ td [ class "operations", colspan 12 ] [ constructionOperation constr ] ]
          Nothing ->
            buildColumns ++
            [ td [ class "operations" ] [ buildOperation constrPossible res binfo ] ]
  in
    tr []
    ([ td [ class "building" ] [ text (translateBuilding binfo) ]
    , col Const.level binfo.level -1
    ] ++ columns)


constructionOperation : ConstructionInfo -> Html Msg
constructionOperation constr =
  let finishStr = Time.Iso8601.fromDateTime constr.finish
      finish = title ("finished: " ++ finishStr)
      durationDesc =
        case constr.timeLeft of
          Just time -> "finished in " ++ time
          Nothing -> "in construction "
  in  a [ class "icon inactive construction", finish ]
      [ span [] [ text durationDesc ]
      , span [] [ icon "gavel" ]
      ]


buildOperation : Bool -> ResourceInfo -> BuildingInfo -> Html Msg
buildOperation constrPossible res binfo =
  let possible = constrPossible && buildPossible res binfo
      buildReq = BuildBuildingRequest res.planetId binfo.name (binfo.level + 1)
      request = ApiRequest buildReq
      duration = Utils.deltaToString binfo.duration
      buildCls =
        if possible then [ href "#", numbClick request ]
        else [ class "inactive" ]
      desc =
        span [ class "description" ]
          [ text <| "Build (" ++ duration ++ ") " ]
  in  a (title duration :: class "icon" :: buildCls) [ desc, icon "cog" ]


buildPossible : ResourceInfo -> BuildingInfo -> Bool
buildPossible res info =
  res.workers >= info.workers &&
  res.power >= info.power &&
  res.ironOre >= info.ironOre &&
  res.gold >= info.gold &&
  res.h2o >= info.h2o &&
  res.oil >= info.oil &&
  res.h2 >= info.h2 &&
  res.uranium >= info.uranium &&
  res.pvc >= info.pvc &&
  res.titan >= info.titan &&
  res.kyanite >= info.kyanite


translateBuilding : BuildingInfo -> String
translateBuilding binfo =
  case binfo.name of
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


noPlanet : List (Html Msg)
noPlanet =
  [ div [ class "row" ]
    [ p [] [ text "welcome to ogonek!" ]
    ]
  ]


-- vim: et sw=2 sts=2
