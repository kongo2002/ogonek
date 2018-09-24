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

module View.Weapons exposing ( weapons )

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)

import Const
import Types exposing (..)
import Utils exposing ( buildingLevel )
import View.Utils exposing (..)


weapons : ActivePlanet -> Model -> List (Html Msg)
weapons active model =
  let planet = active.planet
      name = planetName planet

      content =
        if Dict.isEmpty active.weapons then
          [ div [ class "row" ]
            [ p [] [ text "no weapon production possible yet" ] ]
          ]
        else
          weaponsTable active model
  in  [ h2 [] [ text name ]
      ] ++ content


weaponsTable : ActivePlanet -> Model -> List (Html Msg)
weaponsTable active model =
  let planet = active.planet
      res = active.resources
      header name = th [] [ text name ]

      col0 label content =
        let attr = attribute "data-label" label
        in  td [ attr ] [ content ]

      col label value relative =
        let val = numberSpanTo relative value
        in  col0 label val

      ordersPossible =
        maxOrders active > Dict.size active.weaponOrders

      operation = weaponOperation active ordersPossible

      row info =
        tr []
        [ td [ class "header" ] [ text info.name ]
        , col "Count" info.count -1
        , col "Space" info.space -1
        , col "Power" info.power -1
        , col0 "Damage" (text (toString info.damage))
        , col "Load" info.load -1
        , col Const.ironOre info.ironOre res.ironOre
        , col Const.gold info.gold res.gold
        , col Const.h2o info.h2o res.h2o
        , col Const.oil info.oil res.oil
        , col Const.h2 info.h2 res.h2
        , col Const.uranium info.uranium res.uranium
        , col Const.pvc info.pvc res.pvc
        , col Const.titan info.titan res.titan
        , col Const.kyanite info.kyanite res.kyanite
        , td [] [ operation info ]
        ]

      rows =
        active.weapons
        |> Dict.values
        |> List.sortBy .damage
        |> List.map row
  in
    [ h3 [] [ text "Weapons" ]
    , table [ id "weapons", class "table-responsive table-resources u-full-width" ]
      [ thead []
        [ tr []
          [ header "Name"
          , header "Count"
          , header "Space"
          , header "Power"
          , header "Damage"
          , header "Load"
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
      , tbody [] rows
      ]
    ]


weaponOperation : ActivePlanet -> Bool -> WeaponInfo -> Html Msg
weaponOperation planet ordersAvailable info =
  let id = planet.planet.id
      weapon = info.name
      res = planet.resources
      possible =
        ordersAvailable &&
        info.ironOre <= res.ironOre &&
        info.gold <= res.gold &&
        info.h2o <= res.h2o &&
        info.oil <= res.oil &&
        info.h2 <= res.h2 &&
        info.uranium <= res.uranium &&
        info.pvc <= res.pvc &&
        info.titan <= res.titan &&
        info.kyanite <= res.kyanite

      cls =
        if possible then [ href "#", numbClick (ApiRequest (BuildWeaponRequest id weapon)) ]
        else [ class "inactive" ]

      clss = class "icon" :: cls

  in  a clss [ icon "cog" ]


maxOrders : ActivePlanet -> Int
maxOrders planet =
  let manufactureLevel = buildingLevel planet "weapon_manufacture"
  in  if manufactureLevel >= 50 then 3
      else if manufactureLevel >= 15 then 2
      else if manufactureLevel >= 1 then 1
      else 0


-- vim: et sw=2 sts=2
