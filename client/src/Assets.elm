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


module Assets exposing (..)

import Types



-- this is basically a preparation to use the 'elm-assets-loader'
-- at some point when it supports the current version of webpack


type Asset
    = Asset String


resourceAsset : Types.ResourceType -> Asset
resourceAsset res =
    case res of
        Types.IronOre ->
            ironOreAsset

        Types.Gold ->
            goldAsset

        Types.H2O ->
            waterAsset

        Types.Oil ->
            oilAsset

        Types.H2 ->
            hydrogenAsset

        Types.Uranium ->
            uraniumAsset

        Types.PVC ->
            pvcAsset

        Types.Titan ->
            titanAsset

        Types.Kyanite ->
            kyaniteAsset

        Types.Workers ->
            workerAsset

        Types.Power ->
            powerAsset


ironOreAsset : Asset
ironOreAsset =
    Asset "iron_ore.png"


goldAsset : Asset
goldAsset =
    Asset "gold.png"


waterAsset : Asset
waterAsset =
    Asset "water.png"


oilAsset : Asset
oilAsset =
    Asset "oil.png"


hydrogenAsset : Asset
hydrogenAsset =
    Asset "h2.png"


uraniumAsset : Asset
uraniumAsset =
    Asset "uranium.png"


pvcAsset : Asset
pvcAsset =
    Asset "pvc.png"


titanAsset : Asset
titanAsset =
    Asset "titan.png"


kyaniteAsset : Asset
kyaniteAsset =
    Asset "kyanite.png"


workerAsset : Asset
workerAsset =
    Asset "worker.png"


powerAsset : Asset
powerAsset =
    Asset "power.png"


planetAsset : Types.PlanetType -> Asset
planetAsset ptype =
    case ptype of
        Types.EarthPlanet ->
            earthPlanet

        Types.FirePlanet ->
            firePlanet

        Types.WaterPlanet ->
            waterPlanet

        Types.IcePlanet ->
            icePlanet


waterPlanet : Asset
waterPlanet =
    Asset "neptune.png"


earthPlanet : Asset
earthPlanet =
    Asset "pluto.png"


icePlanet : Asset
icePlanet =
    Asset "uranus.png"


firePlanet : Asset
firePlanet =
    Asset "mars.png"


physics : Asset
physics =
    Asset "physics.png"


path : Asset -> String
path (Asset str) =
    "/static/" ++ str



-- vim: et sw=2 sts=2
