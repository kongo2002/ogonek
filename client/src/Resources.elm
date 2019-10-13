-- Copyright 2019 Gregor Uhlenheuer
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


module Resources exposing (..)

import Types


resourceKey : Types.ResourceType -> String
resourceKey typ =
    case typ of
        Types.IronOre ->
            "iron_ore"

        Types.Gold ->
            "gold"

        Types.H2O ->
            "h2o"

        Types.Oil ->
            "oil"

        Types.H2 ->
            "h2"

        Types.Uranium ->
            "uranium"

        Types.PVC ->
            "pvc"

        Types.Titan ->
            "titan"

        Types.Kyanite ->
            "kyanite"

        Types.Workers ->
            "workers"

        Types.Power ->
            "power"
