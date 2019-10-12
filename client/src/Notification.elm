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


module Notification exposing (init, notify)

import Json.Encode as JE
import Ports


init : Ports.NotificationPort msg -> Cmd msg
init notifyPort =
    -- an empty title indicates to *just* request notification permissions
    notify notifyPort "" Nothing Nothing


notify : Ports.NotificationPort msg -> String -> Maybe String -> Maybe String -> Cmd msg
notify notifyPort title body imageUrl =
    let
        orNull =
            Maybe.map JE.string >> Maybe.withDefault JE.null

        body0 =
            orNull body

        image =
            orNull imageUrl

        value =
            JE.object
                [ ( "title", JE.string title )
                , ( "body", body0 )
                , ( "image", image )
                ]
    in
    notifyPort value



-- vim: et sw=2 sts=2
