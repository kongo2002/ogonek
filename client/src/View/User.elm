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

module View.User exposing ( user )

import Html exposing (..)
import Html.Attributes exposing (..)

import Routing
import Types exposing (..)
import View.Utils exposing ( icon, numbClick, toRight )


user : Model -> List (Html Msg)
user model =
  let name =
      model.user
      |> Maybe.map .name
      |> Maybe.withDefault ""
      route = Routing.routeToPath LogoutRoute
      logout =
          a [ href route, numbClick (NewUrl LogoutRoute) ] [ icon "sign-out-alt" ]
  in  [ h2 [] [ text "User" ]
      , h3 []
        [ text name, span [ toRight ] [ logout ] ]
      , div [ class "row building" ] []
      ]

-- vim: et sw=2 sts=2
