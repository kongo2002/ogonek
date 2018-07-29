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

module Main exposing ( main )

import Debug
import Navigation

import Api
import Types exposing (..)
import Routing
import View


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
  let auth  = AuthInformation "local" "/auth"
      route = Routing.parse location
      model = Model route Nothing auth flags.websocketHost
      actions = routeActions model
  in  model ! actions


routeActions : Model -> List (Cmd Msg)
routeActions model =
  case model.route of
    AuthRoute (Just code) state scope ->
      let auth = Authorize code (orEmpty state) (orEmpty scope)
          req = AuthorizeRequest auth
          emptyAuth = AuthRoute Nothing Nothing Nothing
          -- we want to immediately reset the url query parameters:
          -- this makes for a cleaner impression and is more robust
          -- against accidential 'reloading' of the site that
          -- might trigger a failing authentication
          resetLocation = Navigation.newUrl (Routing.routeToPath emptyAuth)
      in  [ Api.send model req, resetLocation ]
    _ -> []


orEmpty : Maybe String -> String
orEmpty value =
  case value of
    Just str -> str
    Nothing -> ""


atAuth : Route -> Bool
atAuth route =
  case route of
    AuthRoute _ _ _ -> True
    _ -> False


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      model ! []

    NavigationChange location ->
      let newRoute = Routing.parse location
          _ = Debug.log "got new route" newRoute
          model0 = { model | route = newRoute }
      in  model0 ! []

    NewUrl url ->
      model ! [ Navigation.newUrl url ]

    ApiRequest msg ->
      model ! [ Api.send model msg ]

    ApiResponseError error ->
      let _ = Debug.log "error with API response" error
      in  model ! []

    ApiResponse (Auth info) ->
      let _ = Debug.log "auth information received" info
          model0 = { model | authInfo = info }
      in  model0 ! []

    ApiResponse (User info) ->
      let _ = Debug.log "user information received" info
          model0 = { model | user = Just info}
      in  model0 ! []

    ApiResponse cnt ->
      let _ = Debug.log "api content received" cnt
      in  model ! []


main : Program Flags Model Msg
main =
  Navigation.programWithFlags NavigationChange
    { init = init
    , view = View.view
    , update = update
    , subscriptions = Api.websocket
    }


-- vim: et sw=2 sts=2
