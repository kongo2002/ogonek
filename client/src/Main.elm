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
import Dict
import Navigation
import Time
import Time.DateTime exposing ( DateTime )

import Api
import Notification
import Ports
import Routing
import Types exposing (..)
import Utils
import View


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
  let route = Routing.parse location
      research = ResearchInfo [] Nothing
      model = Model route Nothing Dict.empty Dict.empty research flags.websocketHost Dict.empty Nothing
      actions = routeActions model
  in  model ! actions


routeActions : Model -> List (Cmd Msg)
routeActions model =
  case model.route of
    AuthRoute (Just code) state scope provider ->
      let auth = Authorize code (orEmpty state) (orEmpty scope) (orEmpty provider)
          req = AuthorizeRequest auth
          emptyAuth = AuthRoute Nothing Nothing Nothing Nothing
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
    AuthRoute _ _ _ _ -> True
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

    NewUrl LogoutRoute ->
      let model0 = { model | user = Nothing }
          logout = Api.send model0 LogoutRequest
          toHome = Navigation.newUrl (Routing.routeToPath HomeRoute)
      in  model0 ! [ logout, toHome ]

    NewUrl url ->
      model ! [ Navigation.newUrl (Routing.routeToPath url) ]

    Tick now ->
      let model0 = { model | lastTimeStamp = Just now }
          model1 = updateConstructionTimes model0 now
      in  model1 ! []

    FormContent key value ->
      let forms = Dict.insert key value model.formContents
          model0 = { model | formContents = forms }
      in  model0 ! []

    LocalLogin ->
      let get key = Dict.get key model.formContents |> orEmpty
          user = get "localUserInput"
          code = get "localPasswordInput"
          auth = Authorize user code "" "local"
          req = AuthorizeRequest auth
      in  model ! [ Api.send model req ]

    SetBuildingsFilter filter ->
      let model0 =
            case currentPlanet model of
              Just active ->
                let updated = { active | buildingsFilter = filter }
                    planets0 = Dict.insert active.planet.id updated model.planets
                in { model | planets = planets0 }
              Nothing -> model
      in  model0 ! []

    ApiRequest msg ->
      model ! [ Api.send model msg ]

    ApiResponseError error ->
      let _ = Debug.log "error with API response" error
      in  model ! []

    ApiResponse (Auth info) ->
      let _ = Debug.log "auth information received" info
          auths = model.authInfo
          auths0 = Dict.insert info.provider info auths
          model0 = { model | authInfo = auths0 }
      in  model0 ! []

    ApiResponse (Research info) ->
      let _ = Debug.log "research information received" info
          updated = { model | research = info }
      in  updated ! []

    ApiResponse (Building info) ->
      let _ = Debug.log "building information received" info
          updated = updateBuilding model info
      in  updated

    ApiResponse (Capacity info) ->
      let _ = Debug.log "capacity information received" info
          model0 = updateCapacity model info
      in  model0 ! []

    ApiResponse (Construction info) ->
      let _ = Debug.log "construction information received" info
          model0 = updateConstruction model info
      in  model0 ! []

    ApiResponse (Production info) ->
      let _ = Debug.log "production information received" info
          model0 = updateProduction model info
      in  model0 ! []

    ApiResponse (Resources info) ->
      let _ = Debug.log "resource information received" info
          model0 = updateResources model info
      in  model0 ! []

    ApiResponse (Planet info) ->
      let _ = Debug.log "planet information received" info
          model0 = updatePlanet model info
      in  model0 ! []

    ApiResponse (User info) ->
      let _ = Debug.log "user information received" info
          model0 = { model | user = Just info }
          actions = requestPlanetInfo model0
          notifications = Notification.init Ports.notification
          actions0 = notifications :: actions ++ afterLogin model
      in  model0 ! actions0

    ApiResponse cnt ->
      let _ = Debug.log "api content received" cnt
      in  model ! []


updatePlanet : Model -> PlanetInfo -> Model
updatePlanet model info =
  let updated =
        case Dict.get info.id model.planets of
          Just active -> { active | planet = info }
          Nothing -> initialPlanet info
      planets0 = Dict.insert info.id updated model.planets
  in { model | planets = planets0 }


afterLogin : Model -> List (Cmd Msg)
afterLogin model =
  case model.route of
    LoginRoute ->
      [ Navigation.newUrl (Routing.routeToPath HomeRoute) ]
    _ -> []


requestPlanetInfo : Model -> List (Cmd Msg)
requestPlanetInfo model =
  if Dict.isEmpty model.planets then
    [ Api.send model PlanetInfoRequest ]
  else []


initialPlanet : PlanetInfo -> ActivePlanet
initialPlanet info =
  let planetId = info.id
      resources = emptyResources planetId
      capacity = emptyCapacity planetId
      production = resources
      filter = AllBuildings
  in  ActivePlanet info Dict.empty Dict.empty resources capacity production filter


updateCapacity : Model -> CapacityInfo -> Model
updateCapacity model info =
  case Dict.get info.planetId model.planets of
    Just active ->
      let updated = { active | capacity = info }
          planets0 = Dict.insert info.planetId updated model.planets
      in  { model | planets = planets0 }
    Nothing ->
      model


updateProduction : Model -> ResourceInfo -> Model
updateProduction model info =
  case Dict.get info.planetId model.planets of
    Just active ->
      let updated = { active | production = info }
          planets0 = Dict.insert info.planetId updated model.planets
      in  { model | planets = planets0 }
    Nothing ->
      model


updateBuilding : Model -> BuildingInfo -> ( Model, Cmd Msg )
updateBuilding model info =
  case Dict.get info.planetId model.planets of
    Just active ->
      let buildings = active.buildings
          buildings0 = Dict.insert info.name info buildings
          -- any building information with a greater or equal level
          -- will remove any pending construction entries
          constructions0 = removeConstruction active info
          updated = { active | buildings = buildings0, constructions = constructions0 }
          -- if this building update removed at least one construction this
          -- means we just finished a building:
          -- send a push notification in that case
          finished = Dict.size constructions0 < Dict.size active.constructions
          actions =
            if finished then
              let title = "ogonek: building finished"
                  body = info.name ++ " (level " ++ toString info.level ++ ") finished" |> Just
              in [ Notification.notify Ports.notification title body Nothing ]
            else []
          planets0 = Dict.insert info.planetId updated model.planets
      in  { model | planets = planets0 } ! actions
    Nothing ->
      model ! []


removeConstruction : ActivePlanet -> BuildingInfo -> Dict.Dict String ConstructionInfo
removeConstruction planet info =
  let btype = info.name
      constr = Dict.get btype planet.constructions
  in case constr of
      Just c ->
        if c.level <= info.level then
          Dict.remove btype planet.constructions
        else
          planet.constructions
      Nothing -> planet.constructions


updateConstructionTimes : Model -> DateTime -> Model
updateConstructionTimes model now =
  let toDelta name info =
      let delta = Time.DateTime.delta info.finish now
          diffStr = Utils.deltaToString delta
      in { info | timeLeft = Just diffStr }
  in case currentPlanet model of
    Just active ->
      let cs = Dict.map toDelta active.constructions
          updated = { active | constructions = cs }
          planets0 = Dict.insert active.planet.id updated model.planets
      in  { model | planets = planets0 }
    Nothing ->
      model


currentPlanet : Model -> Maybe ActivePlanet
currentPlanet model =
  case model.route of
    PlanetRoute planet ->
      Dict.get planet model.planets
    _ -> Nothing


updateConstruction : Model -> ConstructionInfo -> Model
updateConstruction model info =
  case Dict.get info.planetId model.planets of
    Just active ->
      if constructionValid active info then
        let constructions = active.constructions
            constructions0 = Dict.insert info.building info constructions
            updated = { active | constructions = constructions0 }
            planets0 = Dict.insert info.planetId updated model.planets
        in  { model | planets = planets0 }
      else
        model
    Nothing ->
      model


-- this check helps with idempotency of concurrent
-- building and construction updates
constructionValid : ActivePlanet -> ConstructionInfo -> Bool
constructionValid planet info =
  let binfo = Dict.get info.building planet.buildings
      level = info.level
  in case binfo of
      Just building ->
        building.level + 1 == level
      Nothing ->
        False


emptyResources : String -> ResourceInfo
emptyResources planet =
  ResourceInfo 0 0 0 0 0 0 0 0 0 0 0 planet


emptyCapacity : String -> CapacityInfo
emptyCapacity planet =
  CapacityInfo 0 0 0 0 0 0 0 0 0 planet


updateResources : Model -> ResourceInfo -> Model
updateResources model info =
  case Dict.get info.planetId model.planets of
    Just active ->
      let updated = { active | resources = info }
          planets0 = Dict.insert info.planetId updated model.planets
      in  { model | planets = planets0 }
    Nothing ->
      model


main : Program Flags Model Msg
main =
  let toTick time = Tick (Time.DateTime.fromTimestamp time)
      tenSeconds = Time.second * 10
      timeTick = Time.every tenSeconds toTick
      subs model =
        Sub.batch [ Api.websocket model, timeTick ]
  in Navigation.programWithFlags NavigationChange
      { init = init
      , view = View.view
      , update = update
      , subscriptions = subs
      }


-- vim: et sw=2 sts=2
