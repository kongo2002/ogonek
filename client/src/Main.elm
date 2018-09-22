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
import Time.TimeZones

import Api
import Notification
import Ports
import Routing
import Types exposing (..)
import Utils exposing ( orEmpty )
import View
import View.Utils exposing ( translateBuilding, translateResearch )


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
  let route = Routing.parse location
      deltaZero = Time.DateTime.DateTimeDelta 0 0 0 0 0 0 0
      research = ResearchInfo [] deltaZero Nothing
      timeZone =
        Time.TimeZones.fromName flags.defaultTimeZone
        |> Maybe.withDefault (Time.TimeZones.etc_utc ())
      model = Model route Nothing Dict.empty Dict.empty research timeZone flags.websocketHost Dict.empty Nothing
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
          changed = newRoute /= model.route
      in  if changed then
            let newModel = { model | route = newRoute }
                _ = Debug.log "got new route" newRoute
            in  newModel ! navigationChangeActions newModel
          else model ! []


    NewUrl LogoutRoute ->
      let model0 = { model | user = Nothing }
          logout = Api.send model0 LogoutRequest
          toHome = Navigation.newUrl (Routing.routeToPath HomeRoute)
      in  model0 ! [ logout, toHome ]

    NewUrl url ->
      model ! [ Navigation.newUrl (Routing.routeToPath url) ]

    Tick now ->
      let model0 = { model | lastTimeStamp = Just now }
      in  model0 ! []

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
          finished = Utils.nothing info.status && Utils.just model.research.status
          actions =
            if finished then
              let title = "ogonek: research finished"
                  body =
                    model.research.status
                    |> Maybe.andThen .name
                    |> Maybe.map translateResearch
              in [ Notification.notify Ports.notification title body Nothing ]
            else []
          updated = { model | research = info }
      in  updated ! actions

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

    ApiResponse (WeaponOrder info) ->
      let _ = Debug.log "weapon order information received" info
          model0 = updateWeaponOrder model info
      in  model0 ! []

    ApiResponse (WeaponOrderFinished planet orderId) ->
      let _ = Debug.log "weapon order finished" orderId
          model0 = removeWeaponOrder model planet orderId
      in  model0 ! []

    ApiResponse (Resources info) ->
      let _ = Debug.log "resource information received" info
          model0 = updateResources model info
      in  model0 ! []

    ApiResponse (Utilization info) ->
      let _ = Debug.log "utilization information received" info
          model0 = updateUtilization model info
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


navigationChangeActions : Model -> List (Cmd Msg)
navigationChangeActions model =
  case model.route of
    ProductionRoute planet ->
      [ Api.send model (UtilizationRequest planet)]
    _ -> []


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
      utilization = resources
      filter = AllBuildings
  in  ActivePlanet info Dict.empty Dict.empty Dict.empty resources capacity production utilization filter


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
                  body = translateBuilding info ++ " (level " ++ toString info.level ++ ") finished" |> Just
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


updateWeaponOrder : Model -> WeaponOrderInfo -> Model
updateWeaponOrder model info =
  case Dict.get info.planetId model.planets of
    Just active ->
      let orders = active.weaponOrders
          orders0 = Dict.insert info.id info orders
          updated = { active | weaponOrders = orders0 }
          planets0 = Dict.insert info.planetId updated model.planets
      in  { model | planets = planets0 }
    Nothing ->
      model


removeWeaponOrder : Model -> String -> String -> Model
removeWeaponOrder model planetId orderId =
  case Dict.get planetId model.planets of
    Just active ->
      let orders = active.weaponOrders
          orders0 = Dict.remove orderId orders
          updated = { active | weaponOrders = orders0 }
          planets0 = Dict.insert planetId updated model.planets
      in  { model | planets = planets0 }
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


updateUtilization : Model -> ResourceInfo -> Model
updateUtilization model info =
  case Dict.get info.planetId model.planets of
    Just active ->
      let updated = { active | utilization = info }
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
