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
      in  [Api.send model req]
    _ -> []


orEmpty : Maybe String -> String
orEmpty value =
  case value of
    Just str -> str
    Nothing -> ""


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
