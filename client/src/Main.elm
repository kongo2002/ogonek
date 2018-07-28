module Main exposing ( main )

import Debug
import Navigation

import Api
import Types exposing (..)
import Routing
import View


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
  let auth  = AuthInformation "local" "#login"
      route = Routing.parse location
      model = Model route Nothing auth flags.websocketHost
  in  model ! []


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
    ApiRequest msg ->
      model ! []
    ApiResponseError error ->
      let _ = Debug.log "error with API response" error
      in  model ! []
    ApiResponse (Auth info) ->
      let _ = Debug.log "auth information received" info
          model0 = { model | auth = info }
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
