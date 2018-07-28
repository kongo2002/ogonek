module Main exposing ( main )

import Debug
import Html

import Api
import Types exposing (..)
import View


init : Flags -> ( Model, Cmd Msg )
init flags =
  let auth  = AuthInformation "local" "#login"
      model = Model Nothing auth flags.websocketHost
  in  model ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      model ! []
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
  Html.programWithFlags
    { init = init
    , view = View.view
    , update = update
    , subscriptions = Api.websocket
    }


-- vim: et sw=2 sts=2
