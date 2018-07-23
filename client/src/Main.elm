module Main exposing ( main )

import Debug
import Html

import Api
import Types exposing (..)
import View


init : Flags -> ( Model, Cmd Msg )
init flags =
  let model = Model Nothing flags.websocketHost
  in  model ! [Api.send model "foo bar"]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      model ! []
    ApiRequest msg ->
      model ! []
    ApiResponse msg ->
      Debug.log msg (model ! [])


main : Program Flags Model Msg
main =
  Html.programWithFlags
    { init = init
    , view = View.view
    , update = update
    , subscriptions = Api.websocket
    }


-- vim: et sw=2 sts=2
