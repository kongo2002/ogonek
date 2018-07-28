module Api exposing ( send, websocket )

import Json.Decode as JD
import Json.Encode as JE
import WebSocket

import Types


websocket : Types.Model -> Sub Types.Msg
websocket model =
  let ws = model.websocketHost
  in  WebSocket.listen ws parseWsJson


send : Types.Model -> Types.Request -> Cmd Types.Msg
send model msg =
  let ws = model.websocketHost
      req = toRequest msg
  in  WebSocket.send ws req


toRequest : Types.Request -> String
toRequest request =
  let json = requestEncoder request
  in  JE.encode 0 json


requestEncoder : Types.Request -> JE.Value
requestEncoder req =
  case req of
    Types.AuthorizeRequest auth ->
      JE.object
        [ ("_t", JE.string "authorize")
        , ("code", JE.string auth.code)
        , ("state", JE.string auth.state)
        , ("scope", JE.string auth.scope)
        ]


parseWsJson : String -> Types.Msg
parseWsJson payload =
  case JD.decodeString payloadDecoder payload of
    -- for now we will map the error content into the
    -- generic error response type
    Ok (Types.Error err) -> Types.ApiResponseError err.message
    Ok content -> Types.ApiResponse content
    Err error -> Types.ApiResponseError error


payloadDecoder : JD.Decoder Types.ApiContent
payloadDecoder =
  -- use the "_t" key to determine which decoder to use
  (JD.field "_t" JD.string)
  |> JD.andThen (\t ->
    case t of
      "authinfo" -> JD.map Types.Auth authInfoDecoder
      "error" -> JD.map Types.Error errorDecoder
      _ -> JD.fail ("unexpected message " ++ t))


authInfoDecoder : JD.Decoder Types.AuthInformation
authInfoDecoder =
  JD.map2 Types.AuthInformation
    (JD.field "provider" JD.string)
    (JD.field "loginUrl" JD.string)


errorDecoder : JD.Decoder Types.ApiError
errorDecoder =
  JD.map2 Types.ApiError
    (JD.field "error" JD.bool)
    (JD.field "message" JD.string)


-- vim: et sw=2 sts=2
