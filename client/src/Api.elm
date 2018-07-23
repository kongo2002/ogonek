module Api exposing ( send, websocket )

import Json.Decode as JD
import WebSocket

import Types


websocket : Types.Model -> Sub Types.Msg
websocket model =
  let ws = model.websocketHost
  in  WebSocket.listen ws parseWsJson


send : Types.Model -> String -> Cmd Types.Msg
send model msg =
  let ws = model.websocketHost
  in  WebSocket.send ws msg


parseWsJson : String -> Types.Msg
parseWsJson payload =
  case JD.decodeString payloadDecoder payload of
    Ok content -> Types.ApiResponse content
    Err error -> Types.ApiResponseError error


payloadDecoder : JD.Decoder Types.ApiContent
payloadDecoder =
  -- use the "_t" key to determine which decoder to use
  (JD.field "_t" JD.string)
  |> JD.andThen (\t ->
    case t of
      "auth" -> JD.map Types.Auth authDecoder
      _ -> JD.fail ("unexpected message " ++ t))


authDecoder : JD.Decoder Types.AuthInformation
authDecoder =
  JD.map2 Types.AuthInformation
    (JD.field "provider" JD.string)
    (JD.field "loginUrl" JD.string)


-- vim: et sw=2 sts=2
