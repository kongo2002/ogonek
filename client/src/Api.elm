module Api exposing ( send, websocket )

import WebSocket

import Types


websocket : Types.Model -> Sub Types.Msg
websocket model =
  let ws = model.websocketHost
  in  WebSocket.listen ws Types.ApiResponse


send : Types.Model -> String -> Cmd Types.Msg
send model msg =
  let ws = model.websocketHost
  in  WebSocket.send ws msg


-- vim: et sw=2 sts=2
