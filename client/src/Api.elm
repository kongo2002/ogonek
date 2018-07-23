module Api exposing ( send, websocket )

import WebSocket

import Types


websocket : Types.Model -> Sub Types.Msg
websocket model =
  WebSocket.listen wsAddress Types.ApiResponse


send : String -> Cmd Types.Msg
send msg =
  WebSocket.send wsAddress msg


wsAddress : String
wsAddress = "ws://localhost:8000/.ws"


-- vim: et sw=2 sts=2
