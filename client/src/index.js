'use strict';

require('./index.html');

/* build websocket target host based on current location */
var wsHost = ((window.location.protocol === "https:") ? "wss://" : "ws://") + window.location.host + "/.ws"
var elm = require('./Main.elm');
var app = elm.Main.fullscreen({websocketHost: wsHost});
