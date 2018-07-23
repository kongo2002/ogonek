'use strict';

require('./index.html');

import './css/normalize.css';
import './css/skeleton.css';
import './css/custom.css';

/* build websocket target host based on current location */
var wsHost = ((window.location.protocol === "https:") ? "wss://" : "ws://") + window.location.host + "/.ws"
var elm = require('./Main.elm');
var app = elm.Main.fullscreen({websocketHost: wsHost});
