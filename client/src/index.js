'use strict';

import './index.html';

import './css/normalize.css';
import './css/skeleton.css';
import './css/custom.css';

var elmNotify = (function() {
  var inst = {};

  inst.isAvailable = typeof(Notification) == 'function';

  inst.requestPermission = function(cb) {
    var permission = Notification.permission;
    if (permission != 'default') {
      cb(permission);
    } else {
      Notification.requestPermission(cb);
    }
  };

  inst.notify = function(title, body, img) {
    if (this.isAvailable) {
      this.requestPermission(function(permission) {
        if (permission == 'granted') {
          var options = {};
          if (body) { options.body = body };
          if (img) { options.icon = img };
          var notification = new Notification(title, options);
        }
      });
    }
  };

  return inst;
})();

/* build websocket target host based on current location */
var wsHost = ((window.location.protocol === "https:") ? "wss://" : "ws://") + window.location.host + "/.ws"
var elm = require('./Main.elm');
var app = elm.Main.fullscreen({websocketHost: wsHost});

app.ports.notification.subscribe(function(data) {
  elmNotify.notify(data.title, data.body, data.img);
});

/* vim:set et sw=2 sts=2: */
