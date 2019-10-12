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

var elmWebsocket = (function() {
  var inst = {};

  /* build websocket target host based on current location */
  inst.wsHost = ((window.location.protocol === "https:") ? "wss://" : "ws://") + window.location.host + "/.ws"
  inst.socket = undefined;

  inst.connect = function(app) {
    var toElm = app.ports.fromWebsocket;
    inst.socket = new WebSocket(inst.wsHost);

    inst.socket.onopen = inst.openHandler.bind(null, toElm, inst.socket, inst.wsHost);
    inst.socket.onmessage = inst.messageHandler.bind(null, toElm, inst.socket, inst.wsHost);
    inst.socket.onerror = inst.errorHandler.bind(null, toElm, inst.socket, inst.wsHost);
    inst.socket.onclose = inst.closeHandler.bind(null, toElm, inst.socket, inst.wsHost);
  }

  inst.send = function(msg) {
    if (inst.socket) {
      var text = typeof msg == 'string' ? msg : JSON.stringify(msg);
      inst.socket.send(text);
    } else {
      if (console) {
        console.log('no open socket - cannot send message "' + msg + '"');
      }
    }
  }

  inst.openHandler = function(toElm, socket, url, event) {
    toElm.send({
      type: 'connected',
      msg: {
        url: url
      }
    });
  }

  inst.messageHandler = function(toElm, socket, url, event) {
    if (typeof event.data === 'string') {
      var payload = JSON.parse(event.data);
      toElm.send({
        type: 'send',
        msg: {
          url: url,
          data: payload
        }
      });
    } else {
      if (console) {
        console.log('binary messages are not implemented');
      }
    }
  }

  inst.errorHandler = function(toElm, socket, url, event) {
    toElm.send({
      type: 'error',
      msg: {
        url: url,
        code: event.code
      }
    });
  }

  inst.closeHandler = function(toElm, socket, url, event) {
    toElm.send({
      type: 'closed',
      msg: {
        url: url,
        unsentBytes: socket.bufferedAmount
      }
    });
  }

  return inst;
})();


/* try to determine the browser's timezone to use as a default at least */
var timeZone = Intl.DateTimeFormat().resolvedOptions().timeZone || '';


var elm = require('./Main.elm');
var app = elm.Main.fullscreen({
  defaultTimeZone: timeZone,
});

/* port to open notifications */
app.ports.notification.subscribe(function(data) {
  if (data.title) {
    elmNotify.notify(data.title, data.body, data.img);
  } else {
    /* we are going to recycle this subscription in order to *just*
     * initialize the notifications after successful login:
     * an empty 'title' indicates we just want to request permissions */
    if (elmNotify.isAvailable) {
      elmNotify.requestPermission(function() {});
    }
  }
});

/* port to send commands to the websocket */
app.ports.toWebsocket.subscribe(function (data) {
  switch (data.type) {
    case 'connect':
      elmWebsocket.connect(app);
      break;
    case 'send':
      elmWebsocket.send(data.msg);
      break;
    default:
      if (console) {
        console.log('unknown message type: ' + data.type);
      }
      break;
  }
});

/* vim:set et sw=2 sts=2: */
