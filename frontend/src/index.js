'use strict';

require('./index.html');
var Elm = require('./Main');

var elm = Elm.Main.fullscreen();

var storageKey = "token";
elm.ports.save.subscribe(function(value) {
  localStorage.setItem(storageKey, value);
});
elm.ports.doload.subscribe(function() {
  elm.ports.load.send(localStorage.getItem(storageKey));
});

