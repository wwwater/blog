'use strict';

require('./index.html');
var Elm = require('./Main');

var elm = Elm.Main.fullscreen();


var storageKey = "token";

elm.ports.save.subscribe(function(value) {
  console.log("save port called with", value);
  localStorage.setItem(storageKey, value);
});

elm.ports.doload.subscribe(function() {
  console.log("doload port called");
  var storageValue = localStorage.getItem(storageKey);
  if (storageValue) {
    console.log("found value in local storage, trigger load port with", storageValue);
    elm.ports.load.send(storageValue);
  }
});

elm.ports.remove.subscribe(function() {
  console.log("remove port called");
  localStorage.removeItem(storageKey);
});

