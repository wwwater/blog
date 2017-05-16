"use strict";

require("./index.html");
var Elm = require("./Main");

var storageKey = "token";

var elm = Elm.Main.fullscreen({jwt: localStorage.getItem(storageKey) || ""});


elm.ports.save.subscribe(function(value) {
  console.log("save port called with", value);
  localStorage.setItem(storageKey, value);
});

elm.ports.remove.subscribe(function() {
  console.log("remove port called");
  localStorage.removeItem(storageKey);
});

