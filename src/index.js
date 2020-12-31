"use strict";

require("../index.html");
require("bulma/bulma.sass");

var Elm = require("./Main.elm").Elm;

var app = Elm.Main.init({
  node: document.getElementById("main"),
});
