import { Elm } from "./src/Main.elm";

Elm.Main.init({ node: document.getElementById("elm") });

// TODO: Is there a better (preferably Elm-only) way to do this?
window.addEventListener(
  "keydown",
  function(e) {
    if ([32, 37, 38, 39, 40].indexOf(e.keyCode) > -1) {
      e.preventDefault();
    }
  },
  false
);
