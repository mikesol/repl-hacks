import "./style.css";
var main = require("../output/Main/index.js");
import psci from "../.psci_modules/node_modules/$PSCI/index.js";
main.main();

if (module.hot) {
  module.hot.accept("../.psci_modules/node_modules/$PSCI/index.js", function() {
    console.log('Accepting the updated module!', psci.it);
  })
}