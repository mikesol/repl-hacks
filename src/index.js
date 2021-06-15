import "./style.css";
import main from "../.psci_modules/node_modules/Main/index.js";
import psci from "../.psci_modules/node_modules/$PSCI/index.js";
main.main();

if (module.hot) {
  module.hot.accept("../.psci_modules/node_modules/$PSCI/index.js", function() {
    console.log('Accepting the updated module!', psci.it);
    window.__w4g$_it = psci.it;
  })
}