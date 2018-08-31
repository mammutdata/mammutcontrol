import "../node_modules/materialize-css/dist/js/materialize.min.js";
import Main from "./Main.purs";

Main.main();

// This fails saying that init is not a function, though buttons are not working
// properly if it is not evaluated.
window.Waves.init();
