/*
 * All ports functionality goes through here!
 */
function addPorts(elmApp) {

  /*** ELM TO JS PORT ***/

  if (elmApp.ports && elmApp.ports.elmToJs) {
    elmApp.ports.elmToJs.subscribe(listen);
  } else {
    console.log("ELM to JS port NOT hooked up on the ELM side!")
  }

  var listeners = {};

  function listen([msg, value]) {
    if (listeners[msg]) {
      listeners[msg](value);
    } else {
      console.log("JS received junk msg from ELM: ", msg)
    }
  }

  /*** JS TO ELM PORT ***/

  function send(msg, value) {
    if (elmApp.ports && elmApp.ports.jsToElm) {
      elmApp.ports.jsToElm.send([msg, value]);
    } else {
      console.log(msg + ": JS TO ELM port NOT hooked up on the ELM side!")
    }
  }

  /*** CONSOLE LOG PORT LOGIC ***/
  listeners["Ports.Log.string"] =
    function(string) {
      console.log(string);
    }

  /*** LOCAL STORAGE PORT LOGIC ***/

  // sends updated storage data back to elm on demand.
  listeners["Ports.LocalStorage.request"] =
    function(key) {
      var value = localStorage.getItem(key);
      send("Ports.LocalStorage.listen." + key, JSON.parse(value))
    };

  // saves storage data on demand.
  listeners["Ports.LocalStorage.save"] =
    function({ key, value }) {
      localStorage.setItem(key, JSON.stringify(value));
      listeners["Ports.LocalStorage.request"](key);
    };

  /*** SCREEN PORT LOGIC ***/
  window.addEventListener("resize",
    function() {
      send("Ports.Screen.onResize", {
        width: window.screen.width,
        height: window.screen.height
      })
    });
}
