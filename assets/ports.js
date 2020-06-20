/*
 * All ports functionality goes through here!
 */
function addPorts(elmApp) {

  /*** LISTENER SETUP (listens for "send" Cmd from Ports.elm) ***/

  if (elmApp.ports && elmApp.ports.send) {
    elmApp.ports.send.subscribe(listen);
  } else {
    console.log("JS: Ports.send not hooked up on the Elm side!")
  }

  var listeners = {};

  function listen([msg, value]) {
    if (listeners[msg]) {
      listeners[msg](value);
    } else {
      console.log("JS: Received Junk Msg from Elm: ", msg)
    }
  }

  /*** SEND SETUP (send to "listen" Sub in Ports.elm) ***/

  function send(msg, value) {
    if (elmApp.ports && elmApp.ports.listen) {
      elmApp.ports.listen.send([msg, value]);
    } else {
      console.log("JS: Ports.listen hooked up on the Elm side!")
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

}
