//
// Add control ports for localStorage to Elm app
//
function addLocalStoragePorts(elmApp) {

  // CMD SAVE TO STORAGE
  if (elmApp.ports.saveToLocalStorage) {
    elmApp.ports.saveToLocalStorage
      .subscribe(saveToLocalStorage);
  }

  // CMD ADD KEY TO LISTEN FOR
  var keyHolder = {
    keys: []
  };
  if (elmApp.ports.addLocalStorageListener) {
    elmApp.ports.addLocalStorageListener
      .subscribe(addLocalStorageListener(keyHolder));
  }

  // SUB LISTEN FOR STORAGE KEYS
  window.addEventListener("storage",
    onLocalStorageChange(keyHolder),
    false);

}

function saveToLocalStorage(record) {
  localStorage.setItem(record.key, record.value);
}

function addLocalStorageListener(keyHolder) {
  return function(key) {
    if (!keyHolder.keys.includes(key)) {
      keyHolder.keys.push(key);
      var value = localStorage.getItem(key);
      console.log(value);
      elmApp.ports.onLocalStorageChange.send({
        key: key,
        value: value
      });
    }
  }
}

function onLocalStorageChange(keyHolder) {
  return function(event) {
    if (event.storageArea === localStorage &&
      keysToListenFor.includes(event.key)) {
      elmApp.ports.onLocalStorageChange
        .send({
          key: event.key,
          value: event.newValue
        });
    }
  }
}
