/**
 XMLHttpRequest emulator for Node.js, with Elm compatibility

 We need Elm to think it's in the browser so that it can make HTTP calls.
 But the two most popular npm libs for emulating it don't work properly with Elm!
 ('xmlhttprequest' and 'w3c-xmlhttprequest')

 Here we use a Proxy object to add the 'response' property, which is not implemented
 in the 'xmlhttprequest' library.
*/

var LibXMLHttpRequest = require('xmlhttprequest').XMLHttpRequest;

module.exports = function() {
  var xhr = new LibXMLHttpRequest();
  return new Proxy(xhr, {
    // 'get' is called whenever external code accesses a property, like xhr.response
    get: function(wrappedObject, propName) {
      if (propName === 'response') {
        return wrappedObject.responseText;
      } else {
        return wrappedObject[propName];
      }
    },
  });
};
