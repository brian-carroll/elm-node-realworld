const Elm = require('./elm');
const elmApp = Elm.Main.worker();

elmApp.ports.elmToJs.subscribe(console.log);
process.argv.forEach(arg => elmApp.ports.jsToElm.send(arg));
