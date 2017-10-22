//-----------------------------------------------
// Imports
//-----------------------------------------------
global.XMLHttpRequest = require('./xhr-elm'); // Polyfill for elm-lang/http
const http = require('http');
const Elm = require('../../build/elm');
const passwords = require('./passwords');

//-----------------------------------------------
// Config
//-----------------------------------------------
const hostname = '127.0.0.1';
const port = 8000;

//-----------------------------------------------
// Elm setup
//-----------------------------------------------
const elmFlags = {
  secret: process.env.NODE_ENV === 'production' ? process.env.SECRET : 'secret',
  jsActionTimeout: 1000, // JS interop actions will be abandoned after this timeout
  jsActionCheckInterval: 1000, // Check this often whether JS actions have timed out
};
const elmApp = Elm.Main.worker(elmFlags);
elmApp.ports.elmToJs.subscribe(handleActionsFromElm);
const sendToElm = x => {
  console.log('sendToElm', x.tag);
  elmApp.ports.jsToElm.send(x);
};

//-----------------------------------------------
// JS-Elm interop
//-----------------------------------------------
const respondToClient = ({ nodeResponseObject, statusCode, headers, body }) => {
  nodeResponseObject.writeHead(statusCode, headers);
  nodeResponseObject.end(body);
};

const dispatchEffects = async elmData => {
  switch (elmData.tag) {
    case 'RespondToClient':
      respondToClient(elmData.payload);
      return;

    case 'HashPassword':
      return await passwords.hash(elmData.payload);

    case 'CheckPassword':
      return await passwords.check(elmData.payload);

    default:
      throw new Error('Unhandled Elm action');
  }
};

async function handleActionsFromElm(elmData) {
  console.log('handleActionsFromElm:', elmData.tag);
  try {
    const effectData = await dispatchEffects(elmData);
    if (effectData) {
      sendToElm({
        ...elmData,
        tag: 'JsActionResult',
        payload: effectData,
      });
    }
  } catch (e) {
    sendToElm({
      ...elmData,
      tag: 'JsError',
      payload: e.toString(),
    });
  }
}

//-----------------------------------------------
// Connection handler
//-----------------------------------------------

// Application-level state to guarantee unique connection ID's
let sequenceNo = 0;
let previousTimestamp = 0;

class Connection {
  constructor(request, response) {
    const timestamp = Date.now();
    if (timestamp === previousTimestamp) {
      sequenceNo++;
    } else {
      sequenceNo = 0;
    }
    previousTimestamp = timestamp;
    return {
      tag: 'NewConnection',
      connectionId: [timestamp, sequenceNo],
      payload: { request, response },
    };
  }
}

const handleNewConnection = (request, response) => {
  let bodyChunks = [];
  request
    .on('data', chunk => {
      bodyChunks.push(chunk);
    })
    .on('end', () => {
      request.body = Buffer.concat(bodyChunks).toString();
      sendToElm(new Connection(request, response));
    });
};

//-----------------------------------------------
// Node server
//-----------------------------------------------
const server = http.createServer(handleNewConnection);

server.listen(port, hostname, () => {
  console.log(`Server running at http://${hostname}:${port}/`);
});
