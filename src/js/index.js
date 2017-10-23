//-----------------------------------------------
// Imports
//-----------------------------------------------
global.XMLHttpRequest = require('./xhr-elm'); // Polyfill for elm-lang/http
const http = require('http');
const Elm = require('../../build/elm');
const passwords = require('./passwords');
const config = require('./config');
const database = require('./database');

//-----------------------------------------------
// Config
//-----------------------------------------------

//-----------------------------------------------
// Elm setup
//-----------------------------------------------
const elmFlags = config.elm;
const elmApp = Elm.Main.worker(elmFlags);
elmApp.ports.elmToJs.subscribe(handleActionsFromElm);
const sendToElm = x => {
  console.log('sendToElm', x.tag);
  elmApp.ports.jsToElm.send(x);
};

//-----------------------------------------------
// JS-Elm interop
//-----------------------------------------------
const respondToClient = ({
  nodeResponseObject,
  dbClient,
  statusCode,
  headers,
  body,
}) => {
  nodeResponseObject.writeHead(statusCode, headers);
  nodeResponseObject.end(body);
  database.releaseClient(dbClient);
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

    case 'SqlQuery':
      return await database.runQuery(elmData.payload);

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
  constructor(request, response, dbClient) {
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
      payload: { request, response, dbClient },
    };
  }
}

const handleNewConnection = (request, response) => {
  let bodyChunks = [];
  request
    .on('data', chunk => {
      bodyChunks.push(chunk);
    })
    .on('end', async () => {
      request.body = Buffer.concat(bodyChunks).toString();
      try {
        const dbClient = await database.checkoutClient();
        sendToElm(new Connection(request, response, dbClient));
      } catch (e) {
        response.statusCode(500).end(e.toString());
      }
    });
};

//-----------------------------------------------
// Node server
//-----------------------------------------------
const { hostname, port } = config.server;

const server = http.createServer(handleNewConnection);

server.listen(port, hostname, () => {
  console.log(`Server running at http://${hostname}:${port}/`);
});
