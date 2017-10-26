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
    console.error(e);
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

let sequenceNo = 0;
let previousTimestamp = 0;

const uniqueConnectionId = timestamp => {
  // Requirements:
  //  - Always less than Number.MAX_SAFE_INTEGER (2**53)
  //  - Unique up to a ridiculous number of requests/second (4,096,000)
  //  - OK until at least 2038, when UNIX-like timestamps are dead anyway
  if (timestamp === previousTimestamp) {
    sequenceNo++;
  } else {
    sequenceNo = 0;
  }
  previousTimestamp = timestamp;
  return timestamp * 4096 + sequenceNo;
};

class Connection {
  constructor(request, response, dbClient) {
    const timestamp = Date.now();
    return {
      tag: 'NewConnection',
      connectionId: uniqueConnectionId(timestamp),
      payload: { request, response, timestamp, dbClient },
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
