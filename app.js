global.XMLHttpRequest = require('./xhr-elm'); // Must require before Elm

const http = require('http');
const crypto = require('crypto');
const Elm = require('./elm');

const hostname = '127.0.0.1';
const port = 8000;
const elmApp = Elm.Main.worker();

elmApp.ports.elmToJs.subscribe(handleActionsFromElm);

const sendToElm = x => {
  console.log('sendToElm', x.tag);
  elmApp.ports.jsToElm.send(x);
};

const respondToClient = ({ nodeResponseObject, statusCode, headers, body }) => {
  nodeResponseObject.writeHead(statusCode, headers);
  nodeResponseObject.end(body);
};

const hashPassword = plainText =>
  new Promise((resolve, reject) => {
    crypto.randomBytes(256, (err, buf) => {
      if (err) {
        reject(err);
      } else {
        resolve(buf.toString('base64'));
      }
    });
  }).then(
    salt =>
      new Promise((resolve, reject) => {
        const callback = (err, derivedKey) => {
          if (err) {
            reject(err);
          } else {
            resolve({
              hash: derivedKey.toString('base64'),
              salt,
            });
          }
        };
        crypto.pbkdf2(plainText, salt, 100000, 512, 'sha512', callback);
      })
  );

function handleActionsFromElm(elmData) {
  console.log('handleActionsFromElm', elmData.tag);
  switch (elmData.tag) {
    case 'RespondToClient':
      respondToClient(elmData.payload);
      break;

    case 'HashPassword':
      const plainText = elmData.payload;
      hashPassword(plainText)
        .then(({ hash, salt }) =>
          sendToElm({
            ...elmData,
            tag: 'JsActionResult',
            payload: { hash, salt },
          })
        )
        .catch(e =>
          sendToElm({
            ...elmData,
            tag: 'JsError',
            payload: e.toString(),
          })
        );
      break;

    default:
      console.error('Unhandled Elm action', elmData);
      sendToElm({
        ...elmData,
        tag: 'JsError',
        payload: 'Unhandled Elm action',
      });
      break;
  }
}

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

const server = http.createServer(handleNewConnection);

server.listen(port, hostname, () => {
  console.log(`Server running at http://${hostname}:${port}/`);
});

module.exports = {
  hashPassword,
  server,
  respondToClient,
  Elm,
};
