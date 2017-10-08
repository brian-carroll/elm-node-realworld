global.XMLHttpRequest = require('./xhr-elm'); // Must require before Elm

const http = require('http');
const Elm = require('./elm');

const hostname = '127.0.0.1';
const port = 8000;
const elmApp = Elm.Main.worker();

elmApp.ports.elmToJs.subscribe(elmResponseObject => {
  const { nodeResponseObject, statusCode, headers, body } = elmResponseObject;
  nodeResponseObject.writeHead(statusCode, headers);
  nodeResponseObject.end(JSON.stringify(body));
});

const server = http.createServer((request, response) => {
  let bodyChunks = [];
  request
    .on('data', chunk => {
      bodyChunks.push(chunk);
    })
    .on('end', () => {
      request.body = Buffer.concat(bodyChunks).toString();
      elmApp.ports.jsToElm.send({ request, response });
    });
});

server.listen(port, hostname, () => {
  console.log(`Server running at http://${hostname}:${port}/`);
});
