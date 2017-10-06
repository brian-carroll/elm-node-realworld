const http = require('http');
const Elm = require('./elm');

const hostname = '127.0.0.1';
const port = 8000;
const elmApp = Elm.Main.worker();

elmApp.ports.elmToJs.subscribe(elmResponseObject => {
  const { nodeResponseObject, statusCode, headers, body } = elmResponseObject;
  nodeResponseObject.statusCode = statusCode;
  for (const key in headers) {
    nodeResponseObject.setHeader(key, headers[key]);
  }
  nodeResponseObject.end(JSON.stringify(body));
});

const server = http.createServer((request, response) => {
  let body = [];
  request
    .on('data', chunk => {
      body.push(chunk);
    })
    .on('end', () => {
      const bodyString = Buffer.concat(body).toString();
      elmApp.ports.jsToElm.send({
        request: {
          url: request.url,
          method: request.method,
          body: bodyString,
        },
        response: response,
      });
    });
});

server.listen(port, hostname, () => {
  console.log(`Server running at http://${hostname}:${port}/`);
});
