const fs = require('fs');
const path = require('path');
const Elm = require('./elm');

const config = {
  sqlDir: fs.realpathSync(__dirname + '/../src/sql/functions'),
  elmDir: fs.realpathSync(__dirname + '/../src/elm/GeneratedCode'),
};

const elmApp = Elm.Main.worker(config);

const walk = (dir, callback) => {
  fs.readdir(dir, (err, files) => {
    if (err) throw err;
    files.forEach(file => {
      const filepath = path.join(dir, file);
      fs.stat(filepath, (err, stats) => {
        if (err) throw err;
        if (stats.isDirectory()) {
          walk(filepath, callback);
        } else if (stats.isFile()) {
          callback(filepath, stats);
        }
      });
    });
  });
};

const convertSqlToElm = filepath =>
  fs.readFile(filepath, { encoding: 'utf8' }, (err, body) => {
    console.log('Reading:', filepath);
    if (err) throw err;
    elmApp.ports.jsToElm.send({
      path: filepath,
      body,
    });
  });

const writeElmFile = ({ path, body, error }) => {
  if (error) {
    throw new Error(error);
  }
  console.log('Writing:', path);
  fs.writeFile(path, body, err => err && console.error(err));
};

elmApp.ports.elmToJs.subscribe(writeElmFile);

walk(config.sqlDir, convertSqlToElm);
