const config = require('./config');
const { Pool, types } = require('pg');

// Prevent pg from parsing timestamps into JS Dates, which Elm can't interpret
const TIMESTAMPTZ_OID = 1184;
const TIMESTAMP_OID = 1114;
const passthrough = str => str;
types.setTypeParser(TIMESTAMPTZ_OID, passthrough);
types.setTypeParser(TIMESTAMP_OID, passthrough);

const pool = new Pool(config.postgres);

const checkoutClient = async () => await pool.connect();
const releaseClient = client => client.release();

const runQuery = async ({ dbClient, sql, values }) => {
  try {
    return {
      result: await dbClient.query(sql, values),
    };
  } catch (e) {
    return {
      error: e.toString(),
    };
  }
};

module.exports = {
  checkoutClient,
  releaseClient,
  runQuery,
};
