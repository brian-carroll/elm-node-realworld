const config = require('./config');
const { Pool } = require('pg');

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
