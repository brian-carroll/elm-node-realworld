const crypto = require('crypto');

const generateSalt = () =>
  new Promise((resolve, reject) =>
    crypto.randomBytes(
      256,
      (err, buf) => (err ? reject(err) : resolve(buf.toString('base64')))
    )
  );

const generateHash = (plainText, salt) =>
  new Promise((resolve, reject) => {
    const callback = (err, derivedKey) =>
      err ? reject(err) : resolve(derivedKey);
    crypto.pbkdf2(plainText, salt, 100000, 512, 'sha512', callback);
  });

module.exports = {
  async hash(plainText) {
    const salt = await generateSalt();
    const hashBuffer = await generateHash(plainText, salt);
    return {
      hash: hashBuffer.toString('base64'),
      salt,
    };
  },

  async check({ hash, salt, plainText }) {
    const dbHash = new Buffer(hash, 'base64');
    const formHash = await generateHash(plainText, salt);
    const passwordIsValid = crypto.timingSafeEqual(dbHash, formHash);
    return { passwordIsValid };
  },
};
