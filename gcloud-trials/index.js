'use strict';

const {Datastore} = require('@google-cloud/datastore');
const datastore = new Datastore();

const {Storage} = require('@google-cloud/storage');
const storage = new Storage();
const bucket = storage.bucket('umass-plasma-test-bucket');

/**
 * Gets a Datastore key from the kind/key pair in the request.
 *
 * @param {object} requestData Cloud Function request data.
 * @param {string} requestData.key Datastore key string.
 * @param {string} requestData.kind Datastore kind.
 * @returns {object} Datastore key object.
 */
function getKeyFromRequestData(requestData) {
  if (!requestData.username) { res.status(400).send('Username not provided. Make sure you have a "username" property in your request.'); }

  return datastore.key(["User", requestData.username]);
}

function checkFields(req, res) {
  if (!req.body.username) { res.status(400).send('Username not provided. Make sure you have a "username" property in your request.'); }
  if (!req.body.password) { res.status(400).send('Password not provided. Make sure you have a "password" property in your request.'); }
}

/**
 * Creates a user.
 *
 * @example
 * gcloud functions call register --data '{"username":"emily", "password": "herbert"}'
 *
 * @param {object} req Cloud Function request context.
 * @param {object} req.body The request body.
 * @param {string} req.body.usernaXme User's username.
 * @param {object} req.body.password User's password.
 * @param {object} res Cloud Function response context.
 */
exports.register = (req, res) => {
  checkFields(req, res);

  const key = getKeyFromRequestData(req.body);
  const data = req.body;

  const entity = {
    key: key,
    data: data
  };

  return datastore
    .save(entity)
    .then(() => res.status(200).send(`User ${req.body.username} registered.`))
    .catch(err => {
      console.error(err);
      res.status(500).send(err.message);
      return Promise.reject(err);
    });
};

function authorize(req, res, next) {
    checkFields(req, res);

  const key = getKeyFromRequestData(req.body);

  return datastore
    .get(key)
    .then(([entity]) => {
      if (!entity) { res.status(400).send(`No user found with username ${req.body.username}.`); }

      if(entity.password !== req.body.password) {
        res.status(400).send('Incorrect username/ password combination.');
      } else {
        next(req, res);
      }

    })
    .catch(err => {
      console.error(err);
      res.status(500).send(err.message);
      return Promise.reject(err);
    });
}

/**
 * Checks that username/ password combination is correct.
 *
 * @example
 * gcloud functions call login --data '{"username":"emily", "password":"herbert"}'
 *
 * @param {object} req Cloud Function request context.
 * @param {object} req.body The request body.
 * @param {string} req.body.username User's username.
 * @param {string} req.body.password User's password.
 * @param {object} res Cloud Function response context.
 */
exports.login = (req, res) => {
  return authorize(req, res, function(req, res) {
    res.status(200).send(`Login successful!`);
  });
};

/**
 * Checks that username/ password combination is correct and deletes the user.
 *
 * @example
 * gcloud functions call remove --data '{"username":"emily", "password":"herbert"}'
 *
 * @param {object} req Cloud Function request context.
 * @param {object} req.body The request body.
 * @param {string} req.body.username User's username.
 * @param {string} req.body.password User's password.
 * @param {object} res Cloud Function response context.
 */
exports.remove = (req, res) => {
  return authorize(req, res, function(req, res) {
    const key = getKeyFromRequestData(req.body);
    return datastore
      .delete(key)
      .then(() => res.status(200).send(`User ${req.body.username} removed.`));
  });
};

/**
 * Lists all users.
 *
 * @example
 * gcloud functions call list
 */
exports.list = async (req, res) => {

  const query = datastore
    .createQuery(`User`);

  let str = "Users:\n";
  const [users] = await datastore.runQuery(query);
  users.forEach(u => str += u.username + "\n");

  return res.status(200).send(str);
};

/**
 * Checks that username/ password combination is correct and retrieves a file.
 *
 * @example
 * gcloud functions call getFile --data '{"username":"emily", "password":"herbert", "srcfile":"government_secrets.txt"}'
 *
 * @param {object} req Cloud Function request context.
 * @param {object} req.body The request body.
 * @param {string} req.body.username User's username.
 * @param {string} req.body.password User's password.
 * @param {string} req.body.srcfile Desired file.
 * @param {object} res Cloud Function response context.
 * 
 * https://github.com/googleapis/google-cloud-node
 * 
 */
exports.getFile = (req, res) => {
  if(!req.body.srcfile) { res.status(400).send(`Source file field not provided.`); }

  return authorize(req, res, function(req, res) {
    return bucket
        .file(req.body.srcfile)
        .download()
        .then(function(data) {
            const contents = data[0];
            res.status(200).send(contents);
        });
    });
};