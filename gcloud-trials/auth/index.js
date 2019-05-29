'use strict';

const {Datastore} = require('@google-cloud/datastore');

// Instantiates a client
const datastore = new Datastore();

/**
 * Gets a Datastore key from the kind/key pair in the request.
 *
 * @param {object} requestData Cloud Function request data.
 * @param {string} requestData.key Datastore key string.
 * @param {string} requestData.kind Datastore kind.
 * @returns {object} Datastore key object.
 */
function getKeyFromRequestData(requestData) {

  if (!requestData.username) {
    throw new Error(
      'Username not provided. Make sure you have a "username" property in your request.'
    );
  }

  return datastore.key(["User", requestData.username]);
}

/**
 * Creates a user.
 *
 * @example
 * gcloud functions call register --data '{"username":"emily", "password": "herbert"}'
 *
 * @param {object} req Cloud Function request context.
 * @param {object} req.body The request body.
 * @param {string} req.body.username User's username.
 * @param {object} req.body.password User's password.
 * @param {object} res Cloud Function response context.
 */
exports.register = (req, res) => {

  if (!req.body.username) {
    throw new Error(
      'Username not provided. Make sure you have a "username" property in your request.'
    );
  }

  if (!req.body.password) {
    throw new Error(
      'Password not provided. Make sure you have a "password" property in your request.'
    );
  }

  const key = getKeyFromRequestData(req.body);
  const data = { "username" : req.body.username, "password" : req.body.password };

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
  if (!req.body.username) {
    throw new Error(
      'Username not provided. Make sure you have a "username" property in your request.'
    );
  }

  if (!req.body.password) {
    throw new Error(
      'Password not provided. Make sure you have a "password" property in your request.'
    );
  }

  const key = getKeyFromRequestData(req.body);

  return datastore
    .get(key)
    .then(([entity]) => {

      if (!entity) {
        throw new Error(`No user found with username ${req.body.username}.`);
      }

      if(entity.password !== req.body.password) {
        throw new Error(`Incorrect username/ password combination.`);
      }

      res.status(200).send(`Login successful!`);
    })
    .catch(err => {
      console.error(err);
      res.status(500).send(err.message);
      return Promise.reject(err);
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
  if (!req.body.username) {
    throw new Error(
      'Username not provided. Make sure you have a "username" property in your request.'
    );
  }

  if (!req.body.password) {
    throw new Error(
      'Password not provided. Make sure you have a "password" property in your request.'
    );
  }

  const key = getKeyFromRequestData(req.body);

  return datastore
    .get(key)
    .then(([entity]) => {

      if (!entity) {
        throw new Error(`No user found with username ${req.body.username}.`);
      }

      if(entity.password !== req.body.password) {
        throw new Error(`Incorrect username/ password combination.`);
      }

      return datastore
        .delete(key)
        .then(() => res.status(200).send(`User ${req.body.username} removed.`))
        .catch(err => {
          console.error(err);
          res.status(500).send(err);
          return Promise.reject(err.message);
        });
    })
    .catch(err => {
      console.error(err);
      res.status(500).send(err.message);
      return Promise.reject(err);
    });
};
