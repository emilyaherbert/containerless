// Simple HTTP endpoint function
// https://github.com/serverless/examples/tree/master/aws-node-simple-http-endpoint

'use strict';

module.exports.handler = (event, context, callback) => {
  const response = {
    statusCode: 200,
    body: JSON.stringify({
      message: `Hello, the current time is ${new Date().toTimeString()}.`,
    }),
  };

  callback(null, response);
};
