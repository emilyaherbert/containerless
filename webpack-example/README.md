# Webpacking Serverless JS
`npm` project for webpacking serverless JS functions.
For a full guide on using webpack, check out their introductory guide [here](https://webpack.js.org/guides/getting-started/).

## Setup
First, make sure you have node.js and npm installed (npm is automatically included in [node](https://www.npmjs.com/get-npm)).
To update to the latest version, use `npm install -g npm`.

To add a JS function to be webpacked, simply place it in the `src` directory. Then, open `webpack.config.js` and add an item to `entry` with a key name of your choosing, and a value of the filename.

Finally, run `npm run build`. The webpacked function will be in the `out` folder. This can be zipped up, or simply copy-and-pasted into an AWS Lambda function (or any other cloud function service, though only AWS Lambda has been tested so far).
