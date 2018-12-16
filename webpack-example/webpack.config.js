const path = require('path');

module.exports = {
  mode: 'none', // set to 'development' for minified code
  entry: {
    index: './src/index.js'
  },
  output: {
    filename: '[name].js',
    path: path.resolve(__dirname, 'out'),
    libraryTarget: 'commonjs2'
  }
};
