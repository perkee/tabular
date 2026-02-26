// @WARNING: this only runs for Lamdera production deploys!
const copyToClipboard = require('./elm-pkg-js/copy-to-clipboard.js');

exports.init = async function init(app) {
  copyToClipboard.init(app);
}
