var binwrap = require("binwrap");
var path = require("path");

var binVersion = require(path.join(__dirname, "package.json"));

var root = "https://github.com/NoRedInk/jetpack/releases/download/" + binVersion.version + "/jetpack";

module.exports = binwrap({
  binaries: ["jetpack"],
  urls: {
    "darwin-x64": root + "-mac-x64.tgz",
    "linux-x64": root + "-linux-x64.tgz"
  }
});
