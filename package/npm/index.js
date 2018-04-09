var binwrap = require("binwrap");
var path = require("path");

var binVersion = require(path.join(__dirname, "package.json"));

var root = "https://github.com/NoRedInk/jetpack/releases/download/" + binVersion.version + "/jetpack";

          'https://github.com/NoRedInk/jetpack/releases/download/2.0.15/jetpack-linux.tar.gz'
module.exports = binwrap({
  binaries: ["jetpack"],
  urls: {
    "darwin-x64": root + "-osx.tar.gz",
    "linux-x64": root + "-linux.tar.gz"
  }
});
