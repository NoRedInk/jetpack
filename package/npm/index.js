var binwrap = require("binwrap");
var path = require("path");

var packageInfo = require(path.join(__dirname, "package.json"));
var root =
  "https://github.com/NoRedInk/jetpack/releases/download/" +
  packageInfo.version +
  "/jetpack";

module.exports = binwrap({
  dirname: __dirname,
  binaries: ["jetpack"],
  urls: {
    "darwin-x64": root + "-osx.tar.gz",
    "linux-x64": root + "-linux.tar.gz"
  }
});
