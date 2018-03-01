var binwrap = require("binwrap");
var path = require("path");

var packageInfo = require(path.join(__dirname, "package.json"));
var binVersion = packageInfo.version.replace(/\.[0-9]*$/, "");

var root = "https://github.com/NoRedInk/jetpack/releases/download/" +
  binVersion +
  "/jetpack" +
  binVersion;

module.exports = binwrap({
  binaries: ["jetpack"],
  urls: {
    "darwin-x64": root + "-mac-x64.tgz",
    "linux-x64": root + "-linux-x64.tgz"
  }
});
