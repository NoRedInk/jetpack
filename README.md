jetpack [![Build Status](https://travis-ci.org/NoRedInk/jetpack.svg?branch=master)](https://travis-ci.org/NoRedInk/jetpack)
=======

> **NOTE:** Jetpack is an internal NoRedInk build tool that we decided to open source in hopes that it might be useful to others. It has never been our goal to develop a fully featured front-end build system for other use cases; it currently compiles only Elm and CoffeeScript because that's all our build needs. We're open to potentially accepting outside contributions to support other use cases, but please understand that this remains an internal tool first, and we can't make any promises that anything will be accepted. Thanks for understanding, and by all means don't feel bad about forking! ❤️

Motivation
----------

Webpack didn't work out for us; it did more than we needed, and builds were taking too long.
Jetpack is focused on compilation speed. It only compiles the stuff we need and creates the concatenated output js.

Given an entry point, it follows all `require` statements and creates a dependency tree. It then builds all files using the appropriate compiler, concatenates the compiled js, and wraps modules in functions so that the requires know how to get the right functions.


Configuration
-------------

```json
{
  "entry_points": "./FOLDER/TO/THE/ENTRY_POINTS",
  "modules_directories": [ // folders where jetpack should try to find modules
    "./node_modules",
    "./vendor/assets/javascripts/FOO"
  ],
  "source_directory": "./ui/src", // jetpack will try to resolve modules in source_directory before checking in modules_directories
  "elm_root_directory": "./ui", // where your elm-package.json is
  "temp_directory": "./.jetpack/build_artifacts", // jetpack's build_artifacts will be here
  "log_directory": "./.jetpack/logs", // jetpack will log stuff in here
  "output_js_directory": "assets/javascripts", // js will be written to this folder
  "elm_make_path": "./node_modules/.bin/elm-make", // path to elm-make
  "coffee_path": "./node_modules/.bin/coffee", // path to coffee
  "no_parse": ["./node_modules/clipboard/dist/clipboard.js"] // files that shouldn't be parsed
}
```

## Development

```bash
brew install haskell-stack
stack setup # run ./install-readline.sh if this fails (OSX)
stack build
stack test
```

* install [`entr`](http://entrproject.org/)

Run `./watch-tests` during development. It will rerun all tests when a `hs` file changes.
Run `./hindent.sh` to format everything correctly.

### Releasing a new version

```bash
./release.sh
```
