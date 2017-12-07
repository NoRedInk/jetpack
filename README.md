jetpack
=======

Motivation
----------

Webpack didn't work out for us, since it was super slow and does way to much for our process.
Jetpack only compiles the stuff we need and creates the concatenated output js.
Jetpack compiles elm, coffeeScript and sass.
Given an entrypoint it follows all `require` statements and creates a dependency-tree. It will then builds all files using the appropriate compiler and concatenates the compiled js and wraps modules in functions so that the requires know how to get the right functions.


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
  "sass_load_paths": [ // this will be passed to sassc
    "node_modules",
    "ui/src"
  ],
  "temp_directory": "./.jetpack/build_artifacts", // jetpack's build_artifacts will be here
  "log_directory": "./.jetpack/logs", // jetpack will log stuff in here
  "output_js_directory": "assets/javascripts", // js will be written to this folder
  "output_css_directory": "assets/stylesheets", // css will be written to this folder
  "elm_make_path": "./node_modules/.bin/elm-make", // path to elm-make
  "sassc_path": "./bin/sass", // path to sassc
  "coffee_path": "./node_modules/.bin/coffee", // path to coffee
  "no_parse": ["jsdom" ] // don't parse these files. They raise an exception if they still get required during runtime.
}
```

## Development

https://paper.dropbox.com/doc/jetpack-1EgiUsPF7gx5lwPD1BCKe

```bash
brew install haskell-stack
stack setup # run ./install-readline.sh if this fails (OSX)
stack build
stack test
```

* install `stylish` `stack install stylish`
* install [`entr`](http://entrproject.org/)

Run `./watch-tests` during development. It will rerun all tests when a `hs` file changes.

### Releasing a new version

```bash
./release.sh
```
