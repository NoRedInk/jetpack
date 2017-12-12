#!/bin/bash
# copied from https://github.com/avh4/elm-format. Thanks <3

set -ex

PLATFORM="linux-x64"

## Run tests

stack clean
stack test


## Build binary

stack build

BUILD="jetpack-${PLATFORM}"
mkdir -p dist/package-scripts
JETPACK="$(stack path --local-install-root)/bin/jetpack"
cp "$JETPACK" dist/package-scripts/jetpack
JETPACK_WATCH="$(stack path --local-install-root)/bin/jetpack-watch"
cp "$JETPACK_WATCH" dist/package-scripts/jetpack-watch
tar zcvf "binaries/$BUILD".tgz -C dist/package-scripts jetpack jetpack-watch
