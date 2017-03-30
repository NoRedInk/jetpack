#!/bin/bash
# copied from https://github.com/avh4/elm-format. Thanks <3

set -ex

VERSION="0.0.3"
PLATFORM="mac-x64"

## Run tests

stack clean
stack test


## Build binaries

stack build

BUILD="jetpack-${VERSION}-${PLATFORM}"
mkdir -p dist/package-scripts
JETPACK="$(stack path --local-install-root)/bin/jetpack"
cp "$JETPACK" dist/package-scripts/jetpack
strip dist/package-scripts/jetpack
tar zcvf "binaries/$BUILD".tgz -C dist/package-scripts jetpack