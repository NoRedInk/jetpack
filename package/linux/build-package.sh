#!/bin/bash

set -ex

VERSION="$(git describe --abbrev=8)"
PLATFORM="linux-x64"

## Run tests

stack clean
stack test


## Build binary

stack build

BUILD="jetpack-${VERSION}-${PLATFORM}"
mkdir -p dist/package-scripts
JETPACK="$(stack path --local-install-root)/bin/jetpack"
cp "$JETPACK" dist/package-scripts/jetpack
tar zcvf "$BUILD".tgz -C dist/package-scripts jetpack
