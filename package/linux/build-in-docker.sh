#!/bin/bash
# copied from https://github.com/avh4/elm-format. Thanks <3

set -ex

docker build -t jetpack-dev-linux .
docker run -v "$(pwd)":/jetpack -w /jetpack jetpack-dev-linux ./package/linux/build-package.sh
