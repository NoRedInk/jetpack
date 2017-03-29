#!/bin/bash

set -ex

docker build -t jetpack-dev-linux .
docker run -v "$(pwd)":/jetpack -w /jetpack jetpack-dev-linux ./package/linux/build-package.sh
