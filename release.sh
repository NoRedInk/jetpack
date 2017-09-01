#!/bin/bash

if [ -z "$1" ]; then
  echo "You need to pass this script a new version"
else
  ./package/mac/build-package.sh
  ./package/linux/build-in-docker.sh

  git add ./binaries
  git commit -m "update binaries"
  git push

  git tag -a $1 -m "$1"
  git push --tags

  # updates version in src/Version.hs
  sed -i '' "s/^version:\s*.*/version: ${1}/g" jetpack.cabal
fi
