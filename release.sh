#!/bin/bash

if [ -z "$1" ]; then
  echo "You need to pass this script a new version"
else
  ./package/mac/build-package.sh
  ./package/linux/build-in-docker.sh

  sed -i '' "s/^version:\s*.*/version: ${1}/g" package.yaml
  git add package.yaml

  git add ./binaries
  git commit -m "bump version to $1"
  git push

  git tag -a $1 -m "$1"
  git push --tags
fi
