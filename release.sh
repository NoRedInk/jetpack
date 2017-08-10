#!/bin/bash

if [ -z "$1" ]; then
  echo "You need to pass this script a new version"
else
  ./package/mac/build-package.sh
  ./package/linux/build-in-docker.sh

  git add ./binaries
  git commit -m "update binaries"

  git tag -a $1 -m "$1"
  git push --tags
fi
