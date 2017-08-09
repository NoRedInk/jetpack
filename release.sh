#!/bin/bash

if [ -z "$1" ]; then
  echo "You need to pass this script a new version"
else
  echo ./package/mac/build-in-docker.sh
  echo ./package/linux/build-in-docker.sh

  echo git tag -a $1 -m "$1"
  echo git push --tags
fi
