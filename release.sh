#!/bin/bash

if [ -z "$1" ]; then
  echo "You need to pass this script a new version"
else
  for f in package.yaml jetpack.cabal
  do
    sed -i '' "s/^version:\s*.*/version: ${1}/g" $f
  done

  ./package/mac/build-package.sh
  ./package/linux/build-in-docker.sh
  git add package.yaml

  git commit -m "bump version to $1"
  git push

  git tag -a $1 -m "$1"
  git push --tags

  pushd binaries
  for i in jetpack-{mac-x64.tgz,linux-x64.tgz}; do
    github-release upload --user NoRedInk --repo jetpack --tag "$1" --file "$i"
  done
fi
