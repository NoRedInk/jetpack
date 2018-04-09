# How to publish releases of jetpack (this is based on @avh4's great work in elm-format)


## Preparation

1. Bump version in package.yaml, package/npm/package.json, and package-lock.json
2. Commit the changes "Bump version to *new version*"
3. Create a tag for the new version. `git tag -s <version> -m <version>`
4. Push the tag. `git push && git push --tags`
5. Wait for [CI to successfully build the tag](https://travis-ci.org/NoRedInk/jetpack/builds), this will create a new github release.


## Publishing

1. Check [release page](https://github.com/NoRedInk/jetpack/releases) on github if both OSX and Linux binaries are there.
2. Write the release notes on github.
3. Publish new npm version
```
cd package/npm
npm publish --access=public
```
