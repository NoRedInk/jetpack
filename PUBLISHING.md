# How to publish releases of jetpack (this is based on @avh4's great work in elm-format)


## Workstation setup

### Mac

```bash
brew update
brew install github-release
```


## Preparation

1. `./release.sh`
1. `(cd package/npm && npm version "<new version>")`
1. Commit the changes "Bump version to *new version*"
1. Create a signed tag for the new version. `git tag -s <version> -m <version>`
1. Push the tag.
1. Wait for CI to successfully build the tag.


## Publishing

1. Run `release.sh`
1. Go to the release page for the new tag on github.
1. Upload the zip, tgz and asc files.
1. Write the release notes.
1. Publish the release.
1. Update `README.md`


## NPM

```
cd package/npm
npm publish
```
