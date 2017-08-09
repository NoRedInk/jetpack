jetpack
=======


## Development

https://paper.dropbox.com/doc/jetpack-1EgiUsPF7gx5lwPD1BCKe

```bash
brew install haskell-stack
stack setup # run ./install-readline.sh if this fails (OSX)
stack build
stack test
```

* install `stylish` `stack install stylish`
* install [`entr`](http://entrproject.org/)

Run `./watch-tests` during development. It will rerun all tests when a `hs` file changes.

### Releasing a new version

```bash
./release.sh
```
