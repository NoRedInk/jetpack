{ pkgs ? (import ./nix/pkgs.nix).pkgs, ghc ? pkgs.ghc }:
pkgs.haskell.lib.buildStackProject {
  inherit ghc;
  name = "jetpack";
  buildInputs = [
    pkgs.gcc
    pkgs.libiconv
    pkgs.ncurses
    pkgs.nodejs-10_x
    pkgs.packer
    pkgs.pcre
    pkgs.pkgconfig
    pkgs.stack
    pkgs.zlib
  ] ++
    (if pkgs.stdenv.system == "x86_64-darwin" then
      [
        # MacOS dependency of http-client-tls haskell lib, used by content_creation binary.
        pkgs.darwin.apple_sdk.frameworks.Cocoa
        pkgs.darwin.apple_sdk.frameworks.CoreServices
      ]
    else []);
}
