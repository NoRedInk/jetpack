{ pkgs ? (import ./nix/pkgs.nix).pkgs, ghc ? pkgs.ghc }:
pkgs.haskell.lib.buildStackProject {
  inherit ghc;
  name = "jetpack";

  buildInputs = import ./nix/inputs.nix;
}
