let
  fetch = { rev, sha256 }:
    builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
      sha256 = sha256;
    };


  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          # Example of an override:
          # dotenv = haskellPackagesNew.callPackage ./dotenv.nix { };
        };
      };
    };
  };

  pkgsPath = fetch {
    # This comes from https://nixos.org/channels/
    #
    # Pick a release (e.g. nixpkgs-18.09-darwin) and then open the `git-revision`
    # file. It will contain a rev like this.
    #
    # Example: https://releases.nixos.org/nixpkgs/18.09-darwin/nixpkgs-darwin-18.09pre154040.58fbebc4ea5/git-revision
    rev = "9ec7625cee5365c741dee7b45a19aff5d5d56205";

    # Generate this sha using the following command:
    #
    # $ nix-prefetch-url --unpack --type sha256 https://github.com/NixOS/nixpkgs/archive/<rev>.tar.gz
    sha256 = "0rh26fhdvnp9ssk8g63ysyzigw9zg43k9bd2fzrvhrk75sav723h";
  };
in {
  pkgs = import pkgsPath { config = config; };
}
