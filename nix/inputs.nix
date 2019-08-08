let
  pkgs = (import ./pkgs.nix).pkgs;

  ormoluSrc = pkgs.fetchFromGitHub {
    owner  = "tweag";
    repo   = "ormolu";
    rev    = "d61b9101ca7e1bd9edbc7767eae69e9c7732a0a8";
    sha256 = "1hkrwm1bsriw6pwvml4wlbpfnsvciipdlp2qy8xmmb1wbx0ssiy5";
  };

  ormolu = import ormoluSrc;

  hindent-imposter = import ./hindent-imposter.nix { pkgs = pkgs; ormolu = ormolu.ormolu; };
in
[
  pkgs.gcc
  pkgs.libiconv
  pkgs.ncurses
  pkgs.nodejs-10_x
  pkgs.packer
  pkgs.pcre
  pkgs.pkgconfig
  pkgs.stack
  pkgs.zlib
  hindent-imposter
  ormolu.ormolu
] ++
  (if pkgs.stdenv.system == "x86_64-darwin" then
    [
      # MacOS dependency of http-client-tls haskell lib, used by content_creation binary.
      pkgs.darwin.apple_sdk.frameworks.Cocoa
      pkgs.darwin.apple_sdk.frameworks.CoreServices
    ]
  else [])
