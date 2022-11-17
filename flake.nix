{
  description = "Haskell version of C's struct stat";

  inputs = {
    nixpkgs.url      = "github:nixos/nixpkgs/be44bf67"; # nixos-22.05 2022-10-15
    build-utils.url  = "github:sixears/flake-build-utils/r1.0.0.11";

    tfmt.url         = "github:sixears/tfmt/r0.2.7.13";
  };

  outputs = { self, nixpkgs, build-utils
            , tfmt }:
    build-utils.lib.hOutputs self nixpkgs "fstat" {
      deps = {
        inherit tfmt;
      };
      ghc = p: p.ghc8107; # for tfmt
    };
}
