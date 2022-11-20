{
  description = "Haskell version of C's struct stat";

  inputs = {
    nixpkgs.url      = github:nixos/nixpkgs/be44bf67; # nixos-22.05 2022-10-15
    build-utils.url  = github:sixears/flake-build-utils/r1.0.0.13;

    tfmt.url         = github:sixears/tfmt/r0.2.7.24;
  };

  outputs = { self, nixpkgs, build-utils
            , tfmt }:
    build-utils.lib.hOutputs self nixpkgs "fstat" {
      ghc = p: p.ghc8107; # for tfmtoptparse-plus
      callPackage = { mkDerivation, lib, mapPkg, system
                    , base, base-unicode-symbols, data-textual, text
                    , text-printer, time, unix
                    }:
        mkDerivation {
          pname = "fstat";
          version = "1.0.2.25";
          src = ./.;
          libraryHaskellDepends = [
            base base-unicode-symbols data-textual text text-printer time unix
          ] ++ mapPkg [ tfmt ];
          description = "Haskell version of C's struct stat";
          license = lib.licenses.mit;
        };
    };
}
