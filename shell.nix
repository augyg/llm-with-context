{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let
  inherit (nixpkgs) pkgs;
  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};
  haskellLib = pkgs.haskell.lib;
  scrappy-core = haskellLib.dontCheck (haskellLib.doJailbreak (haskellPackages.callCabal2nix "scrappy-core" ../scrappy-core {}));
  scrappy-json-src =
    let thunkDir = ./thunks/scrappy-json;
        hasThunkNix = builtins.pathExists (thunkDir + "/thunk.nix");
    in if hasThunkNix then import (thunkDir + "/thunk.nix") else thunkDir;
  scrappy-json = haskellLib.dontCheck (haskellLib.doJailbreak (haskellPackages.callCabal2nix "scrappy-json" scrappy-json-src {}));
  henforcer = haskellLib.dontCheck (haskellLib.doJailbreak (haskellPackages.callPackage ({ mkDerivation, base, containers, dlist, fetchzip, filepattern, ghc
    , lib, optparse-applicative, pollock, text, tomland
    }:
    mkDerivation {
      pname = "henforcer";
      version = "1.0.0.1";
      src = fetchzip {
        url = "https://hackage.haskell.org/package/henforcer-1.0.0.1/henforcer-1.0.0.1.tar.gz";
        sha256 = "1vnwvhszvqrypcvk8zvb7px2q78mn3lhbhj521vsrz6zj6459f0r";
      };
      libraryHaskellDepends = [
        base containers dlist filepattern ghc optparse-applicative pollock
        text tomland
      ];
      doHaddock = false;
      license = lib.licenses.mit;
    }) {}));
  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
  pkg = import ./default.nix;
  drv = variant (haskellPackages.callPackage pkg { inherit scrappy-core scrappy-json henforcer; });
in
pkgs.mkShell {
  buildInputs = [ pkgs.cabal-install ];
  inputsFrom = [ (if pkgs.lib.inNixShell then drv.env else drv) ];
}







# { nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

# let

#   inherit (nixpkgs) pkgs;
#   scrappy-coreSrc = pkgs.fetchFromGitHub {
#     owner = "Ace-Interview-Prep";
#     repo =  "scrappy-core";
#     rev = "913670f2f83cabb2b56302e17604ec488e89da7b";
#     sha256 = "0xvxc29x2izm1jpq5zpncyirhadwcx2wf3b61ns9mhvcpkjbw3m8";
#   };
#   scrappy-core = pkgs.haskellPackages.callCabal2nix "scrappy-core" scrappy-coreSrc {};

#   f = { mkDerivation, base, bytestring, containers, directory, lib
#       , parsec, text, transformers, aeson, http-types, http-client
#       , http-client-tls, data-default
#       }:
#       mkDerivation {
#         pname = "llm-with-context";
#         version = "0.1.0.0";
#         src = ./.;
#         libraryHaskellDepends = [
#           base bytestring containers directory parsec scrappy-core text
#           transformers aeson http-types http-client http-client-tls
#           data-default
#         ];
#         license = lib.licenses.mit;
#       };

#   haskellPackages = if compiler == "default"
#                        then pkgs.haskellPackages
#                        else pkgs.haskell.packages.${compiler};

#   variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

#   drv = variant (haskellPackages.callPackage f {});

# in

#   if pkgs.lib.inNixShell then drv.env else drv
