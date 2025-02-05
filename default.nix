{ mkDerivation, aeson, base, bytestring, containers, data-default
, directory, http-client, http-client-tls, http-types, lib, parsec
, text, transformers, pkgs
}:
let
  scrappy-coreSrc = pkgs.fetchFromGitHub {
    owner = "Ace-Interview-Prep";
    repo =  "scrappy-core";
    rev = "913670f2f83cabb2b56302e17604ec488e89da7b";
    sha256 = "0xvxc29x2izm1jpq5zpncyirhadwcx2wf3b61ns9mhvcpkjbw3m8";
  };
  scrappy-core = pkgs.haskellPackages.callCabal2nix "scrappy-core" scrappy-coreSrc {};
in
mkDerivation {
  pname = "llm-with-context";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring containers data-default directory http-client
    http-client-tls http-types parsec scrappy-core text transformers
  ];
  license = lib.licenses.mit;
}
