{ aeson, base, bytestring, containers, data-default
, directory, http-client, http-client-tls, http-types, lib, parsec
, text, transformers
, scrappy-core
, pkgs, mkDerivation
}:
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
