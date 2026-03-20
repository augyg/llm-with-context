{ aeson, base, bytestring, containers, data-default
, directory, http-client, http-client-tls, http-types, lib, parsec
, network-uri, process
, text, transformers
, scrappy-core
, scrappy-json
, pkgs, mkDerivation
}:
mkDerivation {
  pname = "llm-with-context";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring containers data-default directory http-client
    http-client-tls http-types network-uri parsec process scrappy-core
    scrappy-json text transformers
  ];
  license = lib.licenses.mit;
}
