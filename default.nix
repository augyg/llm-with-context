let
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/refs/heads/nixos-24.11.tar.gz";
  }) { };
  # claude-code isn't in nixpkgs 24.11 yet, but it lives in nixpkgs-unstable.
  # Pin a specific unstable rev (per the CLAUDE.md lazamar pattern) so
  # 'newerPkgs.claude-code' resolves without bumping the rest of the channel.
  # rev: e6f23dc08d3624daab7094b701aa3954923c6bbb (2025-06-16, claude-code 1.0.21)
  # sha256 verified locally via `nix-prefetch-url --unpack <url>`.
  newerPkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/e6f23dc08d3624daab7094b701aa3954923c6bbb.tar.gz";
    sha256 = "0m0xmk8sjb5gv2pq7s8w7qxf7qggqsd3rxzv3xrqkhfimy2x7bnx";
  }) { };
  hlib = pkgs.haskell.lib;
  # GHC 9.10: the merged-in upstream modules depend on scrappy-json (base >= 4.19)
  # and upstream targets base 4.20, so the unified tree builds on 9.10, not the
  # 9.6 the effectful half originally used. effectful-core + servant-client are
  # prebuilt for this set in nixpkgs 24.11.
  # The beam + postgresql stack (for LLM.Brain.DB — same libraries as ace-ws).
  # Two standard fixes for this pin (the same scrappy-core / scrappy-json get):
  #   * dontCheck — their test suites want a live PostgreSQL/docker absent from
  #     the build sandbox;
  #   * doJailbreak — stale `base < 4.20` upper bounds that exclude GHC 9.10.1's
  #     base 4.20 although the code builds fine on it.
  # beam-automigrate is deliberately NOT used: it relies on Control.Monad
  # re-exports that mtl 2.3 (GHC >=9.6) dropped and so fails to compile on 9.10
  # at any version — LLM.Brain.DB creates its schema with beam-migrate instead.
  hp = pkgs.haskell.packages.ghc9101.override {
    overrides = self: super:
      let
        fixLib = p: hlib.doJailbreak (hlib.dontCheck p);
        jailbroken =
          [ "uuid"
            "postgresql-libpq" "postgresql-simple"
            "beam-core" "beam-migrate" "beam-postgres"
          ];
      in pkgs.lib.genAttrs jailbroken (n: fixLib super.${n})
         // {
           # nixpkgs-24.11 freezes pre-base-4.20 versions of these (vector-sized
           # 1.5.0 doesn't even compile on 9.10); pull the versions current
           # nixpkgs ships non-broken for ghc910, fetched via callHackage.
           finite-typelits = fixLib (self.callHackage "finite-typelits" "0.2.1.0" {});
           vector-sized    = fixLib (self.callHackage "vector-sized" "1.6.1" {});
         };
  };

  # Filter out the cabal artifacts / multi-package project file so callCabal2nix
  # sees a clean single-package source.
  cleanSrc = src:
    pkgs.lib.cleanSourceWith {
      inherit src;
      filter = path: _type:
        let base = baseNameOf path;
        in base != "dist-newstyle" && base != "cabal.project" && base != ".git";
    };

  # scrappy-core pinned to the rev pipeline/upstream-llm-with-context use
  # (Ace-Interview-Prep/scrappy-core, from pipeline/thunks/scrappy-core). This
  # rev's `scrape` is String/Parsec-based, which is what LLM.ReadLLM expects —
  # the ace-ws local thunk is the Text/Html-scraping variant and mismatches.
  scrappy-core-src = pkgs.fetchFromGitHub {
    owner = "Ace-Interview-Prep";
    repo = "scrappy-core";
    rev = "6f795938d6f2627d5aea0a7d3ecb4f6aceaf1e24";
    sha256 = "1gvis631g83vmrk8v2d0v4la9fg8c301hariirzdx6x8ny6b0rdw";
  };
  scrappy-core = hlib.doJailbreak (hlib.dontCheck
    (hp.callCabal2nix "scrappy-core" scrappy-core-src { }));

  # scrappy-json (TypifyDev) — pinned by thunks/scrappy-json/github.json. Fetched
  # directly from GitHub at that rev so callCabal2nix can build it.
  scrappy-json-src = pkgs.fetchFromGitHub {
    owner = "TypifyDev";
    repo = "scrappy-json";
    rev = "fb86f947183a376feb7e0241f0efa30fd4ad5d09";
    sha256 = "1yfnmn1rc75ayp9jk237m8zl8svggk6m8z7sihg0in6i96zarkfg";
  };
  # doJailbreak: scrappy-json caps parsec <3.1.18-ish, but GHC 9.10.1 ships
  # parsec 3.1.17 (the bound is cosmetic — it builds fine on 3.1.17).
  scrappy-json = hlib.doJailbreak (hlib.dontCheck
    (hp.callCabal2nix "scrappy-json" scrappy-json-src { }));

  # bubblewrap must be on PATH at COMPILE time so $(staticWhich "bwrap") in
  # LLM.Effect.Tool.Sandbox resolves to its /nix/store path. addBuildTool puts
  # it there; because the resolved path is baked into the object code, nix then
  # tracks bubblewrap as a runtime dependency automatically.
  #
  # Same story for claude-code: LLM.Provider.AnthropicCli uses
  # $(staticWhich "claude") so the binary path is baked in at compile time.
  # It comes from the pinned 'newerPkgs' above (claude-code isn't yet in
  # nixpkgs 24.11).
  # henforcer: GHC plugin from gs/henforcer-strict-tests (via
  # newSorted/gs/pre-merge-newSorted-dirty). Same derivation shape as the one
  # in shell.nix — fetchzip from Hackage, dontCheck because tests want an
  # interactive env, doJailbreak because its base/text bounds predate 9.10.
  henforcer = hlib.dontCheck (hlib.doJailbreak (hp.callPackage ({ mkDerivation, base, containers, dlist, fetchzip, filepattern, ghc
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
      license = lib.licenses.bsd3;
    }) { }));

  llm-with-context =
    hlib.addBuildTools
      (hp.callCabal2nix "llm-with-context" (cleanSrc ./.) { inherit scrappy-core scrappy-json henforcer; })
      [ pkgs.bubblewrap newerPkgs.claude-code ];
in
hlib.doCheck llm-with-context

# MERGE NOTE: newSorted/gs/pre-merge-newSorted-dirty shipped a flat cabal2nix-style
# derivation here, listing henforcer among the dependencies. HEAD above uses
# `callCabal2nix` on the cleaned source tree, which derives build-depends FROM
# the .cabal file at build time — so henforcer is picked up automatically once
# it is listed in llm-with-context.cabal's build-depends. The flat derivation
# below is commented out rather than deleted so the design decision is visible
# in blame.
/*
{ aeson, base, bytestring, containers, data-default
, directory, http-client, http-client-tls, http-types, lib, parsec
, network-uri, process
, text, transformers
, scrappy-core
, scrappy-json
, henforcer
, pkgs, mkDerivation
}:
mkDerivation {
  pname = "llm-with-context";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring containers data-default directory http-client
    http-client-tls http-types network-uri parsec process scrappy-core
    scrappy-json henforcer text transformers
  ];
  license = lib.licenses.mit;
}
*/
