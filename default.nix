let
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/refs/heads/nixos-24.11.tar.gz";
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
  llm-with-context =
    hlib.addBuildTool
      (hp.callCabal2nix "llm-with-context" (cleanSrc ./.) { inherit scrappy-core scrappy-json; })
      pkgs.bubblewrap;
in
hlib.doCheck llm-with-context
