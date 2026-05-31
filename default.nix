let
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/refs/heads/nixos-24.11.tar.gz";
  }) { };
  hlib = pkgs.haskell.lib;
  hp = pkgs.haskellPackages;

  # Filter out the cabal artifacts / multi-package project file so callCabal2nix
  # sees a clean single-package source.
  cleanSrc = src:
    pkgs.lib.cleanSourceWith {
      inherit src;
      filter = path: _type:
        let base = baseNameOf path;
        in base != "dist-newstyle" && base != "cabal.project" && base != ".git";
    };

  scrappy-core = hlib.dontCheck
    (hp.callCabal2nix "scrappy-core" (cleanSrc /home/lazylambda/code/ace-ws/thunks/scrappy-core) { });

  # bubblewrap must be on PATH at COMPILE time so $(staticWhich "bwrap") in
  # LLM.Effect.Tool.Sandbox resolves to its /nix/store path. addBuildTool puts
  # it there; because the resolved path is baked into the object code, nix then
  # tracks bubblewrap as a runtime dependency automatically.
  llm-with-context =
    hlib.addBuildTool
      (hp.callCabal2nix "llm-with-context" (cleanSrc ./.) { inherit scrappy-core; })
      pkgs.bubblewrap;
in
hlib.doCheck llm-with-context
