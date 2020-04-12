{ pkgs ? import <nixpkgs> { config = { allowBroken = true; }; }, compiler ? "ghc865" }:
let
  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      tasty-quickcheck-laws = pkgs.haskell.lib.dontCheck(super.tasty-quickcheck-laws);
      webdriver-w3c = pkgs.haskell.lib.dontCheck(super.webdriver-w3c);
      script-monad = pkgs.haskell.lib.dontCheck(super.script-monad);
    };
  };
  ghcide = (import (builtins.fetchTarball
    "https://github.com/cachix/ghcide-nix/tarball/master") { }).${"ghcide-${compiler}"};

  wtp = pkgs.haskell.lib.justStaticExecutables (pkgs.haskell.lib.dontHaddock
    (haskellPackages.callCabal2nix "wtp" ./. {
    }));

in haskellPackages.shellFor {
  withHoogle = true;
  packages = p: [wtp];
  buildInputs = (with pkgs;
  [
    ghcid
    ghcide
    cabal-install
    haskellPackages.ormolu
    firefox
    geckodriver
  ]);
}

