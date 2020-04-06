{ pkgs ? import <nixpkgs> { }, compiler ? "ghc865" }:
let
  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: { };
  };
  ghcide = (import (builtins.fetchTarball
    "https://github.com/cachix/ghcide-nix/tarball/master") { }).${"ghcide-${compiler}"};

  wtp = pkgs.haskell.lib.justStaticExecutables (pkgs.haskell.lib.dontHaddock
    (haskellPackages.callCabal2nix "wtp" ./. { }));

in haskellPackages.shellFor {
  withHoogle = true;
  packages = p: [wtp];
  buildInputs = (with pkgs;
  [
    ghcide
    cabal-install
    haskellPackages.ormolu
  ]);
}

