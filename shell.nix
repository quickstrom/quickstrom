{ pkgs ? import <nixpkgs> { config = { allowBroken = true; }; }, compiler ? "ghc865" }:
let
  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      tasty-quickcheck-laws = pkgs.haskell.lib.dontCheck(super.tasty-quickcheck-laws);
      webdriver-w3c = pkgs.haskell.lib.dontCheck(super.webdriver-w3c);
      script-monad = pkgs.haskell.lib.dontCheck(super.script-monad);
      protolude = pkgs.haskell.lib.doJailbreak(self.callHackage "protolude" "0.2.3" {});
    };
  };
  ghcide = (import (builtins.fetchTarball
    "https://github.com/cachix/ghcide-nix/tarball/master") { }).${"ghcide-${compiler}"};

  fonts = with pkgs; [
    libre-baskerville
    iosevka
    opensans-ttf
  ];

  easy-ps = import ./purescript-webcheck/easy-ps.nix { inherit pkgs; };

  purescript-webcheck = import ./purescript-webcheck { inherit pkgs; };


  src = pkgs.nix-gitignore.gitignoreSource [] ./.;

  webcheck = pkgs.haskell.lib.justStaticExecutables (pkgs.haskell.lib.dontHaddock
    (haskellPackages.callCabal2nix "webcheck" "${src}/webcheck.cabal" {
    }));

in haskellPackages.shellFor {
  withHoogle = true;
  packages = p: [webcheck];
  buildInputs = (with pkgs;
  [
    # ghcid
    ghcide
    cabal-install
    haskellPackages.ormolu
    firefox
    geckodriver
    # chromium
    # chromedriver
    nodejs
    nodePackages.typescript
    nodePackages.browserify
    entr
  ]) ++ (with easy-ps; [purs]);
  FONTCONFIG_FILE = pkgs.makeFontsConf {
    fontDirectories = fonts;
  };
  PURESCRIPT_WEBCHECK = "${purescript-webcheck}";
}

