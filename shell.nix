{ pkgs ? import ./nixpkgs.nix { config = { allowBroken = true; }; }, compiler ? "ghc865" }:
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
  client-side = import ./client-side { inherit pkgs; };

  webcheck-purs-ide = pkgs.writeShellScriptBin "webcheck-purs-ide" ''
    ${easy-ps.purs-0_13_8}/bin/purs ide server \
      --log-level all \
      --output-directory purescript-webcheck/output \
      'purescript-webcheck/src/**/*.purs' \
      'purescript-webcheck/.spago/**/*.purs' \
      'specs/**/*.purs'
  '';

  webcheck-format-sources = pkgs.writeShellScriptBin "webcheck-format-sources" ''
    find . -not -path './dist-newstyle/*' -name '*.hs' -exec ormolu -m inplace {} \;
  '';

  src = pkgs.nix-gitignore.gitignoreSource [] ./.;

  webcheck = import ./. { inherit pkgs compiler; };

in haskellPackages.shellFor {
  withHoogle = true;
  packages = p: [webcheck.package];
  buildInputs = (with pkgs;
  [
    nixfmt
    ghcid
    ghcide
    cabal-install
    haskellPackages.ormolu
    firefox
    geckodriver
    haskellPackages.ghc-prof-flamegraph
    # chromium
    # chromedriver

    webcheck-purs-ide
    webcheck-format-sources
    # only for lorri
    purescript-webcheck
    client-side
  ]);
  FONTCONFIG_FILE = pkgs.makeFontsConf {
    fontDirectories = fonts;
  };
  WEBCHECK_LIBRARY_DIR = "${purescript-webcheck}";
  WEBCHECK_CLIENT_SIDE_BUNDLE = "${client-side}/webcheck.js";
}

