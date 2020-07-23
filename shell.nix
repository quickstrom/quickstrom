{ pkgs ? import ./nixpkgs.nix { config = { allowBroken = true; }; }
, compiler ? "ghc865" }:
let
  ghcide = (import (builtins.fetchTarball
    "https://github.com/cachix/ghcide-nix/tarball/master")
    { }).${"ghcide-${compiler}"};

  fonts = with pkgs; [ libre-baskerville iosevka opensans-ttf ];

  easy-ps = import ./dsl/easy-ps.nix { inherit pkgs; };
  dsl = import ./dsl { inherit pkgs; };
  client-side = import ./client-side { inherit pkgs; };

  webcheck-purs-ide = pkgs.writeShellScriptBin "webcheck-purs-ide" ''
    ${easy-ps.purs-0_13_8}/bin/purs ide server \
      --log-level all \
      --output-directory dsl/output \
      'dsl/src/**/*.purs' \
      'dsl/.spago/**/*.purs' \
      'specs/**/*.purs'
  '';

  webcheck-format-sources =
    pkgs.writeShellScriptBin "webcheck-format-sources" ''
      find . -not -path './dist-newstyle/*' -name '*.hs' -exec ormolu -m inplace {} \;
      find . -not -path './dsl/.spago/*' -name '*.purs' -exec purty --write {} \;
    '';

  webcheck = import ./. { inherit pkgs compiler; };

in webcheck.haskellPackages.shellFor {
  withHoogle = true;
  packages = with pkgs.lib; (p: attrValues (filterAttrs (n: _: hasPrefix "webcheck-" n) p));
  buildInputs = (with pkgs; [
    nixfmt
    ghcid
    ghcide
    cabal-install
    haskellPackages.ormolu
    firefox
    geckodriver
    haskellPackages.ghc-prof-flamegraph
    easy-ps.purty
    # chromium
    # chromedriver

    webcheck-purs-ide
    webcheck-format-sources
    # only for lorri
    dsl
    client-side
  ]);
  FONTCONFIG_FILE = pkgs.makeFontsConf { fontDirectories = fonts; };
  WEBCHECK_LIBRARY_DIR = "${dsl}";
  WEBCHECK_CLIENT_SIDE_DIR = "${client-side}";
}

