{ pkgs ? import ./nixpkgs.nix { config = { allowBroken = true; allowUnfree = true; }; }
, compiler ? "ghc865" }:
let
  fonts = with pkgs; [ libre-baskerville iosevka opensans-ttf ];
  inherit (pkgs.lib.systems.elaborate { system = builtins.currentSystem; })
    isLinux;

  easy-ps = import ./dsl/easy-ps.nix { inherit pkgs; };
  dsl = import ./dsl { inherit pkgs; };
  client-side = import ./client-side { inherit pkgs; };

  quickstrom-purs-ide = pkgs.writeShellScriptBin "quickstrom-purs-ide" ''
    ${easy-ps.purs-0_13_8}/bin/purs ide server \
      --log-level all \
      --output-directory dsl/output \
      'dsl/src/**/*.purs' \
      'dsl/.spago/**/*.purs' \
      'specs/**/*.purs'
  '';

  quickstrom-format-sources =
    pkgs.writeShellScriptBin "quickstrom-format-sources" ''
      find . -not -path './dist-newstyle/*' -name '*.hs' -exec ormolu -m inplace {} \;
      find . -not -path './dsl/.spago/*' -name '*.purs' -exec purty --write {} \;
    '';

  docs = import ./docs { inherit pkgs; };

  quickstrom = import ./. { inherit pkgs compiler; };

in quickstrom.haskellPackages.shellFor {
  withHoogle = true;
  packages = with pkgs.lib; (p: attrValues (filterAttrs (n: _: hasPrefix "quickstrom-" n) p));
  buildInputs = (with pkgs; [
    nixfmt
    ghcid
    quickstrom.haskellPackages.haskell-language-server
    cabal-install
    quickstrom.haskellPackages.ormolu
    easy-ps.purty

    quickstrom-purs-ide
    quickstrom-format-sources

    docs.dependencies
    docs.publish

    # only for lorri
    dsl
    client-side
  ] ++ lib.optional isLinux [
    firefox
    geckodriver
    chromium
    chromedriver
    google-chrome
    selenium-server-standalone
  ]);
  FONTCONFIG_FILE = pkgs.makeFontsConf { fontDirectories = fonts; };
  QUICKSTROM_LIBRARY_DIR = "${dsl}";
  QUICKSTROM_CLIENT_SIDE_DIR = "${client-side}";
}

