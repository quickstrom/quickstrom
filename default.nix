{ pkgs ? (import ./nix/nixpkgs.nix)
, specstrom ? import ./nix/specstrom.nix
, chromedriver ? pkgs.chromedriver
, includeBrowsers ? true
}:
let
  poetry2nix = import ./nix/poetry2nix.nix { inherit pkgs; };

  quickstrom = poetry2nix.mkPoetryApplication {
    projectDir = ./.;
    python = pkgs.python39;
    propagatedBuildInputs = [ specstrom ];
    checkInputs = [ pkgs.nodePackages.pyright ];
    checkPhase = ''
      pyright -p . quickstrom tests
      pytest
    '';

  };

  client-side = import ./client-side { inherit pkgs; };
  html-report = import ./html-report { inherit pkgs; };

  runtimeDeps = [ specstrom chromedriver ]
    ++ pkgs.lib.optionals includeBrowsers [ pkgs.chromium ];

  quickstrom-wrapped = { includeBrowsers }:
    pkgs.stdenv.mkDerivation {
      name = "quickstrom-wrapped";
      src = ./.;
      buildInputs = [ pkgs.makeWrapper ];
      installPhase = ''
        mkdir -p $out/share
        cp -r ulib $out/share/ulib
        makeWrapper ${quickstrom}/bin/quickstrom $out/bin/quickstrom \
          --set QUICKSTROM_CLIENT_SIDE_DIRECTORY ${client-side} \
          --set QUICKSTROM_HTML_REPORT_DIRECTORY ${html-report} \
          --set PATH ${pkgs.lib.makeBinPath runtimeDeps} \
          --add-flags "-I$out/share/ulib"

      '';
    };

  docker = pkgs.dockerTools.buildImage {
    name = "quickstrom";
    contents =
      [ pkgs.coreutils (quickstrom-wrapped { includeBrowsers = true; }) ];
    config = { Cmd = [ "quickstrom" ]; };
  };
in {
  quickstrom = quickstrom-wrapped { inherit includeBrowsers; };
  docker = docker;
}

