{ pkgs ? import ../nixpkgs.nix { } }:
let
  src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
  sphinx-env = pkgs.python37.withPackages (ps: [ ps.sphinx ps.sphinx_rtd_theme ps.sphinx-autobuild ]);
  dependencies = [ sphinx-env pkgs.texlive.combined.scheme-basic pkgs.graphviz ];
  site = pkgs.stdenv.mkDerivation {
    inherit src;
    name = "docs";
    buildInputs = dependencies;
    buildPhase = ''
      make html
    '';
    installPhase = ''
      cp -r build/html $out
    '';
  };
  publish = pkgs.writeShellScriptBin "quickstrom-publish-docs" ''
    ${pkgs.ghp-import}/bin/ghp-import -p -f -l --cname docs.quickstrom.io ${site}
  '';
in { inherit site publish dependencies; }

