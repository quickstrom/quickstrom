{ pkgs ? import ../nixpkgs.nix { } }:
let
  src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
  sphinx-env = pkgs.python.withPackages (ps: [ ps.sphinx ps.sphinx_rtd_theme ]);
  site = pkgs.stdenv.mkDerivation {
    inherit src;
    name = "docs";
    buildInputs = [ sphinx-env pkgs.texlive.combined.scheme-basic ];
    buildPhase = ''
      make html
    '';
    installPhase = ''
      cp -r build/html $out
    '';
  };
  publish = pkgs.writeShellScriptBin "quickstrom-publish-docs" ''
    ${pkgs.ghp-import}/bin/ghp-import -p -l ${site}
  '';
in { inherit site publish; }

