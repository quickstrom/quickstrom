{ pkgs ? import ../nix/nixpkgs.nix }:
let
  src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
  html-report = pkgs.mkYarnPackage {
    inherit src;
    name = "html-report";
    packageJSON = ./package.json;
    yarnLock = ./yarn.lock;
    buildPhase = "yarn --offline build";
    installPhase = "ls -lh; mv deps/$pname/dist $out"; # seems like a giant hack
    distPhase = "true";
  };
in html-report
