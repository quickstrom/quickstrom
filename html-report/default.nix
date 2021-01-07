{ pkgs ? import ../nixpkgs.nix { config = { allowBroken = true; }; } }:
let
  html-report = pkgs.mkYarnPackage {
    name = "html-report";
    src = ./.;
    packageJSON = ./package.json;
    yarnLock = ./yarn.lock;
    yarnNix = ./yarn.nix;

    buildPhase = ''
      pwd
      yarn --offline build
      ls -lh
    '';
    installPhase = "mv /build/$pname/deps/$pname/dist $out";
    distPhase = "true";
  };
in html-report
