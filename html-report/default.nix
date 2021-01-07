{ pkgs ? import ../nixpkgs.nix { config = { allowBroken = true; }; } }:
let

  src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
  html-report = pkgs.mkYarnPackage {
    inherit src;
    name = "html-report";
    packageJSON = ./package.json;
    yarnLock = ./yarn.lock;
    yarnNix = ./yarn.nix;
    buildPhase = "yarn --offline build";
    installPhase = "mv /build/$pname/deps/$pname/dist $out"; # seems like a giant hack
    distPhase = "true";
  };
in html-report
