{ pkgs ? import ../../nixpkgs.nix { } }:
let
  frontend = pkgs.mkYarnPackage {
    name = "frontend";
    src = ./.;
    packageJSON = ./package.json;
    yarnLock = ./yarn.lock;
    # NOTE: this is optional and generated dynamically if omitted
    yarnNix = ./yarn.nix;
    buildPhase = "yarn build";
    installPhase = "mv /build/$pname/deps/$pname/build $out";
    distPhase = "true";
  };
in frontend
