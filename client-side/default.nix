{ pkgs ? import ../nixpkgs.nix { } }:
let
  src = pkgs.nix-gitignore.gitignoreSource [] ./.;
  client-side = pkgs.mkYarnPackage {
    name = "webcheck-client-side";
    src = ./.;
    packageJSON = ./package.json;
    yarnLock = ./yarn.lock;
    # NOTE: this is optional and generated dynamically if omitted
    yarnNix = ./yarn.nix;
    extraBuildInputs = with pkgs; [
      yarn
      yarn2nix
    ];
    installPhase = ''
      mkdir -p $out
      yarn run parcel build src/webcheck.ts -d $out
    '';
    distPhase = ''true'';
  };
in client-side
