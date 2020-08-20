{ pkgs ? import ../nixpkgs.nix { config = { allowBroken = true; }; }
, haskellPackages }:
let
  src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
in haskellPackages.callCabal2nix "quickstrom-web" "${src}" { }
