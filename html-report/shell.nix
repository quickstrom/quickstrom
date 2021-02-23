{ pkgs ? import ../nixpkgs.nix { config = { allowBroken = true; }; } }:
import ./. { inherit pkgs; }
