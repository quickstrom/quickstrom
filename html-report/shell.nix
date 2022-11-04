{ pkgs ? import ../nix/nixpkgs.nix  }:
import ./. { inherit pkgs; }
