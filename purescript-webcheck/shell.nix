{ pkgs ? import <nixpkgs> {} }:
let
  spago2nix = import ./spago2nix.nix { inherit pkgs; };
  drv = import ./. {};
in
pkgs.mkShell {
  inputsFrom = [drv];
  buildInputs = [spago2nix];
}
