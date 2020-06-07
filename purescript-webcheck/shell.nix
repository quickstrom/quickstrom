{ pkgs ? import <nixpkgs> {} }:
let
  spago2nix = import ./spago2nix.nix { inherit pkgs; };
  easy-ps = import ./easy-ps.nix { inherit pkgs; };
  drv = import ./. {};
in
pkgs.mkShell {
  inputsFrom = [drv];
  buildInputs = [spago2nix easy-ps.spago];
}
