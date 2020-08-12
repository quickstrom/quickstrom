{ pkgs ? import ../../nixpkgs.nix {} }:
let
  spago2nix = import ./spago2nix.nix { inherit pkgs; };
  easy-ps = import ./easy-ps.nix { inherit pkgs; };
  dsl = import ./. {};
in
pkgs.mkShell {
  inputsFrom = [dsl.bundle];
  buildInputs = [spago2nix easy-ps.spago pkgs.dhall];
  PURS_IDE_SOURCES = dsl.source-globs;
}
