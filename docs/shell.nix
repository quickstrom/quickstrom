{ pkgs ? (import ../nix/nixpkgs.nix) }:
let docs = (import ./. { inherit pkgs; });
in pkgs.mkShell { buildInputs = [ pkgs.bashInteractive docs.dependencies ]; }
