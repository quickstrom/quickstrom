{ pkgs ? import <nixpkgs> {} }:
import (pkgs.fetchFromGitHub {
  owner = "justinwoo";
  repo = "easy-purescript-nix";
  rev = "0ba91d9aa9f7421f6bfe4895677159a8a999bf20";
  sha256 = "1baq7mmd3vjas87f0gzlq83n2l1h3dlqajjqr7fgaazpa9xgzs7q";
}) {
  inherit pkgs;
}
