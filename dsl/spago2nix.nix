{ pkgs ? import <nixpkgs> {} }:

import (pkgs.fetchFromGitHub {
  owner = "justinwoo";
  repo = "spago2nix";
  rev = "262020b1bae872dac6db855fafe58a9999c91a28";
  sha256 = "0l678qjb73f1kvkk3l1pby2qg272dj166yxl7b1mcb0xhnjgig7g";
}) {}
