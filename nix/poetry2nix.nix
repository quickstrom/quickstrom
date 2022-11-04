{ pkgs }:
pkgs.callPackage (fetchTarball "https://github.com/nix-community/poetry2nix/archive/88ffae91c605aaafc2797f4096ca9f065152796a.tar.gz") { inherit pkgs; }
