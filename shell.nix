{ pkgs ? (import ./nix/nixpkgs.nix), specstrom ? import ./nix/specstrom.nix }:
let
  poetry2nix = import ./nix/poetry2nix.nix { inherit pkgs; };
  appEnv = poetry2nix.mkPoetryEnv {
    projectDir = ./.;
    editablePackageSources = { my-app = ./.; };
  };

in pkgs.mkShell {
  buildInputs = [
    pkgs.bashInteractive

    appEnv
    pkgs.poetry

    pkgs.geckodriver
    pkgs.chromedriver

    specstrom
    pkgs.nodePackages.pyright
  ] ++ pkgs.lib.optional pkgs.stdenv.isLinux [ pkgs.firefox pkgs.chromium ];
  QUICKSTROM_CLIENT_SIDE_DIRECTORY = import ./client-side { inherit pkgs; };
  QUICKSTROM_HTML_REPORT_DIRECTORY = import ./html-report { inherit pkgs; };
}
