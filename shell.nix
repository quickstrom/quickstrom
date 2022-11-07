{ pkgs ? (import ./nix/nixpkgs.nix), specstrom ? import ./nix/specstrom.nix }:
let
  appEnv = pkgs.poetry2nix.mkPoetryEnv {
    python = pkgs.python39;
    projectDir = ./.;
    editablePackageSources = { my-app = ./.; };
  };

in pkgs.mkShell {
  buildInputs = [
    pkgs.bashInteractive

    appEnv
    pkgs.poetry

    pkgs.geckodriver
    pkgs.firefox
    pkgs.chromedriver
    pkgs.google-chrome

    specstrom
    pkgs.nodePackages.pyright
  ] ++ pkgs.lib.optional pkgs.stdenv.isLinux [ pkgs.firefox pkgs.chromium ];
  QUICKSTROM_CLIENT_SIDE_DIRECTORY = import ./client-side { inherit pkgs; };
  QUICKSTROM_HTML_REPORT_DIRECTORY = import ./html-report { inherit pkgs; };
}
