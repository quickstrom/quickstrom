{ python3, poetry2nix, specstrom, bashInteractive, libffi, poetry, mkShell
, pyright, lib, stdenv, includeBrowsers ? false, firefox, chromium
, geckodriver, chromedriver, callPackage }:
let
  appEnv = poetry2nix.mkPoetryEnv {
    python = python3;
    projectDir = ./.;
    editablePackageSources = { my-app = ./.; };
    preferWheels = true;
  };

in mkShell {
  buildInputs = [
    bashInteractive

    appEnv
    # https://github.com/NixOS/nixpkgs/issues/11330
    libffi
    poetry

    specstrom
    pyright
  ] ++ lib.optional includeBrowsers [
    firefox
    chromium
    geckodriver
    chromedriver
  ];
  QUICKSTROM_CLIENT_SIDE_DIRECTORY = callPackage ./client-side { };
  QUICKSTROM_HTML_REPORT_DIRECTORY = callPackage ./html-report { };
}
