{
  callPackage,
  lib,
  stdenv,
  nix-gitignore,
  makeWrapper,
  buildEnv,
  dockerTools,
  specstrom,
  python3,
  poetry2nix,
  pyright,
  chromedriver,
  geckodriver,
  chromium,
  firefox,
  includeBrowsers ? true,
}:
let
  quickstrom = poetry2nix.mkPoetryApplication {
    python = python3;
    projectDir = ./.;
    src = nix-gitignore.gitignoreSource [
      "docs"
      "integration-tests"
    ] ./.;
    propagatedBuildInputs = [ specstrom ];
    preferWheels = true;
    checkPhase = ''
      ${pyright}/bin/pyright -p . quickstrom tests
      pytest
    '';

  };

  client-side = callPackage ./client-side { };
  html-report = callPackage ./html-report { };

  runtimeDeps =
    [ specstrom ]
    ++ lib.optionals includeBrowsers [
      chromedriver
      geckodriver
      chromium
      firefox
    ];

  quickstrom-wrapped = stdenv.mkDerivation {
    name = "quickstrom-wrapped";
    srcs = ./.;
    unpackPhase = false;
    buildInputs = [ makeWrapper ];
    installPhase = ''
      mkdir -p $out/share
      cp -r ulib $out/share/ulib
      makeWrapper ${quickstrom}/bin/quickstrom $out/bin/quickstrom \
        --set QUICKSTROM_CLIENT_SIDE_DIRECTORY ${client-side} \
        --set QUICKSTROM_HTML_REPORT_DIRECTORY ${html-report} \
        --prefix PATH : ${lib.makeBinPath runtimeDeps} \
        --add-flags "-I$out/share/ulib"

    '';
  };

in
quickstrom-wrapped
