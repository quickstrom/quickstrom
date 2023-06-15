{ pkgs ? (import ./nix/nixpkgs.nix), specstrom ? import ./nix/specstrom.nix }:
let

  quickstrom = pkgs.poetry2nix.mkPoetryApplication {
    projectDir = ./.;
    python = pkgs.python39;
    propagatedBuildInputs = [ specstrom ];
    checkInputs = [ pkgs.nodePackages.pyright ];
    checkPhase = ''
      pyright -p . quickstrom tests
      pytest
    '';

  };

  client-side = import ./client-side { inherit pkgs; };
  html-report = import ./html-report { inherit pkgs; };

  runtimeDeps = [ specstrom ];

  quickstrom-wrapped =
    pkgs.stdenv.mkDerivation {
      name = "quickstrom-wrapped";
      srcs = ./.;
      unpackPhase = false;
      buildInputs = [ pkgs.makeWrapper ];
      installPhase = ''
        mkdir -p $out/share
        cp -r ulib $out/share/ulib
        makeWrapper ${quickstrom}/bin/quickstrom $out/bin/quickstrom \
          --set QUICKSTROM_CLIENT_SIDE_DIRECTORY ${client-side} \
          --set QUICKSTROM_HTML_REPORT_DIRECTORY ${html-report} \
          --prefix PATH : ${pkgs.lib.makeBinPath runtimeDeps} \
          --add-flags "-I$out/share/ulib"

      '';
    };

  docker = pkgs.dockerTools.buildImage {
    name = "quickstrom/quickstrom";
    tag = "latest";
    copyToRoot = pkgs.buildEnv {
      name = "image-root";
      paths = [
        quickstrom-wrapped
        pkgs.coreutils
        pkgs.bashInteractive
        pkgs.dockerTools.caCertificates
        pkgs.firefox
        pkgs.geckodriver
        pkgs.chromium
        pkgs.chromedriver
      ];
      pathsToLink = [ "/bin" "/etc" ];
    };
    extraCommands = "mkdir -p -m 0777 tmp";
    config = {
      Cmd = [ "quickstrom" ];
      Env = [
        # Required for Chrome/Chromium rendering. It needs fallback fonts.
        "FONTCONFIG_FILE=${pkgs.fontconfig.out}/etc/fonts/fonts.conf"
      ];
    };
  };
in {
  quickstrom = quickstrom-wrapped;
  docker = docker;
}

