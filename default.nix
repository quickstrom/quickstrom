{ pkgs ? (import ./nix/nixpkgs.nix), specstrom ? import ./nix/specstrom.nix
, chromedriver ? pkgs.chromedriver, includeBrowsers ? true }:
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

  runtimeDeps = [ specstrom ] ++ pkgs.lib.optionals includeBrowsers [
    chromedriver
    pkgs.geckodriver
    pkgs.chromium
    pkgs.firefox
  ];

  quickstrom-wrapped = { includeBrowsers }:
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
          --set PATH ${pkgs.lib.makeBinPath runtimeDeps} \
          --add-flags "-I$out/share/ulib"

      '';
    };

  ubuntu = pkgs.dockerTools.pullImage {
    imageName = "ubuntu";
    imageDigest =
      "sha256:edc5125bd9443ab5d5c92096cf0e481f5e8cb12db9f5461ab1ab7a936c7f7d30";
    sha256 = "sha256-TZSqaLl28S71CLmfn5HEIN+/1UCPMrLlqpr5D0VcULg=";
    finalImageTag = "22.10";
    finalImageName = "ubuntu";
  };

  docker = pkgs.dockerTools.buildImage {
    fromImage = ubuntu;
    name = "quickstrom/quickstrom";
    tag = "latest";
    copyToRoot = pkgs.buildEnv {
      name = "image-root";
      paths = [
        (quickstrom-wrapped { includeBrowsers = true; })
        pkgs.bashInteractive
        pkgs.dockerTools.caCertificates
      ];
      pathsToLink = [ "/bin" "/etc" ];
    };
    config = {
      Cmd = [ "quickstrom" ];
      Env = [
        # Required for Chrome/Chromium rendering. It needs fallback fonts.
        "FONTCONFIG_FILE=${pkgs.fontconfig.out}/etc/fonts/fonts.conf"
       ];
    };
  };
in {
  quickstrom = quickstrom-wrapped { inherit includeBrowsers; };
  docker = docker;
}

