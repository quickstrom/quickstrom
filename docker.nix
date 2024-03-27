
{ buildEnv, dockerTools, coreutils, bashInteractive, fontconfig, quickstrom }:
  docker = dockerTools.buildImage {
    name = "quickstrom/quickstrom";
    tag = "latest";
    copyToRoot = buildEnv {
      name = "image-root";
      paths = [
        (quickstrom-wrapped { includeBrowsers = true; })
        coreutils
        bashInteractive
        dockerTools.caCertificates
      ];
      pathsToLink = [ "/bin" "/etc" ];
    };
    extraCommands = "mkdir -p -m 0777 tmp";
    config = {
      Cmd = [ "quickstrom" ];
      Env = [
        # Required for Chrome/Chromium rendering. It needs fallback fonts.
        "FONTCONFIG_FILE=${fontconfig.out}/etc/fonts/fonts.conf"
      ];
    };
  };
