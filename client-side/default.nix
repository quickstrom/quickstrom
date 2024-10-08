{ lib, stdenv, nix-gitignore, nodejs, nodePackages }:
let
  src = nix-gitignore.gitignoreSource [ ] ./.;
  scriptNames = builtins.map (lib.removeSuffix ".ts")
    (builtins.attrNames (builtins.readDir ./src/scripts));
  browserifyScript = scriptName: ''
    echo "Bundling script ${scriptName} ..."
    echo "window.quickstrom = window.quickstrom || {};" > dist/bundled/${scriptName}.js
    browserify dist/scripts/${scriptName}.js >> dist/bundled/${scriptName}.js
    echo "return window.quickstrom.run.apply(null, arguments);" >> dist/bundled/${scriptName}.js
  '';
  client-side = stdenv.mkDerivation {
    inherit src;
    name = "quickstrom-client-side";
    buildInputs = [ nodejs nodePackages.typescript nodePackages.browserify ];
    buildPhase = ''
      echo "Compiling ${src} ..."
      tsc --outDir dist
      mkdir -p dist/bundled
      ${builtins.concatStringsSep "\n"
      (builtins.map browserifyScript scriptNames)}
    '';
    installPhase = ''
      mkdir $out
      cp dist/bundled/*.js $out/
    '';
  };

in client-side
