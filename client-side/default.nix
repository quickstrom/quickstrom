{ pkgs ? import ../nixpkgs.nix { } }:
let
  src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
  scriptNames = builtins.map (pkgs.lib.removeSuffix ".ts")
    (builtins.attrNames (builtins.readDir ./src/scripts));
  browserifyScript = scriptName: ''
    echo "Bundling script ${scriptName} ..."
    browserify dist/scripts/${scriptName}.js -o dist/bundled/${scriptName}.js
  '';
  client-side = pkgs.stdenv.mkDerivation {
    inherit src;
    name = "webcheck-client-side";
    buildInputs = with pkgs; [
      # nodejs
      nodePackages.typescript
      nodePackages.browserify
    ];
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
