{ pkgs ? import ../nixpkgs.nix { } }:
let
  src = pkgs.nix-gitignore.gitignoreSource [] ./.;
  client-side = pkgs.stdenv.mkDerivation  {
    inherit src;
    name = "webcheck-client-side";
    buildInputs = with pkgs; [
      # nodejs
      nodePackages.typescript
      nodePackages.browserify
    ];
    buildPhase = ''
        echo "Compiling ${src} ..."
        tsc
        browserify -s webcheck target/webcheck.js -o target/webcheck-client-side.js
    '';
    installPhase = ''
        mkdir $out
        cp target/webcheck-client-side.js $out/
    '';
  };

in client-side
