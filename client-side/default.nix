{ pkgs ? import ../nixpkgs.nix { } }:
let

  client-side = pkgs.stdenv.mkDerivation  {
    name = "webcheck-client-side";
    src = pkgs.nix-gitignore.gitignoreSource [] ./.;
    buildInputs = with pkgs; [
      # nodejs
      nodePackages.typescript
      nodePackages.browserify
    ];
    buildPhase = ''
        tsc
        browserify -s webcheck target/webcheck.js -o target/webcheck-client-side.js
    '';
    installPhase = ''
        mkdir $out
        cp target/webcheck-client-side.js $out/
    '';
  };

in client-side
