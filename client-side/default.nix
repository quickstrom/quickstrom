{ pkgs ? import ../nixpkgs.nix { } }:
let
  src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
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
      tsc
    '';
    installPhase = ''
      mkdir $out
      cp dist/*.js $out/
    '';
  };

in client-side
