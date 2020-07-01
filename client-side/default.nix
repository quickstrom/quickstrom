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
      echo -e "window.webcheck = {}; (function (exports) {\n" >> dist/webcheck-bundle.js
      cat dist/webcheck.js >> dist/webcheck-bundle.js
      echo -e "\n})(window.webcheck);" >> dist/webcheck-bundle.js
    '';
    installPhase = ''
        mkdir $out
        cp dist/webcheck-bundle.js $out/webcheck.js
    '';
  };

in client-side
