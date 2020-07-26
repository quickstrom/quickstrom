{ pkgs ? import ../nixpkgs.nix { config = { allowBroken = true; }; }
, haskellPackages }:
let
  src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
  dsl = import ../dsl { inherit pkgs; };
  client-side = import ../client-side { inherit pkgs; };
  setCheckEnv = drv:
    pkgs.haskell.lib.overrideCabal drv (_: {
      preCheck = ''
        export WEBCHECK_LIBRARY_DIR="${dsl}";
        export WEBCHECK_CLIENT_SIDE_BUNDLE="${client-side}/webcheck-client-side.js";
      '';
    });
in setCheckEnv (haskellPackages.callCabal2nix "webcheck-runner" "${src}" { })
