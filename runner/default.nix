{ pkgs ? import ../nixpkgs.nix { config = { allowBroken = true; }; }
, haskellPackages }:
let
  src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
  dsl = import ../dsl { inherit pkgs; };
  client-side = import ../client-side { inherit pkgs; };
  setCheckEnv = drv:
    pkgs.haskell.lib.overrideCabal drv (_: {
      preCheck = ''
        export QUICKSTROM_LIBRARY_DIR="${dsl}";
        export QUICKSTROM_CLIENT_SIDE_DIR="${client-side}";
      '';
    });
in setCheckEnv (haskellPackages.callCabal2nix "quickstrom-runner" "${src}" { })
