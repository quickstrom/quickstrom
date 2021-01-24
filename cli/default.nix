{ pkgs ? import ../nixpkgs.nix { config = { allowBroken = true; }; }
, haskellPackages, git-rev ? null }:
let
  src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;

  setBuildEnv = drv:
    pkgs.haskell.lib.overrideCabal drv (_: {
      preBuild = if git-rev != null then ''
        export QUICKSTROM_GIT_REV="${git-rev}";
      '' else
        "true";
    });
in setBuildEnv (haskellPackages.callCabal2nix "quickstrom-cli" "${src}" { })
