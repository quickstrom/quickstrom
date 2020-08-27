{ pkgs ? import ./nixpkgs.nix { config = { allowBroken = true; }; } }:
let
  drv = import ./. { inherit pkgs; };
  bash = pkgs.dockerTools.buildImage {
    name = "bash";
    tag = "latest";
    contents = pkgs.bashInteractive;
  };
in pkgs.dockerTools.buildImage {
  name = "quickstrom/quickstrom";
  tag = "latest";
  fromImage = bash;
  contents = [ drv.quickstrom ];
}
