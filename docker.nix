{ pkgs ? import ./nixpkgs.nix { config = { allowBroken = true; }; }
, git-rev ? null
}:
let
  drv = import ./. { inherit pkgs git-rev; };
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
