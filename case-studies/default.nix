{ pkgs ? (import ../nix/nixpkgs.nix), specstrom ? (import ../nix/specstrom.nix)
, quickstrom ? import ../default.nix { inherit specstrom; } }:
let
  src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
  py = pkgs.python38.withPackages (p: [ p.click ]);
  todomvc = import ./todomvc.nix;

  run = pkgs.writeShellApplication {
    name = "run-case-study";
    runtimeInputs = [py];
    text = ''
      export TODOMVC_DIR=${todomvc}
      pushd ${src}
      python run.py /tmp/case-study/results "$@"
      popd
    '';
  };
in pkgs.dockerTools.buildImage {
  name = "case-study";
  tag = "firefox";
  contents = [
    pkgs.coreutils
    pkgs.bashInteractive
    quickstrom
    run
  ];
  config = { Cmd = ["run-case-study"]; };
}
