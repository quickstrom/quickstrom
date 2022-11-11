{ pkgs ? (import ../nix/nixpkgs.nix), specstrom ? import ../nix/specstrom.nix
, quickstrom ? import ../default.nix { includeBrowsers = pkgs.stdenv.isLinux; }
}:
let
  todomvc = import ./todomvc.nix;
  py = pkgs.python38.withPackages (p: [ p.click ]);
in pkgs.mkShell {
  buildInputs = [ pkgs.chromedriver pkgs.geckodriver quickstrom py ]
    ++ pkgs.lib.optionals pkgs.stdenv.isLinux [ pkgs.firefox pkgs.chromium ];
  TODOMVC_DIR = todomvc;
}
