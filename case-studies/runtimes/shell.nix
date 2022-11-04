{ pkgs ? (import ../../nix/nixpkgs.nix)
}:
let
  py = pkgs.python39.withPackages (p: [ p.jupyter p.ipython p.pandas p.pip p.notebook ]);
in pkgs.mkShell {
  buildInputs = [py];
}
