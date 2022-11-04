{ pkgs ? import ../nix/nixpkgs.nix }:
pkgs.mkShell {
    buildInputs = with pkgs; [
      nodejs
      nodePackages.typescript
      nodePackages.browserify
      nodePackages.prettier
    ];
  }
