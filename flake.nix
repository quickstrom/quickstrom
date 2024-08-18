{
  description = "Quickstrom: an autonomous testing tool for the web";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.specstrom = {
    url = "github:quickstrom/specstrom";
  };
  inputs.poetry2nix = {
    url = "github:nix-community/poetry2nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      specstrom,
      poetry2nix,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        specstrom-overlay = (self: super: { specstrom = specstrom.packages.${system}.default; });
        pkgs = (
          import nixpkgs {
            inherit system;
            overlays = [ specstrom-overlay ];
            config = { };
          }
        );
        poetry2nix = inputs.poetry2nix.lib.mkPoetry2Nix { inherit pkgs; };
      in
      {
        packages = {
          default = pkgs.callPackage ./default.nix {
            inherit poetry2nix;
            includeBrowsers = false;
          };
          docker = pkgs.callPackage ./docker.nix { quickstrom = self.packages.${system}.default; };
        };
        devShells = {
          default = pkgs.callPackage ./shell.nix {
            inherit poetry2nix;
            includeBrowsers = system == "x86_64-linux";
          };
          integration-tests = pkgs.callPackage ./integration-tests/shell.nix {
            quickstrom = self.packages.${system}.default;
          };
          docs = pkgs.callPackage ./docs/default.nix { quickstrom = self.packages.${system}.default; };
        };
      }
    );
}
