{
  description = "Quickstrom: an autonomous testing tool for the web";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.systems.url = "github:nix-systems/default";
  inputs.specstrom.url = "github:quickstrom/specstrom/flake";
  inputs.poetry2nix = {
    url = "github:nix-community/poetry2nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ self, systems, nixpkgs, specstrom, poetry2nix }:
    let eachSystem = nixpkgs.lib.genAttrs (import systems);
    in {
      packages = eachSystem (system:
        let
          specstrom-overlay = (self: super: {
            specstrom = specstrom.packages.${system}.default;
          });
          pkgs = (import nixpkgs {
            inherit system;
            overlays = [ specstrom-overlay ];
            config = { permittedInsecurePackages = [ "openssl-1.1.1w" ]; };
          });
          poetry2nix = inputs.poetry2nix.lib.mkPoetry2Nix { inherit pkgs; };
        in {
          default = pkgs.callPackage ./default.nix {
            inherit poetry2nix;
            includeBrowsers = false;
          };
          without-browsers = pkgs.callPackage ./default.nix {
            inherit poetry2nix;
            includeBrowsers = true;
          };
        });
    };

}
