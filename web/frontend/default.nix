{ pkgs ? import ../../nixpkgs.nix {} }:
let
  easy-ps = import ./easy-ps.nix { inherit pkgs; };
  
  spagoPkgs = import ./spago-packages.nix { inherit pkgs; };

  getQuotedSourceGlob = x: ''"${x.src}/src/**/*.purs"'';

  psPkgs = (builtins.attrValues spagoPkgs.inputs) ++ [{ src = ./.; }];

  source-globs = map getQuotedSourceGlob psPkgs;

  bundle = pkgs.runCommand "bundle" {
    buildInputs = [ easy-ps.purs-0_13_8 ];
  } ''
      mkdir $out
      purs compile -g corefn ${toString source-globs} --output=$out
    '';

  in {
    inherit source-globs bundle;
  }
