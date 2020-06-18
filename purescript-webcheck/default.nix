{ pkgs ? import ../nixpkgs.nix {} }:
let
  easy-ps = import ./easy-ps.nix { inherit pkgs; };
  
  spagoPkgs = import ./spago-packages.nix { inherit pkgs; };

  getQuotedSourceGlob = x: ''"${x.src}/src/**/*.purs"'';

  psPkgs = (builtins.attrValues spagoPkgs.inputs) ++ [{ src = ./.; }];

  sourceGlobs = map getQuotedSourceGlob psPkgs;

  sourceDirs = map (x: ''"${x.src}/src/"*'') psPkgs;

  purescript-webcheck = pkgs.runCommand "purescript-webcheck" {
    buildInputs = [ easy-ps.purs-0_13_8 ];
  } ''
      mkdir $out
      purs compile -g corefn ${toString sourceGlobs} --output=$out
    '';

  in purescript-webcheck
