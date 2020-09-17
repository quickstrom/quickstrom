{ pkgs ? import ./nixpkgs.nix { config = { allowBroken = true; }; }
, compiler ? "ghc865" }:
let
  inherit (pkgs.lib.systems.elaborate { system = builtins.currentSystem; })
    isLinux;

  dsl = import ./dsl { inherit pkgs; };
  client-side = import ./client-side { inherit pkgs; };

  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      tasty-quickcheck-laws =
        pkgs.haskell.lib.dontCheck (super.tasty-quickcheck-laws);
      webdriver-w3c = pkgs.haskell.lib.dontCheck (super.webdriver-w3c);
      script-monad = pkgs.haskell.lib.dontCheck (super.script-monad);
      protolude =
        pkgs.haskell.lib.doJailbreak (self.callHackage "protolude" "0.2.3" { });

      # For higgledy. Remove overrides once versions have been bumped.
      higgledy = pkgs.haskell.lib.dontCheck (self.callHackage "higgledy" "0.3.1.0" { });
      barbies = self.callHackage "barbies" "1.1.3.0" { };
      generic-lens = pkgs.haskell.lib.dontCheck (self.callHackage "generic-lens" "1.1.0.0" { });


      quickstrom-runner = pkgs.haskell.lib.disableLibraryProfiling(import ./runner {
        inherit pkgs;
        haskellPackages = self;
      });
      quickstrom-cli = pkgs.haskell.lib.justStaticExecutables(import ./cli {
        inherit pkgs;
        haskellPackages = self;
      });
      quickstrom-web = pkgs.haskell.lib.justStaticExecutables(import ./web {
        inherit pkgs;
        haskellPackages = self;
      });
    };
  };

  quickstrom = pkgs.stdenv.mkDerivation {
    name = "quickstrom";
    unpackPhase = "true";
    buildPhase = "";
    nativeBuildInputs = [ pkgs.makeWrapper ];
    installPhase = ''
      mkdir -p $out/bin
      makeWrapper "${haskellPackages.quickstrom-cli}/bin/quickstrom" \
          $out/bin/quickstrom \
          --set QUICKSTROM_LIBRARY_DIR "${dsl}" \
          --set QUICKSTROM_CLIENT_SIDE_DIR "${client-side}"
    '';
  };

in { inherit haskellPackages quickstrom; }
