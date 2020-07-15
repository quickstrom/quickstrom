{ pkgs ? import ./nixpkgs.nix { config = { allowBroken = true; }; }, compiler ? "ghc865" }:
let
  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      tasty-quickcheck-laws = pkgs.haskell.lib.dontCheck(super.tasty-quickcheck-laws);
      webdriver-w3c = pkgs.haskell.lib.dontCheck(super.webdriver-w3c);
      script-monad = pkgs.haskell.lib.dontCheck(super.script-monad);
      protolude = pkgs.haskell.lib.doJailbreak(self.callHackage "protolude" "0.2.3" {});
    };
  };

  dsl = import ./dsl { inherit pkgs; };
  client-side = import ./client-side { inherit pkgs; };

  src = pkgs.nix-gitignore.gitignoreSource [] ./.;

  setCheckEnv = drv: pkgs.haskell.lib.overrideCabal drv (_: {
      preCheck = ''
        export WEBCHECK_LIBRARY_DIR="${dsl}";
        export WEBCHECK_CLIENT_SIDE_BUNDLE="${client-side}/webcheck-client-side.js";
      '';
  });

  package =
    pkgs.haskell.lib.justStaticExecutables 
    (pkgs.haskell.lib.dontHaddock
        (setCheckEnv
            (haskellPackages.callCabal2nix "WebCheck" "${src}" {})));

  webcheck = pkgs.stdenv.mkDerivation { 
      name = "webcheck-wrapped";
      unpackPhase = "true";
      buildPhase = '''';
      nativeBuildInputs = [pkgs.makeWrapper];
      installPhase = ''
        mkdir -p $out/bin
        makeWrapper "${package}/bin/webcheck" \
            $out/bin/webcheck \
            --set WEBCHECK_LIBRARY_DIR "${dsl}" \
            --set WEBCHECK_CLIENT_SIDE_BUNDLE "${client-side}/webcheck.js"
      '';
  };

in {
    webcheck = webcheck;
    package = package;
}

