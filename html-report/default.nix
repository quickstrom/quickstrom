{ nix-gitignore, mkYarnPackage }:
let
  src = nix-gitignore.gitignoreSource [ ] ./.;
  html-report = mkYarnPackage {
    inherit src;
    name = "html-report";
    packageJSON = ./package.json;
    yarnLock = ./yarn.lock;
    buildPhase = "yarn --offline build";
    installPhase = "ls -lh; mv deps/$pname/dist $out"; # seems like a giant hack
    distPhase = "true";
  };
in html-report
