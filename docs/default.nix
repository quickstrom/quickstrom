{
  stdenv,
  nix-gitignore,
  python3,
  graphviz,
  texlive,
  quickstrom
}:
let
  src = nix-gitignore.gitignoreSource [ ] ./.;
  sphinx-env = python3.withPackages (ps: [
    ps.sphinx
    ps.sphinx-autobuild
    ps.sphinx_rtd_theme
  ]);
  dependencies = [
    sphinx-env
    texlive.combined.scheme-basic
    graphviz
  ];
in
stdenv.mkDerivation {
  inherit src;
  name = "docs";
  buildInputs = dependencies;
  buildPhase = ''
    make html
  '';
  installPhase = ''
    cp -r build/html $out
  '';
}
