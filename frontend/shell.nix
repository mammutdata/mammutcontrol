with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "mammut-control";
  buildInputs = [
    haskellPackages.purescript
    nodejs-10_x
    nodePackages.bower
    nodePackages.webpack
    sass
    zlib
  ];
  shellHook = ''
    LD_LIBRARY_PATH=${zlib}/lib:$LD_LIBRARY_PATH
    PATH=node_modules/.bin:$PATH
    pulp --version > /dev/null || npm install pulp
  '';
}
