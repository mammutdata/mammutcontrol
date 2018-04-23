with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "mammut-control";
  buildInputs = [
    haskell.compiler.ghc822
    postgresql
    zlib
  ];
  shellHook = ''
    LD_LIBRARY_PATH=${zlib}/lib:$LD_LIBRARY_PATH
  '';
}
