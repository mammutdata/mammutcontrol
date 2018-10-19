with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "mammut-control";
  buildInputs = [
    haskell.compiler.ghc843
    haskell.packages.ghc843.cabal-install
    haskell.packages.ghc843.ghcid
    haskell.packages.ghc843.hpack
    haskell.packages.ghc822.morph
    postgresql
    zlib
  ];
  shellHook = ''
    LD_LIBRARY_PATH=${zlib}/lib:$LD_LIBRARY_PATH
  '';
}
