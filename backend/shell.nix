with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "mammut-control";
  buildInputs = [
    haskell.compiler.ghc841
    haskellPackages.ghcid
    haskellPackages.hpack
    haskellPackages.morph
    postgresql
    zlib
  ];
  shellHook = ''
    LD_LIBRARY_PATH=${zlib}/lib:$LD_LIBRARY_PATH
  '';
}
