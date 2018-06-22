{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc822" }:

let

  inherit (nixpkgs) pkgs stdenv;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

#  "servant-auth-server" = haskellPackages.callPackage
#    ({ mkDerivation, aeson, base, base64-bytestring, blaze-builder
#     , bytestring, bytestring-conversion, case-insensitive, cookie
#     , crypto-api, data-default-class, entropy, hspec, hspec-discover
#     , http-api-data, http-client, http-types, jose, lens, lens-aeson
#     , markdown-unlit, monad-time, mtl, QuickCheck, servant-auth
#     , servant-server, tagged, text, time, transformers
#     , unordered-containers, wai, warp, wreq
#     }:
#     mkDerivation {
#       pname = "servant-auth-server";
#       version = "0.3.2.0";
##       src = fetchgit {
##         url = "https://github.com/haskell-servant/servant-auth.git";
##         rev = "d5baba488c11606884f0bd2216b976be1f7b8432";
##
##       };
#       sha256 = "1ykjjd1lqivavsxdg8akj9vfh4miblslcwdyhlrgz2ci2f31wify";
#       isLibrary = true;
#       isExecutable = true;
#       libraryHaskellDepends = [
#         aeson base base64-bytestring blaze-builder bytestring
#         bytestring-conversion case-insensitive cookie crypto-api
#         data-default-class entropy http-api-data http-types jose lens
#         monad-time mtl servant-auth servant-server tagged text time
#         unordered-containers wai
#       ];
#       executableHaskellDepends = [
#         aeson base markdown-unlit mtl servant-auth servant-server
#         transformers warp
#       ];
#       testHaskellDepends = [
#         aeson base bytestring case-insensitive hspec http-client http-types
#         jose lens lens-aeson mtl QuickCheck servant-server time wai warp
#         wreq
#       ];
#       testToolDepends = [ hspec-discover ];
#       homepage = "http://github.com/haskell-servant/servant-auth#readme";
#       description = "servant-server/servant-auth compatibility";
#       license = stdenv.lib.licenses.bsd3;
#     }) {};

  f = { mkDerivation, aeson, base, bcrypt, bytestring, hedgehog
      , monad-control, mtl, opaleye, optparse-applicative
      , postgresql-simple, product-profunctors, profunctors
      , resource-pool, servant, servant-auth, servant-auth-server
      , servant-purescript, servant-server, stdenv, tasty, tasty-hedgehog
      , tasty-hunit, text, time, transformers-base, warp, yaml
      }:
      mkDerivation {
        pname = "mammut-control";
        version = "0.9.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson base bcrypt bytestring monad-control mtl opaleye
          optparse-applicative postgresql-simple product-profunctors
          profunctors resource-pool servant servant-auth servant-auth-server
          servant-server text time transformers-base yaml
        ];
        executableHaskellDepends = [
          base optparse-applicative servant-purescript warp
        ];
        testHaskellDepends = [
          base hedgehog tasty tasty-hedgehog tasty-hunit
        ];
        homepage = "https://www.mammutdata.com";
        description = "Mammut control API and web site";
        license = stdenv.lib.licenses.unfree;
      };

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
