{ mkDerivation, base, containers, ghc-prim, hedgehog, inline-c
, openblas, openblasCompat, parsers, primitive, stdenv
, template-haskell, transformers, vector
}:
mkDerivation {
  pname = "linear-algebra";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers ghc-prim inline-c parsers primitive
    template-haskell transformers vector
  ];
  librarySystemDepends = [ openblasCompat ];
  testHaskellDepends = [
    base containers ghc-prim hedgehog inline-c openblas parsers
    primitive template-haskell transformers vector
  ];
  homepage = "https://github.com/ttuegel/linear-algebra#readme";
  license = stdenv.lib.licenses.unfree;
}
