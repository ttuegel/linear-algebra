{ mkDerivation, base, containers, equational-reasoning, ghc-prim
, ghc-typelits-presburger, hedgehog, inline-c, openblasCompat
, parsers, primitive, stdenv, template-haskell, transformers
, transformers-base, vector
}:
mkDerivation {
  pname = "linear-algebra";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers equational-reasoning ghc-prim
    ghc-typelits-presburger inline-c parsers primitive template-haskell
    transformers transformers-base vector
  ];
  librarySystemDepends = [ openblasCompat ];
  testHaskellDepends = [
    base containers equational-reasoning ghc-prim
    ghc-typelits-presburger hedgehog inline-c parsers primitive
    template-haskell transformers transformers-base vector
  ];
  homepage = "https://github.com/ttuegel/linear-algebra#readme";
  license = stdenv.lib.licenses.unfree;
}
