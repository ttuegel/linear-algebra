{ mkDerivation, base, containers, hedgehog, inline-c, openblas
, openblasCompat, parsers, primitive, stdenv, tagged
, template-haskell, transformers, vector
}:
mkDerivation {
  pname = "linear-algebra";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers inline-c parsers primitive tagged template-haskell
    transformers vector
  ];
  librarySystemDepends = [ openblasCompat ];
  testHaskellDepends = [
    base containers hedgehog inline-c openblas parsers primitive tagged
    template-haskell transformers vector
  ];
  homepage = "https://github.com/ttuegel/linear-algebra#readme";
  license = stdenv.lib.licenses.unfree;
}
