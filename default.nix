{ mkDerivation, base, containers, inline-c, openblasCompat, parsers
, primitive, stdenv, template-haskell, transformers, vector
}:
mkDerivation {
  pname = "openblas";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers inline-c parsers primitive template-haskell
    transformers vector
  ];
  librarySystemDepends = [ openblasCompat ];
  homepage = "https://github.com/ttuegel/openblas#readme";
  license = stdenv.lib.licenses.unfree;
}
