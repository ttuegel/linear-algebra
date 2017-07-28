{ mkDerivation, base, containers, inline-c, openblasCompat, parsers
, primitive, stdenv, template-haskell, vector
}:
mkDerivation {
  pname = "openblas";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers inline-c parsers primitive template-haskell vector
  ];
  librarySystemDepends = [ openblasCompat ];
  homepage = "https://github.com/ttuegel/openblas#readme";
  license = stdenv.lib.licenses.unfree;
}
