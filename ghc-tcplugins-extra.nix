{ mkDerivation, base, ghc, stdenv }:
mkDerivation {
  pname = "ghc-tcplugins-extra";
  version = "0.2";
  src = ./ghc-tcplugins-extra;
  libraryHaskellDepends = [ base ghc ];
  homepage = "http://github.com/clash-lang/ghc-tcplugins-extra";
  description = "Utilities for writing GHC type-checker plugins";
  license = stdenv.lib.licenses.bsd2;
}
