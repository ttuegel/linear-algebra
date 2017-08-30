pkgs: lib: self: super:

{
  equational-reasoning = self.callPackage ./equational-reasoning {};
  concurrent-output = lib.doJailbreak super.concurrent-output;
  ghc-tcplugins-extra = self.callPackage ./ghc-tcplugins-extra.nix {};
  ghc-typelits-presburger = self.callPackage ./ghc-typelits-presburger {};
  parsers = lib.dontCheck super.parsers;
  singletons = self.callPackage ./singletons.nix {};
  th-desugar = self.callPackage ./th-desugar.nix {};
}
