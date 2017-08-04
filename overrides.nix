pkgs: lib: self: super:

{
  concurrent-output = lib.doJailbreak super.concurrent-output;
  parsers = lib.dontCheck super.parsers;
}
