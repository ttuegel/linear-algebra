pkgs: lib: self: super:

{
  parsers = lib.dontCheck super.parsers;
}
