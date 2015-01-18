{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "ocharles-blog";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ base ];
  homepage = "http://ocharles.org.uk/blog";
  description = "My blog";
  license = stdenv.lib.licenses.bsd3;
}
