with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, hakyll, stdenv }:
             mkDerivation {
               pname = "ocharles-blog";
               version = "1.0.0";
               src = ./.;
               isLibrary = false;
               isExecutable = true;
               buildDepends = [ base hakyll ];
               homepage = "http://ocharles.org.uk/blog";
               description = "My blog";
               license = stdenv.lib.licenses.bsd3;
             }) {};
in
  pkg.env
