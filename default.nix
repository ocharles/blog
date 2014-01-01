{}:
with import <nixpkgs> {};
stdenv.mkDerivation rec {
  name = "ocharles.org.uk-blog-1";
  src = ./.;
  buildInputs = [
    haskellPackages.ghc
    haskellPackages.hakyll
  ];
  buildPhase = ''
    export LOCALE_ARCHIVE=${glibcLocales}/lib/locale/locale-archive
    export LANG=en_US.UTF-8
    ghc -O2 --make hakyll.hs -o hakyll
    ./hakyll build
  '';
  installPhase = ''
    mkdir -p $out
    cp -R _site/* $out/
  '';
}
