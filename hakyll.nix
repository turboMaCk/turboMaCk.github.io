{ mkDerivation, base, binary, blaze-html, blaze-markup, bytestring
, containers, cryptohash, data-default, deepseq, directory
, fetchgit, file-embed, filepath, fsnotify, http-conduit
, http-types, lrucache, mtl, network-uri, optparse-applicative
, pandoc, pandoc-citeproc, parsec, process, QuickCheck, random
, regex-tdfa, resourcet, scientific, stdenv, tagsoup, tasty
, tasty-hunit, tasty-quickcheck, template-haskell, text, time
, time-locale-compat, unordered-containers, utillinux, vector, wai
, wai-app-static, warp, yaml
}:
mkDerivation {
  pname = "hakyll";
  version = "4.13.0.1";
  src = fetchgit {
    url = "https://github.com/jaspervdj/hakyll";
    sha256 = "0ldmz9njk74lsh21mdgdr4pd64axpqpby8v4ay9ij26mp7nwmpi4";
    rev = "7b924e7d6b98db7de64fa8fc5cae14a3ea35965c";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base binary blaze-html blaze-markup bytestring containers
    cryptohash data-default deepseq directory file-embed filepath
    fsnotify http-conduit http-types lrucache mtl network-uri
    optparse-applicative pandoc pandoc-citeproc parsec process random
    regex-tdfa resourcet scientific tagsoup template-haskell text time
    time-locale-compat unordered-containers vector wai wai-app-static
    warp yaml
  ];
  executableHaskellDepends = [ base directory filepath ];
  testHaskellDepends = [
    base bytestring containers filepath QuickCheck tasty tasty-hunit
    tasty-quickcheck text unordered-containers yaml
  ];
  testToolDepends = [ utillinux ];
  homepage = "http://jaspervdj.be/hakyll";
  description = "A static website compiler library";
  license = stdenv.lib.licenses.bsd3;

  # Customizations
  patches = [./hakyll.patch];
  configureFlags = "-f watchServer -f previewServer";
}
