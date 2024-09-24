{ mkDerivation, base, directory, hakyll, hakyll-sass, pandoc
, stdenv
}:
mkDerivation {
  pname = "blog";
  version = "0.1.0.0";
  src = ../src;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base directory hakyll hakyll-sass pandoc
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
