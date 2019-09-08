{ mkDerivation, base, blaze-html, filepath, hakyll, lens, pandoc
, stdenv, taggy-lens, text
}:
mkDerivation {
  pname = "acthpa-website";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base blaze-html filepath hakyll lens pandoc taggy-lens text
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
