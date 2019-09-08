{ mkDerivation, base, blaze-html, filepath, hakyll, lens, stdenv
, taggy-lens, text
}:
mkDerivation {
  pname = "acthpa-website2";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base blaze-html filepath hakyll lens taggy-lens text
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
