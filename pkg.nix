{ mkDerivation, base, blaze-html, filepath, hakyll, stdenv }:
mkDerivation {
  pname = "acthpa-website2";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base blaze-html filepath hakyll ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
