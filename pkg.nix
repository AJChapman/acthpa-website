{ mkDerivation, base, blaze-html, blaze-markup, bytestring
, containers, coordinate, either, exceptions, filepath, formatting
, hakyll, lens, modern-uri, pandoc, req, stdenv, taggy-lens, text
, time, unordered-containers
}:
mkDerivation {
  pname = "acthpa-website";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base blaze-html blaze-markup bytestring containers coordinate
    either exceptions filepath formatting hakyll lens modern-uri pandoc
    req taggy-lens text time unordered-containers
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
