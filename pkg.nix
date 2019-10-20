{ mkDerivation, aeson, aeson-generic-shorthand, base, binary
, binary-instances, blaze-html, blaze-markup, bytestring
, containers, coordinate, either, exceptions, filepath, formatting
, lens, lens-aeson, modern-uri, mustache, pandoc, req, shake, slick
, stdenv, taggy-lens, text, time, unordered-containers
}:
mkDerivation {
  pname = "acthpa-website";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson aeson-generic-shorthand base binary binary-instances
    blaze-html blaze-markup bytestring containers coordinate either
    exceptions filepath formatting lens lens-aeson modern-uri mustache
    pandoc req shake slick taggy-lens text time unordered-containers
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
