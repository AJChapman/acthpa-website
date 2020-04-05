{ mkDerivation, aeson, aeson-generic-shorthand, base, binary
, binary-instances, blaze-html, blaze-markup, bytestring
, containers, coordinate, directory, either, exceptions, filepath
, formatting, lens, lens-aeson, modern-uri, mtl, mustache, pandoc
, pretty-simple, req, shake, slick, stdenv, taggy-lens, tagsoup
, tagsoup-navigate, text, time, unordered-containers, validation
}:
mkDerivation {
  pname = "acthpa-website";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson aeson-generic-shorthand base binary binary-instances
    blaze-html blaze-markup bytestring containers coordinate directory
    either exceptions filepath formatting lens lens-aeson modern-uri
    mtl mustache pandoc pretty-simple req shake slick taggy-lens
    tagsoup tagsoup-navigate text time unordered-containers validation
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
