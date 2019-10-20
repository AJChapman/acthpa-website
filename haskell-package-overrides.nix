{ haskellLib
, haskellPackages }:
haskellPackages.override {
  overrides = self: super: {
    # Put any overrides here, e.g:
    #
    # Jailbreak a certain package:
    #   pandoc-lens = haskellLib.doJailbreak super.pandoc-lens;
    #
    # Use a local copy of another package:
    #   pandoc-wrapper = super.callPackage ../pandoc-wrapper/pkg.nix { };
    aeson-generic-shorthand = super.callPackage /home/cha748/src/aeson-generic-shorthand/pkg.nix { };
    slick = super.callPackage /home/cha748/ext/slick/pkg.nix { };
  };
}
