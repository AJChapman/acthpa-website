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

    # Get this from https://github.com/AJChapman/aeson-generic-shorthand
    aeson-generic-shorthand = super.callPackage ../aeson-generic-shorthand/pkg.nix { };
  };
}
