{ pkgs ? import ./pinned-nixpkgs.nix
, compiler ? "default"
, doBenchmark ? false
, doTest ? false
}:

let
  inherit pkgs;

  # Import the cabal2nix packages.
  # This should be updated every time the .cabal file changes.
  # To update:
  #   cabal2nix . > pkg.nix
  pkg = import ./pkg.nix;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  modifiedHaskellPackages = import ./haskell-package-overrides.nix { haskellLib = pkgs.haskell.lib; inherit haskellPackages; };

  bench = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
  check = if doTest then pkgs.lib.id else pkgs.haskell.lib.dontCheck;
in
  bench (check (modifiedHaskellPackages.callPackage pkg {}))
