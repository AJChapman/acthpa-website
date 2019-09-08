{ pkgs ? import ./pinned-nixpkgs.nix
, compiler ? "default"
, doBenchmark ? false
, doTest ? true
}:

let
  inherit pkgs;

  drv = import ./default.nix { inherit pkgs compiler doBenchmark doTest; };
  drvWithTools = pkgs.haskell.lib.addBuildDepends drv (with pkgs; [
    cabal-install haskellPackages.ghcid
  ]);
in
  if pkgs.lib.inNixShell then drvWithTools.env else drv
