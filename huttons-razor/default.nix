{ nixpkgs ? import ./nix/nixpkgs.nix
, compiler ? "default"
, doBench ? false
}:
let
  inherit (nixpkgs) pkgs;

  haskellPackages = pkgs.haskellPackages;

  huttons-razor = haskellPackages.callPackage ./huttons-razor.nix {};

  withBench = d: if doBench
    then pkgs.haskell.lib.doBenchmark d
    else d;

  drv = withBench (huttons-razor);
in
  drv
