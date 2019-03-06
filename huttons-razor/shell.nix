{ nixpkgs ? import ./nix/nixpkgs.nix
, compiler ? "default"
, hie ? false
}:

let
  inherit (nixpkgs) pkgs;

  drv = (import ./. {});

  haskellPackages = pkgs.haskellPackages;

  shellDrv = pkgs.haskell.lib.overrideCabal drv (drv': {
    buildDepends =
      (drv'.buildDepends or []) ++
      [ (haskellPackages.hoogleLocal {
          packages =
            (drv'.libraryHaskellDepends or []) ++
            (drv'.executableHaskellDepends or []) ++
            (drv'.testHaskellDepends or []) ;
        })
        pkgs.cabal-install
        haskellPackages.ghcid
      ];
  });

in
  shellDrv.env
