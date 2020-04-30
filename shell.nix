{ nixpkgs ? import ./pkgs-from-json.nix { json = ./nixos-20-03.json; } }:
let
  overlay = self: super: {
    myHaskellPackages = 
      super.haskell.packages.ghc865.override (old: {
        overrides = self.lib.composeExtensions (old.overrides or (_: _: {})) 
          (hself: hsuper: {
            #ghc = hsuper.ghc // { withPackages = hsuper.ghc.withHoogle; };
            #ghcWithPackages = hself.ghc.withPackages;
          });
      });
  };

  pkgs = import nixpkgs {
    overlays = [overlay];
  };

  drv = pkgs.myHaskellPackages.callCabal2nix "cis194" ./cis194.cabal {};

  ghcide = (import (builtins.fetchTarball "https://github.com/cachix/ghcide-nix/tarball/master") {}).ghcide-ghc865;

  drvWithTools = drv.env.overrideAttrs (
    old: with pkgs.myHaskellPackages; {
      nativeBuildInputs = old.nativeBuildInputs ++ [
        ghcid
        # Add other development tools like ormolu here
        pkgs.haskellPackages.cabal-install
        ghcide
        # optional
        pkgs.haskellPackages.hlint
        pkgs.haskellPackages.hsimport
      ];
      }
  );
in
  drvWithTools
