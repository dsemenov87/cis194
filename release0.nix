let
  pkgs = import <nixpkgs> { };

in
  pkgs.haskellPackages.callPackage ./cis194.nix { }
