{
  bootstrap ? import <nixpkgs> {}
, json
}:
let
  nixpkgsPin = bootstrap.pkgs.lib.importJSON json;

  pinnedPkgs = bootstrap.pkgs.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs-channels";
    inherit (nixpkgsPin) rev sha256;
  };
in
  pinnedPkgs
