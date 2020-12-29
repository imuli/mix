{ pkgs ? import <nixpkgs> {}, hpkgs ? pkgs.haskellPackages }:
let mkDerivation = expr: hpkgs.mkDerivation (expr // { enableSeparateDocOutput = true; doHaddock = true; });
    self = import ./default.nix { inherit pkgs hpkgs mkDerivation; };
 in hpkgs.shellFor {
      packages = p: [ self ];
      withHoogle = true;
    }
