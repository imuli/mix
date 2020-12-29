{ pkgs ? import <nixpkgs> {}
, hpkgs ? pkgs.haskellPackages
, mkDerivation ? hpkgs.mkDerivation
, withMix ? false
}: if withMix then (import ./mixlib.nix { nixpkgs = pkgs; mixnix = import ./mix.nix; }).mix
              else hpkgs.callCabal2nix "mix" (pkgs.lib.cleanSource ./.) { inherit mkDerivation; }
