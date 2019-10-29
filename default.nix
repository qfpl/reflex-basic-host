{ nixpkgs ? import ./nix/nixpkgs.nix
, compiler ? "default"
, doBenchmark ? false
}:

let
  inherit (nixpkgs) pkgs;

  baseHaskellPackages = if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  haskellPackages = baseHaskellPackages.override {
    overrides = self: super: with pkgs.haskell.lib; {
      # Required by reflex
      dependent-map = super.dependent-map_0_3;
      dependent-sum = super.dependent-sum_0_6_2_0;
      monoidal-containers = super.monoidal-containers_0_6;
      witherable = super.callHackage "witherable" "0.3.1" {};

      reflex = enableCabalFlag (unmarkBroken super.reflex) "split-these";
    };
  };

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
in
  variant (haskellPackages.callPackage ./reflex-basic-host.nix {})
