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
      witherable = super.callHackage "witherable" "0.3.1" {};

      patch = unmarkBroken super.patch;
      reflex = unmarkBroken super.reflex;
    };
  };

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
in
  variant (haskellPackages.callPackage ./reflex-basic-host.nix {})
