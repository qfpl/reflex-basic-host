{ nixpkgs ? import ./nix/nixpkgs.nix
, compiler ? "default"
, doBenchmark ? false
}:
let
  inherit (nixpkgs) pkgs;
  env = (import ./. { inherit nixpkgs compiler doBenchmark; }).env;
in
  env.overrideAttrs (oldAttrs: {
    buildInputs = with pkgs.haskellPackages; oldAttrs.buildInputs ++ [
      cabal-install cabal2nix ghcid
    ];
  })
