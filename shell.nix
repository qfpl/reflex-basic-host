{ nixpkgs ? import <nixpkgs> {} } : 
let
  inherit (nixpkgs) pkgs;
  reflex-platform = import ./nix/reflex-platform.nix;
  drv = import ./. { inherit reflex-platform; };
in
  if pkgs.lib.inNixShell then drv.env else drv
