{ reflex-platform ? import ./reflex-platform.nix } :
let
  reflex-basic-host = reflex-platform.ghc.callPackage ./reflex-basic-host.nix {};
in
  reflex-basic-host
