{ reflex-platform ? import ./nix/reflex-platform.nix } :
let

  haskellPackages = reflex-platform.ghc.override {
    overrides = self: super: {
      ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
      ghcWithPackages = self.ghc.withPackages;
    };
  };

  reflex-basic-host = haskellPackages.callPackage ./reflex-basic-host.nix {};
in
  reflex-basic-host
