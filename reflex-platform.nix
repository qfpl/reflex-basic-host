let  
  initialNixpkgs = import <nixpkgs> {};

  sources = {
    reflex-platform = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-platform";
      rev = "5fbbe11edd3674585d7b569cc218971a87b2f084";
      sha256 = "0176jxwx7p8j3y6w1qwhnfdnr69d61acvm4bqbzhrc0h8fmbcnc7";
    };
  };

  reflex-platform = import sources.reflex-platform {};
in
  reflex-platform

