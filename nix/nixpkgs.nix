let
  nixpkgsJson = builtins.fromJSON (builtins.readFile ./nixpkgs.json);
  nixpkgsFunc = import (builtins.fetchTarball (with nixpkgsJson; {
    url = "${url}/archive/${rev}.tar.gz";
    inherit sha256;
  }));
in
  nixpkgsFunc {}
