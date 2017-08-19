{ mkDerivation, base, dependent-sum, mtl, primitive, ref-tf, reflex
, stdenv
}:
mkDerivation {
  pname = "reflex-basic-host";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base dependent-sum mtl primitive ref-tf reflex
  ];
  license = stdenv.lib.licenses.bsd3;
}
