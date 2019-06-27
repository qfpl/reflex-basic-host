{ mkDerivation, base, dependent-map, dependent-sum, lens, mtl
, primitive, ref-tf, reflex, stdenv, stm, witherable
}:
mkDerivation {
  pname = "reflex-basic-host";
  version = "0.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base dependent-map dependent-sum lens mtl primitive ref-tf reflex
    stm
  ];
  executableHaskellDepends = [
    base lens mtl ref-tf reflex witherable
  ];
  description = "A basic `reflex` host for backend work";
  license = stdenv.lib.licenses.bsd3;
}
