{ mkDerivation, base, dependent-map, dependent-sum, lens, mtl
, primitive, ref-tf, reflex, stdenv, stm
}:
mkDerivation {
  pname = "reflex-basic-host";
  version = "0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base dependent-map dependent-sum lens mtl primitive ref-tf reflex
    stm
  ];
  executableHaskellDepends = [ base mtl reflex ];
  description = "A basic `reflex` host for backend work";
  license = stdenv.lib.licenses.bsd3;
}
