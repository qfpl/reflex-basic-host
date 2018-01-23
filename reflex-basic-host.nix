{ mkDerivation, async, base, dependent-sum, mtl, primitive, ref-tf
, reflex, stdenv, stm
}:
mkDerivation {
  pname = "reflex-basic-host";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async base dependent-sum mtl primitive ref-tf reflex stm
  ];
  executableHaskellDepends = [ base mtl reflex ];
  license = stdenv.lib.licenses.bsd3;
}
