{ mkDerivation, base, containers, dependent-sum, mtl, primitive
, ref-tf, reflex, stdenv, stm, these, time
}:
mkDerivation {
  pname = "reflex-basic-host";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base dependent-sum mtl primitive ref-tf reflex stm these
  ];
  executableHaskellDepends = [ base containers mtl reflex time ];
  license = stdenv.lib.licenses.bsd3;
}
