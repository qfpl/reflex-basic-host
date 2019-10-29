{ mkDerivation, base, dependent-sum, lens, mtl, primitive, ref-tf
, reflex, stdenv, stm, witherable
}:
mkDerivation {
  pname = "reflex-basic-host";
  version = "0.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base dependent-sum lens mtl primitive ref-tf reflex stm
  ];
  executableHaskellDepends = [ base lens reflex witherable ];
  homepage = "https://github.com/qfpl/reflex-basic-host/";
  description = "A basic Reflex host for backend work";
  license = stdenv.lib.licenses.bsd3;
}
