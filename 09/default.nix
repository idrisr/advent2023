{ mkDerivation, attoparsec, base, bytestring, containers, lens, lib
, mtl, optparse-applicative
}:
mkDerivation {
  pname = "x08";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bytestring containers lens mtl optparse-applicative
  ];
  executableHaskellDepends = [
    attoparsec base bytestring containers lens mtl optparse-applicative
  ];
  testHaskellDepends = [ base ];
  license = "unknown";
  mainProgram = "day8";
}
