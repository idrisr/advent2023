{ mkDerivation, attoparsec, base, bytestring, lib, tasty
, tasty-hunit
}:
mkDerivation {
  pname = "x01";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ attoparsec base bytestring ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ attoparsec base tasty tasty-hunit ];
  license = "unknown";
}
