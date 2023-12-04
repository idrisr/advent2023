{ mkDerivation, attoparsec, base, bytestring, lens, lib, tasty
, tasty-hunit
}:
mkDerivation {
  pname = "x04";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ attoparsec base bytestring lens ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    attoparsec base bytestring lens tasty tasty-hunit
  ];
  license = "unknown";
}
