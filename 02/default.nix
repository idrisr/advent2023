{ mkDerivation, attoparsec, base, bytestring, lib
, microlens-platform, tasty, tasty-hunit
}:
mkDerivation {
  pname = "x02";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bytestring microlens-platform
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    attoparsec base bytestring microlens-platform tasty tasty-hunit
  ];
  license = "unknown";
}
