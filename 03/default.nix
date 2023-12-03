{ mkDerivation, base, lens, lib, split, tasty, tasty-hunit }:
mkDerivation {
  pname = "x03";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base lens split ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base lens tasty tasty-hunit ];
  license = "unknown";
}
