{ mkDerivation, attoparsec, base, bytestring, lens, lib, split }:
mkDerivation {
  pname = "x05";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ attoparsec base bytestring lens split ];
  executableHaskellDepends = [ base ];
  license = "unknown";
}
