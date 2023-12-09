{ mkDerivation, base, lib }:
mkDerivation {
  pname = "x09";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base ];
  license = "unknown";
  mainProgram = "day9";
}
