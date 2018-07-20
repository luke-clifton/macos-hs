{ mkDerivation, base, bytestring, containers, inline-c, stdenv, Security }:
mkDerivation {
  pname = "macos";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base bytestring containers inline-c Security ];
  executableHaskellDepends = [ base bytestring Security ];
  description = "Functions for integrating with macOS";
  license = stdenv.lib.licenses.bsd3;
}
