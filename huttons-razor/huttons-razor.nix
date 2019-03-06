{ mkDerivation, base, parsers, stdenv }:
mkDerivation {
  pname = "huttons-razor";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base parsers ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/ajmcmiddlin/huttons-razor";
  description = "A basic language to learn on";
  license = stdenv.lib.licenses.bsd3;
}
