{ mkDerivation, base, hedgehog, parsec, parsers, stdenv, tasty
, tasty-hedgehog, text
}:
mkDerivation {
  pname = "huttons-razor";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base parsec parsers text ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hedgehog tasty tasty-hedgehog ];
  homepage = "https://github.com/ajmcmiddlin/huttons-razor";
  description = "A basic language to learn on";
  license = stdenv.lib.licenses.bsd3;
}
