{ mkDerivation, aeson, ansi-wl-pprint, authenticate-oauth, base
, bytestring, composition, containers, data-default, directory
, extra, hspec, http-client, http-client-tls, http-types, lens
, megaparsec, optparse-applicative, split, stdenv, text
}:
mkDerivation {
  pname = "tweet-hs";
  version = "0.6.1.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson ansi-wl-pprint authenticate-oauth base bytestring composition
    containers data-default directory extra http-client http-client-tls
    http-types lens megaparsec optparse-applicative split text
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base bytestring hspec ];
  homepage = "https://github.com/vmchale/command-line-tweeter#readme";
  description = "Command-line tool for twitter";
  license = stdenv.lib.licenses.bsd3;
}
