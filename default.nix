{ mkDerivation, aeson, ansi-wl-pprint, authenticate-oauth, base
, bytestring, composition, containers, data-default, directory
, extra, hspec, hspec-megaparsec, http-client, http-client-tls
, http-types, lens, megaparsec, MissingH, optparse-applicative
, split, stdenv, text
}:
mkDerivation {
  pname = "tweet-hs";
  version = "0.5.3.8";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson ansi-wl-pprint authenticate-oauth base bytestring composition
    containers data-default directory extra http-client http-client-tls
    http-types lens megaparsec MissingH optparse-applicative split text
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base bytestring hspec hspec-megaparsec megaparsec
  ];
  homepage = "https://github.com/vmchale/command-line-tweeter#readme";
  description = "Command-line tool for twitter";
  license = stdenv.lib.licenses.bsd3;
}
