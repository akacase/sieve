{ mkDerivation, aeson, base, bytestring, hostname, lib, network
, network-bsd, network-info, network-simple, text, triplesec
}:
mkDerivation {
  pname = "sieve";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring hostname network network-bsd network-info
    network-simple text triplesec
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/githubuser/sieve#readme";
  license = lib.licenses.bsd3;
}
