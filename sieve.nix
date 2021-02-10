{ mkDerivation, base, bytestring, hostname, lib, network
, network-bsd, network-info, network-simple
}:
mkDerivation {
  pname = "sieve";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring hostname network network-bsd network-info
    network-simple
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/githubuser/sieve#readme";
  license = lib.licenses.bsd3;
}
