{ mkDerivation, aeson, base, bytestring, envy, hostname, lens, lib
, network, network-bsd, network-info, network-simple, split, text
, triplesec
}:
mkDerivation {
  pname = "sieve";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring envy hostname lens network network-bsd
    network-info network-simple split text triplesec
  ];
  executableHaskellDepends = [ base envy ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/githubuser/sieve#readme";
  license = lib.licenses.bsd3;
}
