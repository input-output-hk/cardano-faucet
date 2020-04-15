{ callPackage
, gitignoreSource
, sqlite
, crystal
, cardano-wallet-byron
, jq
, expect
, lib
}:

let
  filter = name: type: let
    baseName = baseNameOf (toString name);
    sansPrefix = lib.removePrefix (toString ../.) name;
  in (
    baseName == "src" ||
    lib.hasSuffix ".cr" baseName
  );
in {
  cardano-faucet-cr = crystal.buildCrystalPackage {
    pname = "cardano-faucet";
    version = "0.1.0";
    src = lib.cleanSourceWith {
      inherit filter;
      src = ../.;
      name = "cardano-faucet";
    };
    crystalBinaries.server.src = "src/cardano-faucet.cr";
    shardsFile = ../shards.nix;
    buildInputs = [ sqlite ];
    doCheck = false;
  };

  cardano-faucet-tests = callPackage ./nixos/tests {};
  create-faucet-wallet = callPackage ./nixos/create-faucet-wallet {};
}
