{ callPackage
, gitignoreSource
, sqlite
, crystal
, cardano-wallet-byron
, jq
, expect }:

{
  cardano-faucet-cr = crystal.buildCrystalPackage {
    pname = "cardano-faucet";
    version = "0.1.0";
    src = gitignoreSource ../.;
    crystalBinaries.server.src = "src/cardano-faucet.cr";
    shardsFile = ../shards.nix;
    buildInputs = [ sqlite ];
  };

  cardano-faucet-tests = callPackage ./nixos/tests {};
  create-faucet-wallet = callPackage ./nixos/create-faucet-wallet {};
}
