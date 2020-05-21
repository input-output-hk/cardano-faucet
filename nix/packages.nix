{ callPackage
, sqlite
, crystal
, cardano-wallet-byron
, jq
, expect
, lib
, openssl
, pkg-config
}:

let
  inherit (lib) cleanSourceWith hasSuffix removePrefix;
  filter = name: type: let
    baseName = baseNameOf (toString name);
    sansPrefix = removePrefix (toString ../.) name;
  in (
    baseName == "src" ||
    hasSuffix ".cr" baseName ||
    hasSuffix ".yml" baseName ||
    hasSuffix ".lock" baseName ||
    hasSuffix ".nix" baseName
  );
in {
  cardano-faucet = crystal.buildCrystalPackage {
    pname = "cardano-faucet";
    version = "0.1.0";
    src = cleanSourceWith {
      inherit filter;
      src = ../.;
      name = "cardano-faucet";
    };
    format = "shards";
    crystalBinaries.server.src = "src/cardano-faucet.cr";
    shardsFile = ../shards.nix;
    buildInputs = [ pkg-config openssl sqlite ];
    doCheck = false;
    doInstallCheck = false;
  };

  cardano-faucet-tests = callPackage ./nixos/tests {};
  create-faucet-wallet = callPackage ./nixos/create-faucet-wallet {};
}
