{ runCommand, cardano-wallet-byron, cardano-wallet-shelley, expect, jq }:

runCommand "create-faucet-wallet" {
  expect = "${expect}/bin/expect";
  jq = "${jq}/bin/jq";
  cardanoWalletByron = "${cardano-wallet-byron}/bin/cardano-wallet-byron";
  cardanoWalletShelley = "${cardano-wallet-shelley}/bin/cardano-wallet-shelley";
} ''
  substituteAll ${./create-faucet-wallet.sh} $out
  chmod +x $out
  patchShebangs $out
''
