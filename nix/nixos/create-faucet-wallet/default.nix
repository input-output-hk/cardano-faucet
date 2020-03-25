{ runCommand, cardano-wallet-byron, expect, jq }:

runCommand "create-faucet-wallet" {
  expect = "${expect}/bin/expect";
  jq = "${jq}/bin/jq";
  cardanoWalletByron = "${cardano-wallet-byron}/bin/cardano-wallet-byron";
} ''
  substituteAll ${./create-faucet-wallet.sh} $out
  chmod +x $out
  patchShebangs $out
''
