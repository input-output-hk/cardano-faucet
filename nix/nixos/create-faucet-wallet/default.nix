{ runCommand, cardano-wallet-byron-cli, expect, jq }:

runCommand "create-faucet-wallet" {
  expect = "${expect}/bin/expect";
  jq = "${jq}/bin/jq";
  cardanoWalletByronCli = "${cardano-wallet-byron-cli}/bin/cardano-wallet-byron";
} ''
  substituteAll ${./create-faucet-wallet.sh} $out
  chmod +x $out
  patchShebangs $out
''
