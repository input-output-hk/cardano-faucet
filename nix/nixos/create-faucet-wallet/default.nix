{ runCommand, cardano-wallet, expect, jq }:

runCommand "create-faucet-wallet" {
  expect = "${expect}/bin/expect";
  jq = "${jq}/bin/jq";
  cardanoWallet = "${cardano-wallet}/bin/cardano-wallet";
} ''
  substituteAll ${./create-faucet-wallet.sh} $out
  chmod +x $out
  patchShebangs $out
''
