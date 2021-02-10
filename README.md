# cardano-faucet

A simple faucet API server for cardano wallet.

## Running

* To run the faucet on a target machine, make sure you have [cardano-wallet](https://github.com/input-output-hk/cardano-wallet/) and [cardano-node](https://github.com/input-output-hk/cardano-node) configured appropriately and running, preferably through the nixos cardano-faucet-service [modules](https://github.com/input-output-hk/cardano-faucet/tree/master/nix/nixos) included in this repo.
* After that, you can run the cardano-faucet server, adding deployment and nginx proxy helpers with a cardano-faucet role, if desired, such as:
  * [cardano-ops faucet role](https://github.com/input-output-hk/cardano-ops/tree/master/roles/faucet.nix)

## Usage

Now we can test sending some funds to an address from the faucet:

* To request funds using defaults when anonymous access is allowed.
* `minLovelace` in the output below shows the minimum utxo size required by the network.
```shell-session
$ curl -XPOST "http[s]://$FQDN:$PORT/send-money/$ADDRESS"
{
  "success": true,
  "amount": 5000000,
  "unit": "lovelace",
  "fee": 168801,
  "minLovelace": 1000000,
  "txid": "f9aacf010b66ae820c7082e18e71360f1cdb4de316ddea887a6a083e447a2f8f"
}
```

* To request funds with an API key, an API key can be provided.
* Each API key has specific transaction quantity and rate limit specified.
```shell-session
$ curl -XPOST "http[s]://FQDN/send-money/$ADDRESS?apiKey=$KEY"
{
  "success": true,
  "amount": 10000000,
  "unit": "lovelace",
  "fee": 168801,
  "minLovelace": 1000000,
  "txid": "26c06e16d54c11e8fd10f3b1e22d0c35acf43bc52c0558c01ef3507e67b8a804"
}
```

* To request non-ADA funds, an asset can be provided.
* `minLovelace` in the output below is the minimum lovelace output to the target address required to support the asset transaction by the network.
```shell-session
$ curl -XPOST "http[s]://FQDN/send-money/$ADDRESS?asset=$UNIT_TYPE"
{
  "success": true,
  "amount": 2,
  "unit": "6b8d07d69639e9413dd637a1a815a7323c69c86abbafb66dbfdb1aa7:",
  "fee": 172585,
  "minLovelace":1407406,
  "txid":"5f97ce363e0b9f898a29fc83244279f3d971fd9acae2514d0f357305691449bf"
}
```

* If both a valid API key and an asset parameter are provided, the API key will take precedence.

* Once a request has been made:
  * Check the network's explorer for the transaction, or,
  * Use the cardano-wallet cli to query your wallet
```shell-session
$ cardano-wallet wallet list
```

## Troubleshooting

* If faucet does not start properly and the logs do not clearly indicate why, ensure that on the faucet server:
  * cardano-node is started, syncronized and is working as expected
  * cardano-wallet is started, syncronized and is working as expected

* Continue troubleshooting, if needed, by verifying that:
  * File `faucet.id` exists at the nixos option `faucetBasePath` and contains a wallet id
  * Cardano wallet contains a wallet with the id found in `faucet.id` Example:
```shell-session
cardano-wallet wallet list | grep $(cat faucet.id)
```

* Continue troubleshooting, if needed, by verifying you are not trying to:
  * Create a new faucet wallet or faucet wallet passphrase or apikey file when those corresponding state files still exist at `faucetBasePath`
    * If you are trying to do this, delete the respective old state files manually and restart the cardano-faucet service
    * Restarting the cardano-faucet service will populate new state files to `faucetBasePath` as needed from the respective nixos options (ex: faucetApiKeyPath) and take appropriate re-config action

* If the troubleshooting steps above don't help and a wallet you are expecting to be created isn't getting created:
  * Check the expect script in the cardano-faucet service preStart command
  * Check the expect script's output file `create-output.log` in `faucetBasePath`
  * Delete the `faucet.id` file in `faucetBasePath` and restart the cardano-faucet service to attempt to re-create the faucet wallet by expect script

## TODO

* Faucet wallets are currently created with expect scripts via the cardano-wallet cli
  * The expect script error handling is fragile; improve this by implementing wallet creation by API if available
