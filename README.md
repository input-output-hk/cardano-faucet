# cardano-faucet

A simple faucet API server for cardano wallet.

## Running

* To run the faucet on a target machine, make sure you have [cardano-wallet](https://github.com/input-output-hk/cardano-wallet/) and [cardano-node](https://github.com/input-output-hk/cardano-node) configured appropriately and running, preferably through the nixos cardano-faucet-service [modules](https://github.com/input-output-hk/cardano-faucet/tree/master/nix/nixos) included in this repo.
* After that, you can run the cardano-faucet server, adding deployment and nginx proxy helpers with a cardano-faucet role, if desired, such as:
  * [cardano-ops faucet role](https://github.com/input-output-hk/cardano-ops/tree/master/roles/faucet.nix)

## Usage

Now we can test sending some funds to the new account from our faucet:

```shell-session
$ curl -XPOST "$FAUCET:$PORT/send-money/$ADDR"
{"success":true,"amount":1000000000,"fee":171334,"txid":"bbe3514818c490b661546f83e1a2ac4ec51180ca9d1f4731642923f447b445b7"}
```

Optionally, an api key can be provided to bypass the rate limiter:

```shell-session
$ curl -XPOST "$FAUCET:$PORT/send-money/$ADDR?apiKey=$APIKEY"
```

* Check the network's explorer for the transaction to post, or,
* Use the cardano-wallet cli to query your wallet

```shell-session
$ cardano-wallet wallet list
```

## Troubleshooting

* If faucet does not start properly and the logs do not clearly indicate why, ensure that on the faucet server:
  * cardano-node is started, syncronized and is working as expected
  * cardano-wallet is started, syncronized and is working as expected

* Continue troubleshooting, if needed, by verifying:
  * file `faucet.id` exists at the nixos option `faucetBasePath` and contains a wallet id
  * wallet contains a wallet with the id found in `faucet.id` Example:

```shell-session
cardano-wallet wallet list | grep $(cat faucet.id)
```

* Continue troubleshooting, if needed, by verifying you are not trying to:
  * create a new faucet wallet or faucet wallet passphrase or apikey file when those corresponding state files still exist at `faucetBasePath`
    * if you are trying to do this, delete the respective old state files manually and restart the cardano-faucet service
    * restarting the cardano-faucet service will populate new state files to `faucetBasePath` as needed from the respective nixos options (ex: faucetApiKeyPath) and take appropriate re-config action

* If the troubleshooting steps above don't help and a wallet you are expecting to be created isn't getting created:
  * check the expect script in the cardano-faucet service preStart command
  * check the expect script's output file `create-output.log` in `faucetBasePath`
  * delete the `faucet.id` file in `faucetBasePath` and restart the cardano-faucet service to attempt to re-create the faucet wallet by expect script

## TODO

* Faucet wallets are currently created with expect scripts via the cardano-wallet cli
  * The expect script error handling is fragile; improve this by implementing wallet creation by API if available
