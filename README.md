# cardano-faucet

A simple faucet API server for cardano wallet.

## Running

* To run locally, make sure you have [cardano-wallet](https://github.com/input-output-hk/cardano-wallet/) and [cardano-node](https://github.com/input-output-hk/cardano-node) and configured appropriately.
* After that, you can run the server
* The easiest way to do this is with the nix modules system and a nix cardano-faucet role, such as:
  * [cardano-ops faucet role](https://https://github.com/input-output-hk/cardano-ops/tree/master/roles/faucet.nix)

## Usage

Now we can test sending some funds to the new account from our faucet:

```shell-session
$ curl -XPOST localhost:8091/send-money/37btjrVyb4KAo443bvsk2FkEanKrLShXY9MAy7BeuW8iKnbKRZWvgJo1dRP5WmJbpDwqVyxLjuyxLfUuwFRWb9HdW2dH3PUyznWPPDS2buK4g7tov4
{"success":true,"amount":1000000000,"fee":171334,"txid":"bbe3514818c490b661546f83e1a2ac4ec51180ca9d1f4731642923f447b445b7"}
```

* Check the network's explorer for the transaction to post, or,
* Use the cardano-byron-wallet cli to query your wallet

```shell-session
$ cardano-wallet-byron wallet list
```
