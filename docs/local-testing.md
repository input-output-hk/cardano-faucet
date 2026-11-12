# Testing Locally

> Guide demonstrating how to run cardano-faucet backed by a local ephemeral testnet.

## Prerequisites

All required tools are included in the default nix devShell:

```
nix develop

# Alternatively, if using direnv
direnv allow
```

If you're not using Nix, you will need the following CLI tools:

 * [cardano-node](https://github.com/IntersectMBO/cardano-node/releases/latest)
 * [cardano-address](https://intersectmbo.github.io/cardano-addresses/intro/)
 * [cardano-playground](https://github.com/input-output-hk/cardano-playground) faucet setup scripts:
   * [distribute.py](https://github.com/input-output-hk/cardano-playground/blob/main/scripts/distribute.py)
   * [setup-delegation-accounts.py](https://github.com/input-output-hk/cardano-playground/blob/main/scripts/restore-delegation-accounts.py)
   * [restore-delegation-accounts.py](https://github.com/input-output-hk/cardano-playground/blob/main/scripts/restore-delegation-accounts.py)

You will also need `cardano-cli` and `cardano-testnet` applications included in the
`cardano-node` release distribution.

## Create a Local Testnet

Launch a local cluster:

```
rm -rf testnet
CARDANO_NODE="$(which cardano-node)" CARDANO_CLI="$(which cardano-cli)" \
    cardano-testnet cardano --params-mainnet
```

You should now have a local testnet cluster running in `testnet/`. In another terminal
session, define environment variables for the faucet and funding scripts:

```
export CONFIG_FILE=./testnet-faucet/faucet-config.json
export CARDANO_NODE_SOCKET_PATH=./testnet/socket/node1/sock
export CARDANO_NODE_NETWORK_ID=42
```

Query one of the nodes to verify it's running:

```
cardano-cli query tip --socket-path testnet/socket/node1/sock --testnet-magic 42
```

## Configure Faucet

Generate a wallet mnemonic and payment address:

```
mkdir -p testnet-faucet
cardano-address recovery-phrase generate > testnet-faucet/faucet.mnemonic
```

Get the wallet address

```
cardano-address key from-recovery-phrase Shelley < testnet-faucet/faucet.mnemonic \
    | cardano-address key child 1852H/1815H/0H/0/0 \
    | cardano-address key public --with-chain-code \
    | cardano-address address payment --network-tag testnet \
    > testnet-faucet/faucet.addr
```

Generate a Faucet API key:

```
head -c 128 /dev/urandom | tr -dc 'a-zA-Z0-9' | head -c 32 > testnet-faucet/api.key
```

Create the Faucet configuration file to `testnet-faucet/faucet-config.json`:

```
{
  "debug": false,
  "api_keys": [
    {
      "api_key": "$API_KEY",
      "lovelace": 10000200000,
      "rate_limit": 60,
      "delegate": true
    }
  ],
  "recaptcha_limits": {
    "default": {
      "api_key": "unused",
      "lovelace": 10000200000,
      "rate_limit": 86401,
      "delegate": true
    }
  },
  "max_stake_key_index": 50,
  "delegation_utxo_size": 10,
  "mnemonic": [
    "MNEMONIC_WORD_1",
    "MNEMONIC_WORD_2",
    "MNEMONIC_WORD_3",
    // ...
    "MNEMONIC_WORD_24"
  ],
  "network": {
    "Testnet": 42
  },
  "recaptcha_site_key": "6LeIxAcTAAAAAJcZVRqyHh71UMIEGNQ_MXjiZKhI",
  "recaptcha_secret_key": "6LeIxAcTAAAAAGG-zFI1TnRWxMZNFuojJ4WifJWe",
  "allowed_cors_origins": [
    "http://localhost:8090"
  ],
  "address_index": 0
}
```

Note: The reCAPTCHA keys above are Google's public
[test keys](https://developers.google.com/recaptcha/docs/faq)

Generate a rewards configuration:
```
 jq -nc --arg addr "$(<testnet-faucet/faucet.addr)" '[range(1000) | { ($addr): 10000200000 }]' \
    | jq '.' \
    > testnet-faucet/rewards.json
```

Start Faucet:

```
cabal run cardano-faucet:exe:cardano-faucet
```

Faucet is now running, but is empty. Stop it and proceed to [Fund Faucet](#fund-faucet)

## Fund Faucet

Generate the rewards transactions:

```
distribute \
  --testnet-magic 42 \
  --signing-key-file testnet/utxo-keys/utxo1/utxo.skey \
  --address "$(<testnet/utxo-keys/utxo1/utxo.addr)" \
  --payments-json testnet-faucet/rewards.json
```

Submit them:

```
for tx in *.txsigned; do 
    cardano-cli latest transaction submit \
        --testnet-magic 42 \
        --socket-path testnet/socket/node1/sock \
        --tx-file "$tx"
done
```

Generate the delegations transactions:

```
setup-delegation-accounts \
  --testnet-magic "42" \
  --signing-key-file testnet/utxo-keys/utxo1/utxo.skey \
  --wallet-mnemonic testnet-faucet/faucet.mnemonic \
  --delegation-amount 1000000000 \
  --num-accounts 51
```

Clean up the transaction files:

```
rm -f *.tx* pparams.json tmp_utxo.json
```

**NOTE**: `cardano-testnet` has no resume functionality, so you will start it from scratch
and fund Faucet every time you start a local testnet cluster!

## Start Faucet

Make sure environment variables are defined that point to the `cardano-node` and 
`cardano-cli` executables:

```
export CONFIG_FILE=./testnet-faucet/faucet-config.json
export CARDANO_NODE_SOCKET_PATH=./testnet/socket/node1/sock
export CARDANO_NODE_NETWORK_ID=42
```

Start Faucet:

```
cabal run cardano-faucet:exe:cardano-faucet

lovelace values for api keys [Ada (Coin 10000200000)]
faucet address: addr_test1vz43lpfj5fa32a48lgxqnsxzay0pc4tc5zkdnh0h5rn3vts8vxxl8
"/home/sgillespie/dev/iog/cardano-faucet/cardano-faucet/data/index.html"
UtxoStats (fromList [(Ada (Coin 10000200000),1000)])
utxo set initialized
0 stake keys not registered, 51 stake keys registered and ready for use, 0 stake keys delegated to pools
```

Check the output to make sure that Faucet is fully funded. It should look similar to the
output above.

The faucet web interface can be accessed at [http://localhost:8090/basic-faucet].
Alternatively, `curl` can be used, for example:

```
curl -v -XPOST "http://localhost:8090/send-money?address=$SEND_ADDRESS&api_key=$API_KEY"
```

## Test Faucet

Generate a recipient address:

```
cardano-cli latest address key-gen \
    --verification-key-file testnet-faucet/recipient.vkey \
    --signing-key-file testnet-faucet/recipient.skey
cardano-cli latest address build \
    --payment-verification-key-file testnet-faucet/recipient.vkey \
    --testnet-magic 42 \
    --out-file testnet-faucet/recipient.addr
```

Open the [faucet web interface](http://localhost:8090/basic-faucet). To send funds, use:

 * `address(required)`: Contents of `testnet-faucet/reciepient.addr`
 * `api key(optional)`: Contents of `testnet-faucet/api.key`

Verify funds were transferred:

```
cardano-cli latest query utxo \
    --address "$(<testnet-faucet/recipient.addr)" \
    --socket-path ./testnet/socket/node1/sock \
    --testnet-magic 42
```

