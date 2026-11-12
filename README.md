# Cardano Faucet

> A testnet faucet for Cardano networks: provides test ada and optionally delegates it
> to a stake pool.

Cardano Faucet connects to a local `cardano-node`, serves a small webpage and Rest API,
and pays funds from a pre-funded wallet. Access is protected by API keys or Google
reCAPTCHA, with fixed payouts and rate limits.

## Prerequisites

All build and runtime tools are provided by the Nix development shell:

```
nix develop

# Alternatively, if using direnv
direnv allow
```

Without Nix you will need GHC, `cabal`, and a
[cardano-node](https://github.com/IntersectMBO/cardano-node/releases/latest) binary.

The Faucet also requires a running Cardano node and a funded wallet.
[Testing Locally](docs/local-testing.md) walks through both using an ephemeral local
testnet.

## Getting Started

Build:

```
cabal build all
```

The faucet reads its settings from environment variables:

| Variable                   | Purpose                               |
| -------------------------- | ------------------------------------- |
| `CONFIG_FILE`              | Path to the faucet configuration JSON |
| `CARDANO_NODE_SOCKET_PATH` | Node socket to connect to             |
| `PORT`                     | HTTP listen port (default `8090`)     |

See [Testing Locally](docs/local-testing.md) for a complete configuration file and the
steps to fund the wallet it refers to.

Run:

```
cabal run cardano-faucet:exe:cardano-faucet
```

For deploying and operating a faucet in a public test network such as preview or preprod,
see the [Faucet Setup Guide](https://github.com/input-output-hk/cardano-playground/blob/main/docs/explain/faucet-setup.md)
in `cardano-playground`.

## Usage

| Endpoint        | Purpose                               |
| --------------- | ------------------------------------- |
| `/send-money`   | Send test ada to an address           |
| `/delegate`     | Delegate a faucet stake key to a pool |
| `/basic-faucet` | Minimal webpage for requesting funds  |
| `/metrics`      | Prometheus metrics                    |

The web form is served at
[http://localhost:8090/basic-faucet](http://localhost:8090/basic-faucet).

To send test ada:

```
curl -XPOST "http://localhost:8090/send-money/$ADDRESS?api_key=$API_KEY"
```

To delegate to a stake pool:

```
curl "http://localhost:8090/delegate/$POOL_ID?api_key=$API_KEY"
```

## License

Apache-2.0. See [LICENSE](LICENSE) and [NOTICE](NOTICE).
