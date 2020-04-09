{ config, lib, pkgs, ... }:

let
  cardano-faucet = (import ../. { }).packages.cardano-faucet;
  cfg = config.services.cardano-faucet;
  inherit (lib) mkOption types;
in {
  options.services.cardano-faucet = {
    package = mkOption {
      type = types.package;
      default = (import ../. {}).packages.cardano-faucet-cr;
      defaultText = "cardano-faucet";
      description = ''
        The cardano-faucet package to be used
      '';
    };

    cardanoEnv = mkOption {
      type = types.str;
      default = "selfnode";
      description = ''
        The environment to configure the faucet for.
        See examples at iohk-nix/cardano-lib/default.nix
      '';
    };

    faucetListenPort = mkOption {
      type = types.port;
      default = 8091;
      description = "The default port cardano faucet will listen on.";
    };

    faucetLogLevel = mkOption {
      type = types.enum [ "UNKNOWN" "DEBUG" "INFO" "WARN" "ERROR" "FATAL" ];
      default = "INFO";
      description = ''
        The log level for the cardano faucet service. Valid levels are:
        UNKNOWN DEBUG INFO WARN ERROR FATAL.
      '';
    };

    faucetWalletIdPath = mkOption {
      type = types.str;
      default = "/var/lib/cardano-faucet/faucet.id";
      description = "The default path to the faucet wallet id file.";
    };

    faucetSecretMnemonicPath = mkOption {
      type = types.str;
      default = "/var/lib/keys/faucet.mnemonic";
      description = "The default path to the faucet secret key file.";
    };

    faucetSecretPassphrasePath = mkOption {
      type = types.str;
      default = "/var/lib/keys/faucet.passphrase";
      description = "The default path to the faucet passphrase file.";
    };

    faucetApiKeyPath = mkOption {
      type = types.str;
      default = "/var/lib/keys/faucet.apikey";
      description = "The default path to the faucet api key file.";
    };

    walletApi = mkOption {
      type = types.str;
      default = "http://localhost:8090/v2";
      description = "The default API URL endpoint cardano wallet is available at.";
    };

    walletListenPort = mkOption {
      type = types.port;
      default = 8090;
      description = "The default port cardano wallet will listen on.";
    };

    walletPackage = mkOption {
      type = types.package;
      default = (import ../. {}).pkgs.cardano-wallet-byron;
      description = "Package for the cardano wallet executable.";
    };

    lovelacesToGive = mkOption {
      type = types.int;
      default = 1000000000;
      description = "The default quantity of lovelaces to send per faucet transaction.";
    };

    secondsBetweenRequests = mkOption {
      type = types.int;
      default = 24 * 60 * 60;
      description = "The default seconds per transactions a user is allowed to make on the faucet.";
    };
  };
}
