{ config, lib, pkgs, ... }:

let
  sources = import ../sources.nix;
  cardano-faucet = (import ../. { }).packages.cardano-faucet;
  iohkNix = import sources.iohk-nix {};
  cfg = config.services.cardano-faucet;
  inherit (lib) mkOption types;
in {
  options.services.cardano-faucet = {
    anonymousAccess = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Whether to allow anonymous access to the faucet, or force the use of an API key.
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

    faucetApiKeyPath = mkOption {
      type = types.str;
      default = "/var/lib/keys/faucet.apikey";
      description = "The default path to the faucet api key file.";
    };

    faucetBasePath = mkOption {
      type = types.str;
      default = "/var/lib/cardano-faucet";
      description = "The default path to the cardano faucet installation.";
    };

    faucetListenPort = mkOption {
      type = types.port;
      default = 8091;
      description = "The default port cardano faucet will listen on.";
    };

    faucetLogLevel = mkOption {
      type = types.enum [ "DEBUG" "VERBOSE" "INFO" "WARNING" "ERROR" "FATAL" "NONE" ];
      default = "INFO";
      description = ''
        The log level for the cardano faucet service. Valid levels are:
        DEBUG VERBOSE INFO WARNING ERROR FATAL NONE
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

    lovelacesToGiveAnonymous = mkOption {
      type = types.int;
      default = 1000000000;
      description = ''
        The default quantity of lovelaces to send per faucet transaction for an anonymous request.
      '';
    };

    lovelacesToGiveApiKeyAuth = mkOption {
      type = types.int;
      default = 1000000000;
      description = ''
        The default quantity of lovelaces to send per faucet transaction for an API key authenticated request.
      '';
    };

    package = mkOption {
      type = types.package;
      default = (import ../. {}).packages.cardano-faucet;
      defaultText = "cardano-faucet";
      description = ''
        The cardano-faucet package to be used
      '';
    };

    useByronWallet = mkOption {
      type = types.bool;
      default = iohkNix.cardanoLib.environments."${cfg.cardanoEnv}".useByronWallet;
      description = "Whether to use a Byron wallet or a Shelley wallet for faucet funding operations/APIs.";
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
      default = if cfg.useByronWallet then
        (import ../. {}).pkgs.cardano-wallet-byron
      else
        (import ../. {}).pkgs.cardano-wallet-shelley;
      description = "Package for the cardano wallet executable.";
    };

    secondsBetweenRequests = mkOption {
      type = types.int;
      default = 24 * 60 * 60;
      description = "The default seconds per transactions a user is allowed to make on the faucet.";
    };
  };
}
