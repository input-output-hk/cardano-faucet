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
        The environment name to configure the faucet for.
        This name can be an environment name from those found at
        iohk-nix/cardano-lib/default.nix, or, it can be a custom
        name, for example, if launching a test cluster that does
        not have an environment yet defined in iohk-nix.  If custom,
        the cardanoEnvAttrs option must contain an attribute set
        of the same form as the cardano-lib environments found
        in iohk-nix.

        NOTE: If the environment name has `selfnode` in it,
        the cardanoEnvAttrs option will be expected to have
        the following addtional attrs compared to a non-selfnode
        environment:
          topology, signingKey, delegationCertificate
      '';
    };

    cardanoEnvAttrs = mkOption {
      type = types.attrs;
      default = iohkNix.cardanoLib.environments."${cfg.cardanoEnv}";
      description = ''
        The environment attribute set to configure the faucet for.
        This must be an attribute set from an environment found at
        iohk-nix/cardano-lib/default.nix, or, it must be an attribute set
        of the same form.

        NOTE: If the cardanoEnv option has `selfnode` in it,
        this cardanoEnvAttrs option will be expected to have
        the following addtional attrs compared to a non-selfnode
        environments:
          topology, signingKey, delegationCertificate
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

    faucetListenAddress = mkOption {
      type = types.str;
      default = "127.0.0.1";
      description = "The default IP address cardano faucet will listen on.";
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
      default = 10000000000;
      description = ''
        The default quantity of lovelaces to send per faucet transaction for an API key authenticated request.
      '';
    };

    package = mkOption {
      type = types.package;
      default = (import ../. {}).packages.cardano-faucet;
      defaultText = "cardano-faucet";
      description = "The cardano-faucet package to be used";
    };

    rateLimitOnSuccess = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Whether to apply a rate limiting timestamp on confirmation of a successful
        send-money transaction, or at the point of receiving the send-money request.
        Applying a rate limiting timestamp on success will ensure users submitting
        improper requests won't have to wait another potentially extended period of
        time prior to fixing their mistake and trying again.  Setting this parameter
        to false will apply a rate limiting timestamp upon receiving the request so
        that even if the request fails, the rate limit is in effect.  This would help
        protect against errant scripts, [D]DoS load, exploitation probes, etc.
      '';
    };

    useByronWallet = mkOption {
      type = types.bool;
      default = cfg.cardanoEnvAttrs.useByronWallet;
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

    secondsBetweenRequestsAnonymous = mkOption {
      type = types.int;
      default = 24 * 60 * 60;
      description = "The default seconds per tx (rate limit) an anonymous user is allowed to make on the faucet.";
    };

    secondsBetweenRequestsApiKeyAuth = mkOption {
      type = types.int;
      default = 0;
      description = "The default seconds per tx (rate limit) an authenticated user is allowed to make on the faucet.";
    };
  };
}
