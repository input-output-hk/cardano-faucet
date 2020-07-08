{ config, lib, pkgs, ... }:
let
  cfg = config.services.cardano-faucet;
  inherit (lib) hasInfix mkForce mkIf mkOption reverseList splitString types;
  inherit (builtins) head;
  sources = import ../sources.nix;
  iohkNix = import sources.iohk-nix {};
  defaultPackages = (import ../. {}).packages;
  defaultPkgs = (import ../. {}).pkgs;
in {
  options.services.cardano-faucet = {

    enable = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Enable the Cardano Faucet, an HTTP service that talks to Cardano wallet to spread ADA love.
      '';
    };

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

    faucetSecretRecaptchaPath = mkOption {
      type = types.str;
      default = "/var/lib/keys/faucet.recaptcha";
      description = ''
        The default path to the faucet recaptcha file.  This file
        contains the secret recaptcha key that is associated with the
        site key being used by a recaptcha implemented on a front-end.
        For details, see:
        https://developers.google.com/recaptcha/docs/display
      '';
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
      default = defaultPackages.cardano-faucet;
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

    useRecaptchaOnAnon = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Whether to expect a recaptcha post parameter for anonymous requests.
        If true, the expected POST parameter which will be validated by the
        backend is: g-recaptcha-response
      '';
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
        defaultPkgs.cardano-wallet-byron
      else
        defaultPkgs.cardano-wallet-shelley;
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

  config = mkIf cfg.enable {

    environment.systemPackages =
      let completions = pkgs.runCommand "cardano-cli-completions" {} ''
        mkdir -p $out/etc/bash_completion.d
        ${defaultPkgs.cardano-cli}/bin/cardano-cli --bash-completion-script cardano-cli > $out/etc/bash_completion.d/cardano-cli
      '';
      in [
        completions
        defaultPkgs.cardano-cli
        defaultPkgs.cardano-wallet-byron
        defaultPkgs.cardano-wallet-shelley
        pkgs.jq
      ];

    security.sudo.extraConfig = ''
      cardano-node ALL = NOPASSWD: /run/current-system/sw/bin/systemctl restart cardano-wallet.service
    '';

    environment.variables = {
      CARDANO_NODE_SOCKET_PATH = config.services.cardano-node.socketPath;
    };

    services.cardano-node = {
      enable = true;
      extraArgs = [ "+RTS" "-N2" "-A10m" "-qg" "-qb" "-M3G" "-RTS" ];
      environments = { "${cfg.cardanoEnv}" = cfg.cardanoEnvAttrs; };
      environment = cfg.cardanoEnv;

      # Enable selfnode and utilize DNS relay for other envs
      topology = if (hasInfix "selfnode" cfg.cardanoEnv)
        then
          cfg.cardanoEnvAttrs.topology
        else iohkNix.cardanoLib.mkEdgeTopology {
          edgeHost = cfg.cardanoEnvAttrs.relaysNew;
          edgeNodes = [];
        };
      signingKey = if (hasInfix "selfnode" cfg.cardanoEnv)
        then cfg.cardanoEnvAttrs.signingKey
        else null;
      delegationCertificate = if (hasInfix "selfnode" cfg.cardanoEnv)
        then cfg.cardanoEnvAttrs.delegationCertificate
        else null;
    };

    systemd.services.cardano-node = {
      startLimitIntervalSec = 0;
      serviceConfig = {
        Restart = mkForce "always";
        RestartSec = mkForce 10;
      };
    };

    systemd.services.cardano-wallet = {
      description = "cardano-wallet daemon";
      after = [ "cardano-node.service"];
      wantedBy = [ "multi-user.target" ];
      startLimitIntervalSec = 0;

      script = ''
        ${cfg.walletPackage}/bin/cardano-wallet-${if cfg.useByronWallet then "byron" else "shelley"} serve \
          --node-socket ${config.services.cardano-node.socketPath} \
          ${if (cfg.cardanoEnv == "mainnet") then "--mainnet" else "--testnet"} \
            ${if (hasInfix "selfnode" cfg.cardanoEnv)
                then "${config.services.cardano-node.stateDir}/genesis.json"
                else config.services.cardano-node.genesisFile} \
          --database /var/lib/cardano-wallet/${cfg.cardanoEnv};
      '';

      serviceConfig = {
        Restart = "always";
        RestartSec = 10;
        User = "cardano-node";
        StateDirectory = "cardano-wallet";
        RuntimeDirectory = "cardano-wallet";
        WorkingDirectory = "/var/lib/cardano-wallet";
      };

      preStart = ''
        mkdir -p /var/lib/cardano-wallet/${cfg.cardanoEnv}/
      '';
    };

    systemd.services.cardano-faucet = {
      description = "cardano-faucet daemon";
      after = [ "cardano-wallet.service"];
      wantedBy = [ "multi-user.target" ];
      startLimitIntervalSec = 0;

      script = ''
        ${cfg.package}/bin/cardano-faucet
      '';

      serviceConfig = {
        Restart = "always";
        RestartSec = 10;
        User = "cardano-node";
        StateDirectory = head (reverseList (splitString "/" cfg.faucetBasePath));
        RuntimeDirectory =  head (reverseList (splitString "/" cfg.faucetBasePath));
        WorkingDirectory = cfg.faucetBasePath;
      };

      environment = {
        ANONYMOUS_ACCESS = if cfg.anonymousAccess then "TRUE" else "FALSE";
        CARDANO_ENV = cfg.cardanoEnv;
        CRYSTAL_LOG_LEVEL = cfg.faucetLogLevel;
        CRYSTAL_LOG_SOURCES = "*";
        FAUCET_API_KEY_PATH = "${cfg.faucetBasePath}/faucet.apikey";
        FAUCET_LISTEN_ADDRESS = cfg.faucetListenAddress;
        FAUCET_LISTEN_PORT = toString cfg.faucetListenPort;
        FAUCET_SECRET_MNEMONIC_PATH = "${cfg.faucetBasePath}/faucet.mnemonic";
        FAUCET_SECRET_PASSPHRASE_PATH = "${cfg.faucetBasePath}/faucet.passphrase";
        FAUCET_SECRET_RECAPTCHA_PATH = "${cfg.faucetBasePath}/faucet.recaptcha";
        FAUCET_WALLET_ID_PATH = "${cfg.faucetBasePath}/faucet.id";
        LOVELACES_TO_GIVE_ANON = toString cfg.lovelacesToGiveAnonymous;
        LOVELACES_TO_GIVE_APIKEY = toString cfg.lovelacesToGiveApiKeyAuth;
        RATE_LIMIT_ON_SUCCESS = if cfg.rateLimitOnSuccess then "TRUE" else "FALSE";
        SECS_BETWEEN_REQS_ANON = toString cfg.secondsBetweenRequestsAnonymous;
        SECS_BETWEEN_REQS_APIKEY = toString cfg.secondsBetweenRequestsApiKeyAuth;
        USE_BYRON_WALLET = if cfg.useByronWallet then "TRUE" else "FALSE";
        USE_RECAPTCHA_ON_ANON = if cfg.useRecaptchaOnAnon then "TRUE" else "FALSE";
        WALLET_API = cfg.walletApi;
        WALLET_LISTEN_PORT = toString cfg.walletListenPort;
      };

      preStart = ''
        mkdir -p ${cfg.faucetBasePath}
        cd ${cfg.faucetBasePath}
        # For all files but the api key file, copy the files
        # into place only if they do not already exist.
        # The reason is that new faucet wallets will
        # require removing an old wallet manually first if
        # the new wallet name will remain the same.  It will also
        # help prevent accidental overwrites.  The api key file
        # is updated more routinely and is the exception.

        # Automatically copy api key updates into place
        if [ -s faucet.apikey ]; then
          chmod 0600 faucet.apikey
        fi
        cp ${cfg.faucetApiKeyPath} faucet.apikey
        chmod 0400 faucet.apikey

        if ! [ -s faucet.mnemonic ]; then
          cp ${cfg.faucetSecretMnemonicPath} faucet.mnemonic
          chmod 0400 faucet.mnemonic
        fi

        if ! [ -s faucet.passphrase ]; then
          cp ${cfg.faucetSecretPassphrasePath} faucet.passphrase
          chmod 0400 faucet.passphrase
        fi

        if ! [ -s faucet.recaptcha ]; then
          cp ${cfg.faucetSecretRecaptchaPath} faucet.recaptcha
          chmod 0400 faucet.recaptcha
        fi

        if ! [ -s faucet.id ]; then
          ${defaultPackages.create-faucet-wallet} \
            -e ${if cfg.useByronWallet then "byron" else "shelley"} \
            -m ./faucet.mnemonic \
            -p ./faucet.passphrase
        fi
      '';

      path = [
        defaultPkgs.cardano-wallet-byron
        defaultPkgs.cardano-wallet-shelley
        pkgs.sudo
      ];
    };
  };
}
