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
  imports = [
    ./cardano-faucet-options.nix
  ];

  options.services.cardano-faucet.enable = mkOption {
    type = types.bool;
    default = false;
    description = ''
      Enable the Cardano Faucet, an HTTP service that talks to Cardano wallet to spread ADA love.
    '';
  };

  config = mkIf cfg.enable {

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
        FAUCET_WALLET_ID_PATH = "${cfg.faucetBasePath}/faucet.id";
        LOVELACES_TO_GIVE_ANON = toString cfg.lovelacesToGiveAnonymous;
        LOVELACES_TO_GIVE_APIKEY = toString cfg.lovelacesToGiveApiKeyAuth;
        RATE_LIMIT_ON_SUCCESS = if cfg.rateLimitOnSuccess then "TRUE" else "FALSE";
        SECS_BETWEEN_REQS_ANON = toString cfg.secondsBetweenRequestsAnonymous;
        SECS_BETWEEN_REQS_APIKEY = toString cfg.secondsBetweenRequestsApiKeyAuth;
        USE_BYRON_WALLET = if cfg.useByronWallet then "TRUE" else "FALSE";
        WALLET_API = cfg.walletApi;
        WALLET_LISTEN_PORT = toString cfg.walletListenPort;
      };

      preStart = ''
        mkdir -p ${cfg.faucetBasePath}
        cd ${cfg.faucetBasePath}
        if ! [ -s faucet.mnemonic ]; then
          cp ${cfg.faucetSecretMnemonicPath} faucet.mnemonic
          chmod 0400 faucet.mnemonic
        fi
        if ! [ -s faucet.passphrase ]; then
          cp ${cfg.faucetSecretPassphrasePath} faucet.passphrase
          chmod 0400 faucet.passphrase
        fi
        if ! [ -s faucet.apikey ]; then
          cp ${cfg.faucetApiKeyPath} faucet.apikey
          chmod 0400 faucet.apikey
        fi
        if ! [ -s faucet.id ]; then
          ${defaultPackages.create-faucet-wallet} \
            -e ${if cfg.useByronWallet then "byron" else "shelley"} \
            -m ./faucet.mnemonic \
            -p ./faucet.passphrase
        fi
      '';

      path = [ defaultPkgs.cardano-wallet-byron defaultPkgs.cardano-wallet-shelley ];
    };
  };
}
