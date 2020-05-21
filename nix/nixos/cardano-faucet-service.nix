{ config, lib, pkgs, ... }:
let
  cfg = config.services.cardano-faucet;
  inherit (lib) mkOption mkForce types mkIf optionals;
  inherit (builtins) replaceStrings;
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
      environment = cfg.cardanoEnv;

      # Enable selfnode and utilize DNS relay for other envs
      topology = if (cfg.cardanoEnv == "selfnode")
        then
          iohkNix.cardanoLib.environments.selfnode.topology
        else iohkNix.cardanoLib.mkEdgeTopology {
          edgeHost = iohkNix.cardanoLib.environments."${cfg.cardanoEnv}".relaysNew;
          edgeNodes = [];
        };
      signingKey = if (cfg.cardanoEnv == "selfnode")
        then iohkNix.cardanoLib.environments.selfnode.signingKey
        else null;
      delegationCertificate = if (cfg.cardanoEnv == "selfnode")
        then iohkNix.cardanoLib.environments.selfnode.delegationCertificate
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
        ${cfg.walletPackage}/bin/cardano-wallet-byron serve \
          --node-socket ${config.services.cardano-node.socketPath} \
          ${if (cfg.cardanoEnv == "mainnet") then "--mainnet" else "--testnet"} \
            ${if (cfg.cardanoEnv == "selfnode")
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
        StateDirectory = "cardano-faucet";
        RuntimeDirectory = "cardano-faucet";
        WorkingDirectory = "/var/lib/cardano-faucet";
      };

      environment = {
        ANONYMOUS_ACCESS = if cfg.anonymousAccess then "TRUE" else "FALSE";
        CRYSTAL_LOG_LEVEL = cfg.faucetLogLevel;
        CRYSTAL_LOG_SOURCES = "*";
        FAUCET_API_KEY_PATH = "/var/lib/cardano-faucet/faucet.apikey";
        FAUCET_LISTEN_PORT = toString cfg.faucetListenPort;
        FAUCET_SECRET_MNEMONIC_PATH = "/var/lib/cardano-faucet/faucet.mnemonic";
        FAUCET_SECRET_PASSPHRASE_PATH = "/var/lib/cardano-faucet/faucet.passphrase";
        FAUCET_WALLET_ID_PATH = cfg.faucetWalletIdPath;
        LOVELACES_TO_GIVE_ANON = toString cfg.lovelacesToGiveAnonymous;
        LOVELACES_TO_GIVE_APIKEY = toString cfg.lovelacesToGiveApiKeyAuth;
        SECONDS_BETWEEN_REQUESTS = toString cfg.secondsBetweenRequests;
        USE_BYRON_WALLET = if cfg.useByronWallet then "TRUE" else "FALSE";
        WALLET_API = cfg.walletApi;
        WALLET_LISTEN_PORT = toString cfg.walletListenPort;
      };

      preStart = ''
        mkdir -p /var/lib/cardano-faucet
        cd /var/lib/cardano-faucet
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
          ${defaultPackages.create-faucet-wallet} -m ./faucet.mnemonic -p ./faucet.passphrase
        fi
      '';

      path = with pkgs; [ defaultPkgs.cardano-wallet-byron ];
    };
  };
}
