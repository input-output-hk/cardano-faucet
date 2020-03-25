{ pkgs , ... }:

{
  name = "cardano-faucet-test";
  nodes = {
    machine = { config, pkgs, ... }: {
      imports = [
        ../.
      ];
      services = {
        cardano-faucet = {
          enable = true;
          cardanoEnv = "selfnode";
          faucetLogLevel = "DEBUG";
        };
      };
    };
  };
  testScript = ''
    startAll
    $machine->execute("mkdir -p /var/lib/keys");
    $machine->execute("echo SECRET_KEY=key > /var/lib/keys/cardano-faucet.key");
    $machine->waitForUnit("cardano-faucet.service");
    $machine->waitForOpenPort(8091);
  '';
}
