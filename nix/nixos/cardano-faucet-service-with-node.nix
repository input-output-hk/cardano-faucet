let
  sources = import ../sources.nix;
in {
  imports = [
    ./cardano-faucet-service.nix
    (sources.cardano-node + "/nix/nixos")
  ];
}
