{
  description = "Faucet for Cardano";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/d0f9857448e77df50d1e0b518ba0e835b797532a";
    flake-utils.url = "github:numtide/flake-utils";
    cardano-node.url = "github:jmgilman/cardano-node";
    cardano-wallet.url = "github:input-output-hk/cardano-wallet/v2022-10-06";
    iohk-nix.url = "github:input-output-hk/iohk-nix";
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , cardano-node
    , cardano-wallet
    , iohk-nix
    , ...
    }@inputs: (flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
    in
    {
      devShells = {
        default = pkgs.mkShell {
          buildInputs = with pkgs; [
            cardano-node.packages.${system}.cardano-cli
            #cardano-wallet
            crystal
            crystal2nix
            expect
            niv
            jq
            shards
            pkg-config
            openssl
            (lowPrio sqlite)
            sqlite-interactive
          ];
        };
      };
    }));
}
