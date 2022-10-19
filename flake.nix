{
  description = "Faucet for Cardano";
  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    iohk-nix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
    cardano-node = {
      url = "github:jmgilman/cardano-node";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    cardano-wallet = {
      url = "github:input-output-hk/cardano-wallet/v2022-10-06";
      inputs.nixpkgs.follows = "nixpkgs";
    };
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
