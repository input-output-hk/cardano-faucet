{ sources ? import ./sources.nix }:
let
  crystalPkgs = import sources.nixpkgs-crystal {};
in with {
  overlay = self: super: {
    inherit (import sources.niv { }) niv;
    inherit (crystalPkgs) crystal2nix expect jq pkg-config openssl shards;
    crystal = crystalPkgs.crystal_0_34;
    inherit (import sources.cardano-wallet { gitrev = sources.cardano-wallet.rev; })
      cardano-wallet cardano-cli cardano-node;
    packages = self.callPackages ./packages.nix { };
    inherit (import sources.gitignore { inherit (self) lib; }) gitignoreSource;
    iohkNix = import sources.iohk-nix {
      application = "cardano-faucet";
      nixpkgsOverride = self;
    };
  };
};
import sources.nixpkgs {
  overlays = [ overlay ];
  config = { };
}
