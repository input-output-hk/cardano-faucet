{ sources ? import ./sources.nix }:
with {
  overlay = self: super: {
    inherit (import sources.niv { }) niv;
    inherit (import sources.nixpkgs-crystal {}) crystal;
    inherit (import sources.cardano-wallet { gitrev = sources.cardano-wallet.rev; }) cardano-wallet-byron;
    packages = self.callPackages ./packages.nix { };
    inherit (import sources.gitignore {}) gitignoreSource;
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
