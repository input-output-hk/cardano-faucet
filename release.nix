{ cardano-faucet ? { outPath = ./.; rev = "abcdef"; } }:
let
  pkgs = import ./nix/default.nix {};
in pkgs.lib.fix (self: {
  inherit (pkgs) crystal;
  inherit (pkgs.packages) cardano-faucet;
  forceNewEval = pkgs.writeText "forceNewEval" cardano-faucet.rev;
  required = pkgs.releaseTools.aggregate {
    name = "required";
    constituents = with self; [
      crystal
      cardano-faucet
      forceNewEval
    ];
  };
})
