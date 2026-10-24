{
  flake-parts-lib,
  lib,
  ...
}:
# For motivation/inspiration, see:
#  * https://github.com/hercules-ci/flake-parts/issues/376
#  * https://github.com/TeaClientMC/Registry/blob/master/hydraJobs.nix
flake-parts-lib.mkTransposedPerSystemModule {
  name = "hydraJobs";
  file = "./hydra-jobs.nix";
  option = lib.mkOption {
    default = {};
    type = let
      hydraJobType = lib.types.lazyAttrsOf (lib.types.either lib.types.package hydraJobType);
    in
      hydraJobType;
  };
}
