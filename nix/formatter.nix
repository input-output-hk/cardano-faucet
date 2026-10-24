{
  perSystem = {
    treefmt = {
      projectRootFile = "flake.nix";

      programs = {
        alejandra.enable = true; # Nix

        # TODO[sgillespie]: Uncomment when ready
        # fourmolu.enable = true; # Haskell source files
        # cabal-gild.enable = true; # Cabal
      };
    };
  };
}
