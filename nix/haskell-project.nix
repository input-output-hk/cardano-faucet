{inputs, ...}: {
  imports = [
    {
      perSystem = {system, ...}: {
        # Inject haskell.nix overlays
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          inherit (inputs.haskell-nix) config;

          overlays = builtins.attrValues inputs.iohk-nix.overlays ++ [inputs.haskell-nix.overlay];
        };
      };
    }
  ];

  perSystem = {
    lib,
    pkgs,
    ...
  }: let
    # We use cabalProject' to ensure we don't build the plan for
    # all systems.
    cabalProject = pkgs.haskell-nix.cabalProject' {
      src = ../.;
      name = "cardano-faucet";
      compiler-nix-name = lib.mkDefault "ghc96";

      # CHaP input map, so we can find CHaP packages (needs to be more
      # recent than the index-state we set!). Can be updated with
      #
      #  nix flake lock --update-input chap
      #
      inputMap = {
        "https://chap.intersectmbo.org/" = inputs.chap;
      };

      shell = {
        # Tools we want from hackage
        tools = {
          cabal = "3.12.1.0";
          fourmolu = "0.14.0.0";
          ghcid = "0.8.9";
          haskell-language-server.src = pkgs.haskell-nix.sources."hls-2.12";
          hlint = "3.8";
        };

        # and from nixpkgs or other inputs
        nativeBuildInputs = with pkgs; [
          alejandra # nix formatter
          statix # nix static analysis
          deadnix # nix dead-code-detector

          # Required to ledger/consensus
          liburing
          lmdb

          # Other useful tools
          gh
          jq
          yq-go
        ];

        # disable Hoogle until someone request it
        # Disabled: GHC 9.6.7 haddock panics on ouroboros-network-1.1.0.0 BigLedgerPeers
        withHoogle = false;
      };

      # package customizations as needed. Where cabal.project is not
      # specific enough, or doesn't allow setting these.
      modules = [
        ({pkgs, ...}: {
          packages.cardano-faucet.configureFlags = ["--ghc-option=-Werror"];
        })
      ];
    };

    # ... and construct a flake from the cabal project
    flake = cabalProject.flake {};

    # add a required job that's basically all hydraJobs.
    hydraJobs =
      flake.hydraJobs
      // pkgs.callPackages inputs.iohk-nix.utils.ciJobsAggregates
      {
        ciJobs =
          flake.hydraJobs
          // {
            # This ensure hydra send a status for the required job (even if no change other than commit hash)
            revision = pkgs.writeText "revision" (inputs.self.rev or "dirty");
          };
      };
  in {
    inherit (flake) checks apps;
    inherit hydraJobs;

    packages =
      flake.packages
      // {
        project = cabalProject;
      };

    devShells =
      flake.devShells
      // {
        # `nix develop .#profiling` (or `.#ghc927.profiling): a shell with profiling enabled
        profiling = (cabalProject.appendModule {modules = [{enableLibraryProfiling = true;}];}).shell;
      };

    legacyPackages = {
      inherit cabalProject pkgs;
      # also provide hydraJobs through legacyPackages to allow building without system prefix
      inherit hydraJobs;
      # expose cardano-faucet binary at top-level
      cardano-faucet = cabalProject.hsPkgs.cardano-faucet.components.exes.cardano-faucet;
    };
  };
}
