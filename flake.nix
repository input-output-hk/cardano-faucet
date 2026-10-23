{
  description = "Faucet service providing test ada on Cardano testnets";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-utils.url = "github:hamishmack/flake-utils/hkm/nested-hydraJobs";
    incl.url = "github:divnix/incl";
    treefmt.url = "github:numtide/treefmt-nix";
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";

    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.hackage.follows = "hackage-nix";
    };
    hackage-nix = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
    iohk-nix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    chap = {
      url = "github:IntersectMBO/cardano-haskell-packages?ref=repo";
      flake = false;
    };
  };

  outputs = {
    flake-parts,
    treefmt,
    ...
  } @ inputs:
    flake-parts.lib.mkFlake {inherit inputs;} ({...}: {
      imports = [
        treefmt.flakeModule

        # Add hydraJobs to perSystem
        ./nix/hydra-jobs.nix
        # Project builds via haskell.nix
        ./nix/haskell-project.nix
        # Source code formatters via treefmt
        ./nix/formatter.nix
      ];

      systems = [
        "x86_64-linux"
        # disabling to reduce CI time initially. Uncomment later
        #"x86_64-darwin"
        #"aarch64-linux"
        #"aarch64-darwin"
      ];
    });

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = true;
  };
}
