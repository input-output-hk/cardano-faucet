{inputs, ...}: {
  perSystem = {
    lib,
    pkgs,
    system,
    ...
  }: {
    # Uses cardano-parts/capkgs, which are x86_64-linux only
    packages = lib.optionalAttrs (system == "x86_64-linux") {
      # Create a package that combines everything needed to set up and run a local testnet
      cardano-testnet-env = let
        inherit
          (inputs.cardano-parts.packages.${system})
          cardano-address
          cardano-cli
          cardano-node
          cardano-testnet
          ;

        # Extract the python script `<name>.py` from the cardano-playground source,
        # and wrap it as a python application
        mkPythonScript = name:
          pkgs.writeShellApplication {
            inherit name;

            runtimeInputs = [
              (pkgs.python3.withPackages (ps: with ps; [docopt cbor2]))
              cardano-cli
              cardano-address
            ];

            text = ''
              exec python3 ${inputs.cardano-playground}/scripts/${name}.py "$@"
            '';
          };

        # Pull the faucet setup scripts from cardano-playground and add them to `$PATH`
        scripts = map mkPythonScript [
          "distribute"
          "setup-delegation-accounts"
          "restore-delegation-accounts"
        ];
      in
        pkgs.symlinkJoin {
          name = "cardano-testnet-env";
          paths =
            [
              cardano-address
              cardano-cli
              cardano-node
              cardano-testnet
            ]
            ++ scripts;
        };
    };
  };
}
