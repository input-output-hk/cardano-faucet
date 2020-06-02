with { pkgs = import ./nix { }; };
pkgs.mkShell {
  buildInputs = with pkgs; [
    cardano-cli
    cardano-wallet-byron
    cardano-wallet-jormungandr
    cardano-wallet-shelley
    crystal
    crystal2nix
    expect
    niv
    jq
    shards
    pkg-config
    openssl
    (lowPrio sqlite)
    sqliteInteractive
  ];
}
