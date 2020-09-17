with { pkgs = import ./nix { }; };
pkgs.mkShell {
  buildInputs = with pkgs; [
    cardano-cli
    cardano-wallet
    cardano-wallet-jormungandr
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
