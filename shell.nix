with { pkgs = import ./nix { }; };
pkgs.mkShell {
  buildInputs = with pkgs; [
    cardano-wallet-byron
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
