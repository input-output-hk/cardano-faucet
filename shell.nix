with { pkgs = import ./nix { }; };
pkgs.mkShell {
  buildInputs = with pkgs; [
    cacert
    cardano-wallet-byron
    crystal
    crystal2nix
    expect
    niv
    jq
    shards
    (lowPrio sqlite)
    sqliteInteractive
  ];
}
