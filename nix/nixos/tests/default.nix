{ pkgs, supportedSystems ? [ "x86_64-linux" ] }:

let
  inherit (pkgs) lib;
  inherit (lib) genAttrs hydraJob;

  forAllSystems = genAttrs supportedSystems;

  importTest = fn: args: system:
    let
      imported = import fn;
      test = import (pkgs.path + "/nixos/tests/make-test.nix") imported;
    in test ({ inherit system; } // args);

  callTest = fn: args:
    forAllSystems (system: hydraJob (importTest fn args system));
in rec {
  simple = callTest ./simple.nix { };
}
