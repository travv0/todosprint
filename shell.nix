{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = [
    pkgs.stack
  ];
  shellHook =
    ''
      stack build yesod-bin --install-ghc
    '';
  }
