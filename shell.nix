{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/master.tar.gz") {} }:
pkgs.mkShell {
  buildInputs = [
    pkgs.nodejs-14_x
    pkgs.purescript
    pkgs.spago
  ];
}
