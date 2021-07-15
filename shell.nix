{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/4d0ee90c6e253d40920f8dae5edb717a7d6f151d.tar.gz") {} }:
pkgs.mkShell {
  buildInputs = [
    pkgs.nodejs-14_x
  ];
}
