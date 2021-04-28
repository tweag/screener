{ pkgs ? import (builtins.fetchTarball {
  name = "nixos-20.09-20210428";
  url =
    "https://github.com/nixos/nixpkgs/archive/17b101e29dfff7ae02cdd00e8cde243d2a56472d.tar.gz";
  sha256 = "142lbns0qxl9c6gz035c07v9gpsfd29absqvpd539iz898bdlc48";
}) { } }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    purescript
    spago
    nodePackages.purescript-language-server
    nodePackages.parcel-bundler
    nodePackages.purty
    nodejs
  ];
}
