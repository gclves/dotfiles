# For specific package versions and their tarballs, see:
# https://lazamar.co.uk/nix-versions/

{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/2c162d49cd5b979eb66ff1653aecaeaa01690fcc.tar.gz") {} }:

pkgs.mkShell {
  buildInputs = [
    # pkgs.ruby_3_0
  ];
}

