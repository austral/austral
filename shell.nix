{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  buildInputs = [
    pkgs.gnumake
    pkgs.opam
    pkgs.gmp
  ];
}
