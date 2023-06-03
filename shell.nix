{ nixpkgs ? import <nixpkgs> { }}:

let
  pinnedPkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/23.05.tar.gz";
    sha256 = "10wn0l08j9lgqcw8177nh2ljrnxdrpri7bp0g7nvrsn9rkawvlbf";
  };
  pkgs = import pinnedPkgs {};
in

pkgs.mkShell {
  buildInputs = with pkgs; [
    # Libraries
    gmp

    # Tooling
    ocamlPackages.ocaml
    ocamlPackages.dune_3
    ocamlPackages.findlib
    ocamlPackages.odoc

    # OCaml libraries
    ocamlPackages.yojson
    ocamlPackages.ppx_deriving
    ocamlPackages.ounit2
    ocamlPackages.menhir
    ocamlPackages.sexplib
    ocamlPackages.ppx_sexp_conv
    ocamlPackages.zarith
  ];
}
