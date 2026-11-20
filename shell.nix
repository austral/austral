{ nixpkgs ? import <nixpkgs> { }}:

let
  pinnedPkgs = builtins.fetchTarball {
    url = "https://releases.nixos.org/nixos/23.11/nixos-23.11.7870.205fd4226592/nixexprs.tar.xz";
    sha256 = "sha256-h3DunxVF01tidoonuEFjA6Ie8HvYITV42g/337w8d9U=";
  };
  pkgs = import pinnedPkgs {};
in

pkgs.mkShell {
  buildInputs = with pkgs; [
    # General
    gmp
    python311

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
