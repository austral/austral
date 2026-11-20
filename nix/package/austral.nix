{
  lib,
  stdenv,
  gmp,
  python311,
  ocamlPackages,
}:

stdenv.mkDerivation {
  pname = "austral";
  version = "0.2.0";
  __structuredAttrs = true;

  src =
    let
      fs = lib.fileset;
    in
    fs.toSource {
      root = ../..;
      fileset = fs.unions [
        ../../Makefile
        ../../LICENSE
        ../../austral.opam
        ../../concat_builtins.py
        ../../dune-project
        ../../bin
        ../../docs
        ../../lib
        ../../test-programs
      ];
    };

  strictDeps = true;

  nativeBuildInputs = [
    # General
    gmp
    python311

    # Tooling
    ocamlPackages.ocaml
    ocamlPackages.dune_3
    ocamlPackages.findlib
    ocamlPackages.odoc

    # OCaml libraries
    ocamlPackages.menhir
  ];

  buildInputs = [
    # OCaml libraries
    ocamlPackages.yojson
    ocamlPackages.ppx_deriving
    ocamlPackages.ounit2
    ocamlPackages.sexplib
    ocamlPackages.ppx_sexp_conv
    ocamlPackages.zarith
  ];

  doCheck = true;

  installFlags = [ "PREFIX=$(out)" ];

  meta = {
    license = lib.licenses.asl20;
  };
}
