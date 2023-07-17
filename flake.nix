{
  description = "Systems language with linear types and capability-based security.";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
  inputs.utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, utils }: 
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
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

      in {
        packages.default = pkgs.stdenv.mkDerivation {
          pname = "austral";
          version = "0.2.0";
          src = ./.;
          inherit buildInputs;
          buildPhase = ''
            make
          '';

          installPhase = ''
            mkdir -p $out/bin
            install -m 755 austral $out/bin/austral
          '';
        };

        devShells.default = pkgs.mkShell {
          inherit buildInputs;
        };
      });
}
