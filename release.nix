{
  system ? builtins.currentSystem,
  nixpkgs ?  builtins.fetchTarball {
    url = "https://releases.nixos.org/nixos/23.11/nixos-23.11.7870.205fd4226592/nixexprs.tar.xz";
    sha256 = "sha256-h3DunxVF01tidoonuEFjA6Ie8HvYITV42g/337w8d9U=";
  },
}:

let
  pkgs = import nixpkgs {
    inherit system;
    overlays = [ 
      (import ./nix/overlay)
      (import ./nix/overlay/development.nix)
    ];
  };
in
{
  inherit (pkgs.austral)
    default
    dev-shell
    austral
    vim-plugin;
}
