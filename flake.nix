{
  description = "Systems language with linear types and capability-based security.";

  inputs.nixpkgs.url = "https://channels.nixos.org/nixos-23.11/nixexprs.tar.xz";

  outputs = { self, nixpkgs }: 
    let
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
        "i686-linux"
        "aarch64-darwin"
      ];

      eachSystem =
        systems: f:
        builtins.foldl' (
          a: s: a // builtins.mapAttrs (k: v: (a.${k} or { }) // { ${s} = v; }) (f s)
        ) { } systems;
    in
    {
       overlays = {
         default = import ./nix/overlay;
         development = import ./nix/overlay/development.nix;
       };
    } 
    // eachSystem supportedSystems (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            self.overlays.default
            self.overlays.development
          ];
        };
      in {
        packages = {
          inherit (pkgs.austral)
            default
            austral
            vim-plugin;
        };

        devShells = {
          default = pkgs.austral.dev-shell;
        };
      });
}
