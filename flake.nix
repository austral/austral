{
  description = "Systems language with linear types and capability-based security.";

  inputs.nixpkgs.url = "https://channels.nixos.org/nixos-23.11/nixexprs.tar.xz";

  inputs.utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, utils }: 
    {
       overlays = {
         default = import ./nix/overlay;
         development = import ./nix/overlay/development.nix;
       };
    } 
    // utils.lib.eachDefaultSystem (system:
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
