{
  system ? builtins.currentSystem,
}:

(import ./release.nix { inherit system; }).dev-shell
