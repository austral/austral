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
    pyright
    black
    isort
  ];
}
