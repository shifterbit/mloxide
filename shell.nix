{pkgs ? import <nixpkgs> {}}:
pkgs.mkShellNoCC {
  packages = with pkgs; [
    rustc
    cargo
    rustfmt
  ];
}
