{pkgs ? import <nixpkgs> {}}:
pkgs.mkShellNoCC {
  packages = with pkgs; [
    rustc
    cargo
    rustfmt
    clippy
    llvm_18
  ];
  LLVM_SYS_18_PREFIX= "${pkgs.llvm_18.dev}";
}