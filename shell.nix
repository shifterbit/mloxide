{pkgs ? import <nixpkgs> {}}:
pkgs.mkShell {
  packages = with pkgs; [
    rustc
    cargo
    rustfmt
    clippy
    llvm_18
    libclang
    zlib
    libxml2
  ];
  LLVM_SYS_18_PREFIX= "${pkgs.llvm_18}";
}