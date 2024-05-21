{
  description = "A statically typed functional programming language inspired by ML";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = {
    self,
    nixpkgs,
  }: let
    pkgs = nixpkgs.legacyPackages.x86_64-linux;
  in {
    devShells.x86_64-linux.default = pkgs.callPackage ./shell.nix {};
    packages.x86_64-linux.default = pkgs.callPackage ./default.nix {};
    packages.x86_64-linux.mloxide = pkgs.callPackage ./default.nix {};
  };
}
