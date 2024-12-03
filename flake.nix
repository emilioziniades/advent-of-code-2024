{
  description = "Guile development environment for Advent of Code 2024";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = {
    self,
    nixpkgs,
  }: let
    forAllSystems = fn:
      nixpkgs.lib.genAttrs
      ["x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin"]
      (system: fn system nixpkgs.legacyPackages.${system});
  in {
    devShells = forAllSystems (system: pkgs: {
      default = pkgs.mkShell {
        buildInputs = with pkgs; [
          guile
          guile-gnutls
          just
          self.packages.${system}.code-formatter
        ];
      };
    });

    packages = forAllSystems (system: pkgs: {
      code-formatter = pkgs.callPackage ./nix/code-formatter.nix {};
    });
  };
}
